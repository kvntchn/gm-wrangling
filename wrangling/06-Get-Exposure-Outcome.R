# Data preparation and analysis with Cox PH ####
# Kevin Chen
# July 29, 2019

get.cohort_analytic <- function(
	cohort_full = NULL,
	cohort_py = NULL,
	exposure.lag = 21,
	deathage.max = NULL,
	outcome_type = "mortality",
	include_alcohol = F,
	year.max = 2015,
	hire.year.min = 1938,
	hire.year.max = Inf,
	use_seer = F) {


	if (is.null(cohort_full)) {
		cohort_full <- data.table::copy(cohort)
	}

	if (is.null(cohort_py)) {
		cohort_py <- get.ltab_obs(cohort_full = data.table::copy(cohort_full),
															include_alcohol = include_alcohol,
															hire.year.min = hire.year.min,
															end.year = year.max,
															outcome_type = outcome_type,
															deathage.max = deathage.max,
															use_seer = use_seer)
	} else {
		cohort_py <- data.table::copy(cohort_py)
	}

	# Exposure data ####
	if (sum(grepl('exposure.names', ls(envir = .GlobalEnv))) == 0) {
		exposure.names <- c('missyr',
												# 'dry',
												'soluble',
												'straight',
												'synthetic',
												'grinding',
												'machining',
												'grinding soluble',
												'machining soluble',
												# 'str.grinding',
												# 'str.machining',
												# 'syn.grinding',
												# 'syn.machining'
												NULL
		)}

	# # Function that takes sum or returns NaN if all NA
	# sum_or_na <- function(x) {
	# 	if (sum(!is.na(x)) < 1) {NaN} else {
	# 		sum(x, na.rm = T)}}

	# Make exposure data person-year specific
	setorder(exposure, studyno, year)
	if (length(table(exposure[,.N, by = .(studyno, year)]$N)) > 1) {
		exposure[,`:=`(I = 1:.N, N = .N), by = .(studyno, year)]
		# for (w in exposure.names) {
		# exposure[, (w) := lapply(w, function(x) {
		# 	sum(get(x))
		# }), by = .(studyno, year)]}
		# exposure <<- exposure[I == 1, -c("I", "N", "plant"), with = F]
		exposure <- exposure[, .(
			missyr = sum(missyr),
			soluble = sum(soluble),
			straight = sum(straight),
			synthetic = sum(synthetic),
			grinding = sum(grinding),
			machining = sum(machining),
			`grinding soluble` = sum(`grinding soluble`),
			`machining soluble` = sum(`machining soluble`)
		), by = .(studyno, year)]
	}

	# Left outer join
	if (class(cohort_py$plant) == "factor") {
		cohort_py$plant <- levels(cohort_py$plant)[as.numeric(cohort_py$plant)]
	}
	exposure_obs <- merge(cohort_py[, .(studyno, year, plant)],
												# Use plant data from main dataset
												exposure,
												by = c('studyno', 'year'),
												all.x = T)

	# Get plants from jobhist
	exposure_obs <- merge(
		exposure_obs,
		jobhist_py.cast[,.(studyno, year,
											 numeric.gan, numeric.han, numeric.san)],
		by = c("studyno", "year"),
		all.x = T
	)

	if (is.factor(exposure_obs$plant)) {
		exposure_obs[, plant := levels(plant)[as.numeric(plant)]]
	} else {
		exposure_obs[, plant := as.character(plant)]
	}

	exposure_obs[plant == 9, `:=`(
		plant = as.character((ifelse(
			!is.na(numeric.gan[1]),
			which.max(c(numeric.gan[1], numeric.han[1], numeric.san[1])),
			NA
		)))), by = .(studyno, year)]

	# Any na plants? Carry forward
	# exposure_obs[, .(noplant = as.numeric(sum(!is.na(plant)) == 0)),
	# 						 by = .(studyno)][noplant == 1]
	# n_distinct(exposure_obs[is.na(plant), studyno])
	exposure_obs[, plant := {
		# No plants are missing plant for the first year on record,
		#  but code is here for filling backward
		if (is.na(plant[1])) {
			plant[1:(which(!is.na(plant))[1])] <-
				zoo::na.locf(plant[1:(which(!is.na(plant))[1])], fromLast = T)
		}
		# Fill forward
		plant <- zoo::na.locf(plant)
		plant
	}, by = .(studyno)]

	# Merge ####
	cohort_analytic <- merge(
		cohort_py[,-'plant'],
		exposure_obs,
		by = c('studyno', 'year'),
		all.x = T)

	cohort_analytic <- merge(
		cohort_analytic[,-c("numeric.gan", "numeric.han", "numeric.san")],
		jobhist_py.cast,
		by = c("studyno", "year"), all.x = T)

	component.names <- list(c("bio", "cl", "ea", "tea", "trz", "s", "no2"))
	component.names <- list(
		component.names[[1]],
		paste0(rep(component.names[[1]], each = 3), c(".gan", ".han", ".san"))
	)

	cohort_analytic[, `:=`(
		bio = bio.gan + bio.han + bio.san,
		cl = cl.gan + cl.han + cl.san,
		ea = ea.gan + ea.han + ea.san,
		tea = tea.gan + tea.han + tea.san,
		trz = trz.gan + trz.han + trz.san,
		s = s.gan + s.han + s.san,
		no2 = no2.gan + no2.han + no2.san
	)]

	cohort_analytic <- cohort_analytic[, -c(
		"bio.gan", "bio.han", "bio.san",
		"cl.gan", "cl.han", "cl.san",
		"ea.gan", "ea.han", "ea.san",
		"tea.gan", "tea.han", "tea.san",
		"trz.gan", "trz.han", "trz.san",
		"s.gan", "s.han", "s.san",
		"no2.gan", "no2.han", "no2.san"
	), with = F]

	to_shift.names <- c(
		exposure.names,
		# component.names[[1]],
		# component.names[[2]]
		NULL)

	# Lag exposure
	if (exposure.lag != 0) {
		cohort_analytic[, (to_shift.names) := lapply(
			to_shift.names, function(x) {shift(get(x), n = exposure.lag, fill = 0)}),
			by = .(studyno)]
	}

	# Also include original plant
	cohort_analytic <- merge(cohort_analytic,
													 cohort_py[,.(PLANT = unique(plant)), by = .(studyno)],
													 by = c('studyno'),
													 all.x = T)

	# # Percent person-years with no exposure information
	# nrow(cohort_analytic[is.na(soluble)]) / nrow(cohort_analytic)

	if (outcome_type == 'mortality') {
		# Factor calendar periods (LTAB-style)
		cohort_analytic[, `:=`(calendar = factor(ltab_calendar(year)))]
	}

	# # Misisng data folks (studyno 133570 is pesky)
	# cohort_analytic[!complete.cases(
	# 	cohort_analytic[,.(
	# 		straight, soluble, synthetic
	# 	)]) &
	# 		# year < year(yout)  &
	# 		wh == 1 & nohist == 0, c(
	# 			"studyno", "year", "yout",
	# 			"straight",
	# 			"soluble",
	# 			"synthetic",
	# 			component.names[[1]]), with = F]

	cohort_analytic[!complete.cases(
		cohort_analytic[,.(
			straight, soluble, synthetic
		)]) &
			# year < year(yout)  &
			wh == 1 & nohist == 0, `:=`(
				straight = 0,
				soluble = 0,
				synthetic = 0)]

	# Cumulative exposure
	# setorder(cohort_analytic, studyno, year)
	cohort_analytic[order(year),
		(gsub(" ", "_", paste0("cum_", to_shift.names))) := lapply(
			to_shift.names, function(x) {cumsum(get(x))}), by = .(studyno)]

	# Year out of work
	jobloss <- jobhist_py[,.(
		dateout.date = dateout.date[1],
		employment_end.date = employment_end.date[1],
		employment_end.date.legacy = employment_end.date.legacy[1],
		yout_recode = yout_recode[1]),
		by = .(studyno)]

	cohort_analytic <- merge(
		cohort_analytic,
		jobloss,
		by = c('studyno'),
		all.x = T
	)

	# Fill with yout where missing
	cohort_analytic[is.na(dateout.date), dateout.date := yout]
	cohort_analytic[is.na(employment_end.date), employment_end.date := yout]
	cohort_analytic[is.na(employment_end.date.legacy), employment_end.date.legacy := yout]

	cohort_analytic[, exposure.lag := exposure.lag]

	return(cohort_analytic)
}

# Heuristic check
# View(cohort_analytic[
# 	studyno == median(studyno) + 1,
# 	.(studyno, year,
# 		missyr, cum_missyr,
# 		soluble, cum_soluble,
# 		straight, cum_straight,
# 		synthetic, cum_synthetic)])
# rm(list = ls())