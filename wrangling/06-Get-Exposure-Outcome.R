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
	end.year = 2015,
	hire.year.min = 1938,
	hire.year.max = Inf,
	use_seer = F) {
	
	
	if (is.null(cohort_full)) {
		cohort_full <- as.data.table(as.data.frame(cohort))
	}

	if (is.null(cohort_py)) {
		cohort_py <-
			get.ltab_obs(cohort_full = as.data.frame(cohort_full),
									 include_alcohol = include_alcohol,
									 hire.year.min = hire.year.min,
									 end.year = end.year,
									 outcome_type = outcome_type,
									 deathage.max = deathage.max,
									 use_seer = use_seer)
	} else {
		cohort_py <- as.data.table(as.data.frame(cohort_py))
	}

	# Collapse by person-year
	exposure <<- exposure[,.(
		missyr = {if (sum(!is.na(missyr)) < 1) {
			NaN} else {
				sum(missyr, na.rm = T)
			}},
		soluble = {if (sum(!is.na(soluble)) < 1) {
			NaN} else {
				sum(soluble, na.rm = T)
			}},
		straight = {if (sum(!is.na(straight)) < 1) {
			NaN} else {
				sum(straight, na.rm = T)
			}},
		synthetic = {if (sum(!is.na(synthetic)) < 1) {
			NaN} else {
				sum(synthetic, na.rm = T)
			}},
		grinding = {if (sum(!is.na(grinding)) < 1) {
			NaN} else {
				sum(grinding, na.rm = T)
			}},
		machining = {if (sum(!is.na(machining)) < 1) {
			NaN} else {
				sum(machining, na.rm = T)
			}},
		`grinding soluble` = {if (sum(!is.na(`grinding soluble`)) < 1) {
			NaN} else {
				sum(`grinding soluble`, na.rm = T)
			}},
		`machining soluble` = {if (sum(!is.na(`machining soluble`)) < 1) {
			NaN} else {
				sum(`machining soluble`, na.rm = T)
			}}
	), by = .(studyno, year)]

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
	exposure_obs[, .(noplant = as.numeric(sum(!is.na(plant)) == 0)),
							 by = .(studyno)][noplant == 1]
	n_distinct(exposure_obs[is.na(plant), studyno])
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

	# Lag exposure
	if (exposure.lag != 0) {
		exposure_obs[, `:=`(
			missyr = shift(missyr, exposure.lag, fill = 0),
			soluble = shift(soluble, exposure.lag, fill = 0),
			straight = shift(straight, exposure.lag, fill = 0),
			synthetic = shift(synthetic, exposure.lag, fill = 0),
			grinding = shift(grinding, exposure.lag, fill = 0),
			machining = shift(machining, exposure.lag, fill = 0),
			`grinding soluble` = shift(`grinding soluble`, exposure.lag, fill = 0),
			`machining soluble` = shift(`machining soluble`, exposure.lag, fill = 0)
		), by = .(studyno)]
	}

	# Merge ####
	cohort_analytic <-
		merge(cohort_py[,-'plant'],
					exposure_obs,
					by = c('studyno', 'year'),
					all.x = T)

	cohort_analytic <- merge(
		cohort_analytic[,-c("numeric.gan", "numeric.han", "numeric.san")],
		jobhist_py.cast,
		by = c("studyno", "year"), all.x = T)

	# Also include original plant
	cohort_analytic <-
		merge(cohort_analytic,
					cohort_py[,.(PLANT = unique(plant)), by = .(studyno)],
					by = c('studyno'),
					all.x = T)

	# Percent person-years with no exposure information
	nrow(cohort_analytic[is.na(soluble)]) / nrow(cohort_analytic)

	if (outcome_type == 'mortality') {
		# Factor calendar periods (LTAB-style)
		cohort_analytic[, `:=`(calendar = factor(ltab_calendar(year)))]
	}

	if (sum(grepl('exposure.names', ls(envir = .GlobalEnv))) == 0) {
		exposure.names <<- c('missyr',
												 # 'dry',
												 'soluble',
												 'straight',
												 'synthetic',
												 'grinding',
												 'machining',
												 'grinding soluble',
												 'machining soluble'
												 # 'str.grinding',
												 # 'str.machining',
												 # 'syn.grinding',
												 # 'syn.machining'
		)
	}

	# Misisng data folks (studyno 133570 is pesky)
	cohort_analytic[!complete.cases(
		cohort_analytic[,.(
			straight, soluble, synthetic
		)]) &
			# year < year(yout)  &
			wh == 1 & nohist == 0, .(
				studyno, year, yout,
				straight,
				soluble,
				synthetic)]

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
	cum_exposure.names <- paste0("cum_", exposure.names)
	cohort_analytic[order(year), (cum_exposure.names) := list(
		cum_missyr = cumsum(missyr),
		# cum_dry = cumsum(dry),
		cum_soluble = cumsum(soluble),
		cum_straight = cumsum(straight),
		cum_synthetic = cumsum(synthetic),
		cum_grinding = cumsum(grinding),
		cum_machining = cumsum(machining),
		cum_grinding_soluble= cumsum(`grinding soluble`),
		cum_machining_soluble = cumsum(`machining soluble`)
		# cum_str.grinding = cumsum(str.grinding),
		# cum_str.machining = cumsum(str.machining),
		# cum_str.grinding = cumsum(syn.grinding),
		# cum_str.machining = cumsum(syn.machining)
	), by = .(studyno)]

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