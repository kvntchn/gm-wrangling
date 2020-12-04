# Making person-year dataset ####
# Kevin Chen
# July 15, 2019

gm.to.date <- function(x) {
	if (x[1] < 200) {x <- x + 1900}
	as.Date(paste0(floor(x), '/01/01')) +
		floor((x - floor(x)) * time_length(difftime(
			as.Date(paste0(floor(x), "-12-31")),
			as.Date(paste0(floor(x), "-01-01"))), "day"
		))}

date.to.gm <- function(x = "2013-01-01") {
	as.numeric(
		year(x) +
			time_length(difftime(x, as.Date(paste0(year(x), "-01-01"))), "year") / (
			time_length(difftime(as.Date(paste0(year(x), "-12-31")),
								 as.Date(paste0(year(x), "-01-01"))), "year"))
	)}

# Person-year dataset ending on whichever date ####
get.cohort_py <- function(
	full_cohort = NULL,
	hire.year.min = -Inf, #1938,
	end.year = NULL,
	hire.year.max = 1981,
	outcome_type = "mortality",
	start.name = "yin16",
	deathage.max = NULL,
	use_seer = F) {

	incidence.key <- data.table::fread(here::here("../gm-wrangling/cancer incidence", 'cancer-key.tsv'))

	if (is.null(full_cohort)) {
		full_cohort <- as.data.table(as.data.frame(cohort))
	}

	if (outcome_type == "incidence" & is.null(end.year)) {
		end.year <- 2015
	}

	cohort_py <- as.data.table(as.data.frame(full_cohort))

	# Subset data by hire date
	cohort_py <- cohort_py[floor(yin16 + 1900) >= hire.year.min &
												 	floor(yin16 + 1900) <= hire.year.max,]

	# Get oldest age at death
	if (is.null(deathage.max)) {
		death.age.max <<- deathage.max <- max(cohort_py[
			!is.na(yod) & floor(yod) <= end.year,
			yod - yob], na.rm = T)
	}

	# Make censoring date
	if (is.finite(deathage.max)) {
	cohort_py[,`:=`(
		yoc = gm.to.date(yob + deathage.max)
	)]}

	if (deathage.max == Inf) {cohort_py[,`:=`(
		yoc = as.Date(paste0(end.year, "-12-31"))
	)]}

	# Make if censoring date greater than observed date of death, make NA
	cohort_py[yoc > gm.to.date(yod), yoc := NA]
	# cohort_py[, yoc.gm := date.to.gm(yoc)]

	# Transform variable classes, labels, names
	cohort_py[,`:=`(sex = ifelse(sex == 1, 'M', 'F'),
									finrace = ifelse(
										finrace %in% c(1, 2), finrace, 9),
									race = ifelse(race == 0, "Black", "White"),
									plant = as.character(plant),
									yod.gm = yod + 1900,
									yod = gm.to.date(yod),
									yob.gm = yob + 1900,
									yob = gm.to.date(yob),
									start = gm.to.date(get(start.name)),
									end = {
										end <- gm.to.date(yod)
										end[is.na(yod) | (floor(yod + 1900) > end.year)] <- as.Date(
												paste0(end.year, '-12-31'))
										end
									},
									yrin = gm.to.date(yrin16),
									yin.gm = yin16 + 1900,
									yin = gm.to.date(yin16),
									yout.gm = yout16 + 1900,
									yout = gm.to.date(yout16)
	)]

	cohort_py <- cohort_py[end >= start]

	# Make sure levels are ordered sensibly
	cohort_py[,`:=`(
		sex = factor(sex,
								 levels = c("M", "F"), labels = c("M", "F")),
		race = factor(race,
									levels = c("White", "Black"),
									labels = c("White", "Black")),
		plant = factor(plant, levels = c(1,2,3,9))
	),
	by = .(studyno)]

	# Make long data and merge by studnyo
	cohort_py <- merge(
		cohort_py,
		cohort_py[, .(
			year = seq(year(start), year(end)),
			age = seq(year(start), year(end)) - year(yob)),
			by = .(studyno)],
		on = "studyno")

	# cohort_py$end %>% summary

	if (outcome_type == 'incidence') {

		# Remove cancer incidence columns from cohort_py, to make merge cleaner
		cohort_py <- cohort_py[,-c(grep("canc_|ddiag_", names(cohort_py), value = T)), with = F]

		incidence.tab <- cohort[,grepl('canc_|ddiag_|studyno', names(cohort)), with = F]
		names(incidence.tab) <- gsub("_yn", "", names(incidence.tab))

		# Make NA cancer indicators 0
		canc.names <- grep("canc_", names(incidence.tab), value = T)
		incidence.tab[, (canc.names):=(
			lapply(canc.names, function(code) {
				canc_tmp <- get(code)
				canc_tmp[is.na(canc_tmp)] <- 0
				canc_tmp})
		)]

		# Include SEER data?
		if (use_seer) {
			if ("seer" %in% ls(envir = .GlobalEnv)) {
				seer <- get("seer", envir = .GlobalEnv)
			} else {
				seer <- box_read(660223470027)
				assign("seer", seer, envir = .GlobalEnv)
			}
			setDT(seer)

			# Set class of ddiag_* to Date
			ddiag.names <- grep("ddiag_", names(seer), value = T)
			seer[ ,(ddiag.names):=(
				lapply(ddiag.names, function(code) {
					as.Date(get(code))}))]

			assign("seer", seer, envir = .GlobalEnv)

			# Use first record: indicator
			incidence.tab[studyno %in% seer$studyno, (canc.names):=(
				lapply(canc.names, function(code = canc.names[1]) {
					canc.seer <- seer[na.exclude(match(
					incidence.tab[studyno %in% seer$studyno, studyno], seer$studyno)), code, with = F]
					canc.seer[is.na(canc.seer)] <- 0
					as.numeric(get(code) + canc.seer > 0)
				})
			)]

			# Use first record: date
			incidence.tab[studyno %in% seer$studyno, (ddiag.names):=(
				lapply(ddiag.names, function(code) {
					apply(data.frame(get(code), seer[na.exclude(match(
					incidence.tab[studyno %in% seer$studyno, studyno], seer$studyno)), code, with = F]), 1,
					function(ddiag) {
						if (is.na(ddiag[1]) & is.na(ddiag[2])) {
							NA } else {min(as.Date(ddiag[1]), as.Date(ddiag[2]), na.rm = T)}})
				}))]
		}

		cohort_py <- merge(
			cohort_py,
			incidence.tab,
			by = c('studyno'),
			all.x = T)


		# Incidence indicator
		# 0: at risk
		# 1: case at time
		# 2: left-censored
		for (code in incidence.key$code) {
			incidence <- paste0("canc_", code)
			incidence.date <- paste0("ddiag_", code)
			cohort_py[, (incidence) := ifelse(
				# Never experienced incident case
				is.na(get(incidence.date)), 0, ifelse(
					# Not yet experienced
					year < year(get(incidence.date)), 0, ifelse(
						# Experienced in the past
						year > year(get(incidence.date)), 2,
						# None of the above means year of incidence!
						1)
				))]
		}

		# # Sensible names
		# incidence.names <- names(cohort_py)[grepl("canc_|ddiag_", names(cohort_py))]
		#
		# # Incidence indicator names
		# incidence.names[na.exclude(
		# 	match(paste0("canc_", incidence.key$code, "_yn"), incidence.names))] <-	paste0(
		# 		"canc_", incidence.key$description[na.exclude(
		# 		match(incidence.names, paste0("canc_", incidence.key$code, "_yn")))])
		#
		# # Date of incidence names
		# incidence.names[na.exclude(
		# 	match(paste0("ddiag_", incidence.key$code), incidence.names))] <-	paste0(
		# 		"ddiag_", incidence.key$description[na.exclude(
		# 		match(incidence.names, paste0("ddiag_", incidence.key$code)))])
		#
		# incidence.names -> names(cohort_py)[grepl("canc_|ddiag_", names(cohort_py))]

	# Any incidence
	canc_first <- melt(
		cohort_py[
			, grep("canc_|studyno|^year$", names(cohort_py)),
			with = F], id.vars = c("studyno", "year"))[,.(
				canc_first = ifelse(sum(value) > 0, 1, 0)), by = .(studyno, year)]
	ddiag_first <- melt(
		cohort_py[
			, grep("ddiag_|studyno", names(cohort_py)),
			with = F], id.vars = c("studyno"))[!is.na(value)]

	# Remove duplicates, if any and set order
	ddiag_first <- ddiag_first[!duplicated(ddiag_first)]
	setorder(ddiag_first, studyno, value)

	# Clean up variable
	ddiag_first[,`:=`(variable = substring(variable, 7))]

	# Pick the most specific label
	ddiag_first <- ddiag_first[,.(
				ddiag_first = min(value, na.rm = T),
				canc_which_first = list(variable[value == min(value)])
				), by = .(studyno)]

	# Merge incidence indicator
	cohort_py <- merge(cohort_py,
				canc_first,
				by = c('studyno', 'year'),
				all.x = T)
	# Merge first first incidence
	cohort_py <- merge(cohort_py,
				ddiag_first,
				by = c('studyno'),
				all.x = T)
	# Appropriate status variable for all cancer indicator
	cohort_py[canc_first > 0 & year > year(ddiag_first), canc_first := 2]
	}

	# Order dataset
	setorder(cohort_py, studyno, year)
	cohort_py[year >= (year(yin) + 3) & year <= apply(
		data.frame(end.year,
							 year(yod),
							 year(yoc)), 1, min, na.rm = T
	), `:=`(
		I = 1:.N,
		N = .N
	), by = .(studyno)]

	# Immortal time
	cohort_py[,immortal := c(1, 1, 1, rep(0, .N - 3)), by = .(studyno)]

	# Right censoring
	cohort_py[, right.censored := ifelse(
		!is.na(yoc) & year > year(yoc), 1, 0)]

	# Calculate person-year contribution of each row
	cohort_py$I <- NA; cohort_py$N <- NA
	cohort_py[immortal == 0 &
							year <= apply(data.frame(
								year(yod), year(yoc), end.year
							), 1, min, na.rm = T), `:=`(
								I = 1:.N,
								N = .N
							), by = .(studyno)]

	cohort_py[is.na(I), py := 0]

	cohort_py[I > 1 & I < N, py := 1]

	cohort_py[I > 1 & I < N, py := 1]

	cohort_py[I == 1 | I == N, py := time_length(difftime(
		as.Date(apply(data.frame(
			yod + days(1), yoc + days(1),
			as.Date("2016-01-01"),
			as.Date(paste0(year + 1, "-01-01"))
		), 1, min, na.rm = T)),
		as.Date(apply(data.frame(
			yin + years(3), as.Date(paste0(year, "-01-01"))
		), 1, max, na.rm = T))
	), 'years')]

	# Basic age
	cohort_py[!is.na(I), `:=`(age.year1 = time_length(
		difftime(as.Date(paste0(year, "-01-01")),
						 yob), 'day'))]
	cohort_py[!is.na(I), `:=`(age.year2 = time_length(
		difftime(as.Date(paste0(year + 1, "-01-01")),
						 yob), 'day'))]

	# Correct age1 at entry
	cohort_py[I == 1, `:=`(age.year1 = time_length(
		difftime(yin, yob), 'day') + 3 * 365)]
	# Correct age2 at death or censoring
	cohort_py[I == N, `:=`(
		age.year2 = time_length(difftime(
			as.Date(apply(
				data.frame(as.Date(paste0(end.year + 1, "-01-01")),
									 yod + days(1),
									 yoc + days(1)), 1, min, na.rm = T
			)), yob), 'day'))]

	# Corect false precision
	cohort_py[,`:=`(
		age.year1 = floor(age.year1),
		age.year2 = floor(age.year2)
	)]

	# Years since entry
	cohort_py[, yearwork := year - year(start) + 1, by = .(studyno)]


	# Make year1 and year2
	cohort_py[I == 1, `:=`(
		year1 = yin + years(3) - days(1)
	)]
	cohort_py[I > 1, `:=`(
		year1 = as.Date(paste0(year - 1, "-12-31"))
	)]
	cohort_py[,`:=`(
		year2 = as.Date(apply(data.frame(
			as.Date(paste0(year, "-12-31")),
			as.Date(paste0(end.year, "-12-31")),
			yod,
			yoc), 1, min, na.rm = T
		)))
	]

	cohort_py <- cohort_py[,-c("I", "N")]

	return(cohort_py)
}

# cohort_py[age < 18, .(
# 	studyno,
# 	sex,
# 	race,
# 	start = year(start),
# 	end = year(end),
# 	year,
# 	age
# )]


# Sanity checks
# cohort_py[year(end) == year, .(studyno), by = .(studyno)] %>% nrow
# cohort_py[year(yod) == year, .(studyno), by = .(studyno)] %>% nrow