# Get outcome ####
# Kevin Chen

ltab_calendar <- function(x) {
	if (lubridate::is.Date(x)) {
		x <- year(x) + (month(x) - 1) / 12 + day(x) / 365.25
	}
	cut(x, c(seq(1915, 2016, by = 5), Inf), right = F)
}

ltab_age <- function(x) {
	cut(x, c(10, seq(20, 85, by = 5), Inf), right = F)
}

get.ltab_obs <- function(
	cohort_full = data.table::copy(cohort),
	cohort_py = NULL,
	deathage.max,
	outcome_type,
	include_alcohol = F,
	end.year,
	hire.year.min,
	hire.year.max,
	use_seer
) {

	if (is.null(cohort_py)) {
		cohort_py <- data.table::copy(get.cohort_py(
			full_cohort = data.table::copy(cohort_full),
			hire.year.min = hire.year.min,
			end.year = end.year,
			hire.year.max = Inf,
			outcome_type = outcome_type,
			deathage.max = deathage.max,
			use_seer = use_seer
			))} else {
				cohort_py <- data.table::copy(cohort_py)
			}

	cohort_full <- data.table::copy(cohort_full)

	# Get cohort death types
	cohort.deaths <- death_type(
		icd = cohort_full$icd,
		v_icd = cohort_full$v_icd,
		description = T,
		codes = spec_icd_codes(totals = F),
		studyno = cohort_full$studyno
	)

	cohort.deaths$description[is.na(cohort.deaths$description)] <- 'Alive'

	cohort.deaths_total <- death_type(
		icd = cohort_full$icd,
		v_icd = cohort_full$v_icd,
		description = T,
		codes = spec_icd_codes(totals_only = T),
		studyno = cohort_full$studyno
	)

	cohort.deaths_total$description[is.na(cohort.deaths_total$description)] <- 'Alive'

	cohort.deaths_natural <- death_type(
		icd = cohort_full$icd,
		v_icd = cohort_full$v_icd,
		description = T,
		codes = spec_icd_codes(
			natural_causes = T,
			totals = F,
			totals_only = F
		),
		studyno = cohort_full$studyno
	)

	cohort.deaths_natural$description[is.na(cohort.deaths_natural$description)] <- 'Alive'

	# Make wide format, with indicator columns for the death types
	death_obs <-
		model.matrix( ~ -1 + major, cohort.deaths$description['major'])
	death_obs_total <-
		model.matrix( ~ -1 + major, cohort.deaths_total$description['major'])

	death_obs_natural <- cohort.deaths_natural$description['major']
	death_obs_natural[, 1] <- ifelse(death_obs_natural$major == 'All natural causes', 1, 0)
	names(death_obs_natural) <- "All natural causes"

	# Pretty column names
	colnames(death_obs) <- gsub('major', '', colnames(death_obs))
	colnames(death_obs_total) <-
		gsub('major', '', colnames(death_obs_total))

	# Make column for studyno
	death_obs <-
		as.data.frame(cbind(as.numeric(rownames(death_obs)), death_obs))
	colnames(death_obs)[1] <- 'studyno'

	death_obs_total <-
		as.data.frame(cbind(as.numeric(rownames(death_obs_total)), death_obs_total))
	colnames(death_obs_total)[1] <- 'studyno'

	death_obs_natural <-
		as.data.frame(cbind(as.numeric(rownames(death_obs_natural)), death_obs_natural))
	colnames(death_obs_natural)[1] <- 'studyno'

	# Suicide indicator ####
	despair.icd <- self_injury.function(alcohol = include_alcohol)
	death_suicide <-  cohort_full[,.(
			studyno,
			v_icd,
			icd,
			Suicide = ifelse(
				(v_icd == 9 & icd %in% despair.icd$suicide_codes$icd[
					despair.icd$suicide_codes$v_icd == 9]) |
					(v_icd == 10 & icd %in% despair.icd$suicide_codes$icd[
						despair.icd$suicide_codes$v_icd == 10]),
				1, 0)
		)][,.(studyno, Suicide)]

	# Overdose indicator ####
	death_overdose <-  cohort_full[,.(
			studyno,
			v_icd,
			icd,
			Overdose = ifelse(
				(v_icd == 9 & icd %in% despair.icd$overdose_codes$icd[
					despair.icd$overdose_codes$v_icd == 9]) |
					(v_icd == 10 & icd %in% despair.icd$overdose_codes$icd[
						despair.icd$overdose_codes$v_icd == 10]),
				1, 0)
		)][,.(studyno, Overdose)]

	# All causes indicator ####
	death_All_causes <-  cohort_full[,.(
			studyno,
			`All causes` = ifelse(!is.na(yod),	1, 0)
		)]

	# Additional deaths ####
	get.death_indicator <- function(description, icd, v_icd) {
		ifelse((v_icd == 9 & icd %in% additional_outcomes(9, description)) | (
			v_icd == 10 & icd %in% additional_outcomes(10, description)),
			1, 0 )}

	additional_outcomes.list <- additional_outcomes(list_all = T)

	death_additional <- cohort_full[, .(studyno, icd, v_icd)]
	death_additional[,(additional_outcomes.list) := lapply(
		additional_outcomes.list, function(x) {
			get.death_indicator(x, icd, v_icd)})]

	# Merging indicator columns
	setDT(death_obs)
	death_obs <- death_obs[,-c('Alive')]
	setDT(death_obs_total)
	death_obs_total <- death_obs_total[,-c('Alive')]
	setDT(death_obs_natural)
	death_obs <- merge(death_obs, death_obs_total,
										 by = 'studyno', all = T)
	death_obs <- merge(death_obs, death_obs_natural,
										 by = 'studyno', all = T)
	death_obs <- merge(death_obs, death_suicide,
										 by = 'studyno', all = T)
	death_obs <- merge(death_obs, death_overdose,
										 by = 'studyno', all = T)
	death_obs <- merge(death_obs, death_All_causes,
										 by = 'studyno', all = T)
	death_obs <- merge(death_obs, death_additional[,-c("icd", "v_icd")],
										 by = 'studyno', all = T)

	# Remove individuals not in cohort_py
	death_obs <- death_obs[studyno %in% unique(cohort_py$studyno)]

	# Merge death data ####
	cohort_py <- merge(cohort_py, death_obs, by = 'studyno', all = T)
	cod <- names(death_obs)[-1]

	cohort_py[year != year(yod), (cod) := 0, .(studyno)]

	# Merge sanity check
	# death_obs[,-1] %>% unlist %>% sum
	# cohort_py[, lapply(.SD, sum), by = .(studyno), .SDcols = cod][,-1] %>% unlist %>% sum

	# Analytic dataset ####
	ltab_obs <- data.table::copy(cohort_py)
	setDT(ltab_obs)
	ltab_obs[, `:=`(calendar = factor(ltab_calendar(year)),
									age = factor(ltab_age(age)))]

	return(ltab_obs)
}