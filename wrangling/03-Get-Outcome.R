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
	cohort_full = as.data.frame(cohort),
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
		cohort_py <- as.data.table(as.data.frame(get.cohort_py(
			full_cohort = as.data.table(as.data.frame(cohort_full)),
			hire.year.min = hire.year.min,
			end.year = end.year,
			hire.year.max = Inf,
			outcome_type = outcome_type,
			deathage.max = deathage.max,
			use_seer = use_seer
			)))} else {
				cohort_py <- as.data.table(as.data.frame(cohort_py))
			}

	cohort_full <- as.data.table(as.data.frame(cohort_full))

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
	death_suicide <- as.data.table(as.data.frame(
		cohort_full[,.(
			studyno,
			v_icd,
			icd,
			Suicide = ifelse(
				(v_icd == 9 & icd %in% despair.icd$suicide_codes$icd[
					despair.icd$suicide_codes$v_icd == 9]) |
					(v_icd == 10 & icd %in% despair.icd$suicide_codes$icd[
						despair.icd$suicide_codes$v_icd == 10]),
				1, 0)
		)]))[,.(studyno, Suicide)]

	# Overdose indicator ####
	death_overdose <- as.data.table(as.data.frame(
		cohort_full[,.(
			studyno,
			v_icd,
			icd,
			Overdose = ifelse(
				(v_icd == 9 & icd %in% despair.icd$overdose_codes$icd[
					despair.icd$overdose_codes$v_icd == 9]) |
					(v_icd == 10 & icd %in% despair.icd$overdose_codes$icd[
						despair.icd$overdose_codes$v_icd == 10]),
				1, 0)
		)]))[,.(studyno, Overdose)]

	# All causes indicator ####
	death_All_causes <- as.data.table(as.data.frame(
		cohort_full[,.(
			studyno,
			`All causes` = ifelse(!is.na(yod) & !is.na(icd),	1, 0)
		)]))

	# Additional deaths ####
	death_additional <- as.data.table(as.data.frame(
		cohort_full[, .(
			studyno,
			`Gallbladder cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Gallbladder and extrahepatic bile duct cancer")) |
					(v_icd == 10 &
					 	icd %in% additional_outcomes(
					 		10, "Gallbladder and extrahepatic bile duct cancer")),
				1,
				0
			),
			`Liver cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Liver cancer")) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, "Liver cancer")),
				1,
				0
			),
			`Biliary cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Biliary cancer")) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, "Biliary cancer")),
				1,
				0
			),
			`Brain cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Brain cancer")) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, "Brain cancer")),
				1,
				0
			),
			`Colorectal cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Colorectal cancer")) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, "Colorectal cancer")),
				1,
				0
			),
			`Colon cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Colon cancer")) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, "Colon cancer")),
				1,
				0
			),
			`Small intestinal cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, 'Small intestinal cancer')) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, 'Small intestinal cancer')),
				1,
				0
			),
			`Bladder cancer` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, 'Bladder cancer')) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, 'Bladder cancer')),
				1,
				0
			),
			`Melanoma` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, 'Melanoma')) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, 'Melanoma')),
				1,
				0
			),
			`Lymphoid leukemia` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, 'Lymphoid leukemia')) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, 'Lymphoid leukemia')),
				1,
				0
			),
			`Alzheimer's disease` = ifelse(
				(v_icd == 9 & icd %in% additional_outcomes(
					9, "Alzheimer's disease")) |
					(v_icd == 10 & icd %in% additional_outcomes(
						10, "Alzheimer's disease")),
				1,
				0
			)
		)]))

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
	death_obs <- merge(death_obs, death_additional,
										 by = 'studyno', all = T)

	# Remove individuals not in cohort_py
	death_obs <- death_obs[studyno %in% unique(cohort_py$studyno)]

	# Merge death data ####
	cohort_py <- merge(cohort_py, death_obs, by = 'studyno', all = T)
	cod <- names(death_obs)[-1]

	cohort_py[year != year(yod), (cod) := 0, by = 'studyno']

	# Merge sanity check
	# death_obs[,-1] %>% unlist %>% sum
	# cohort_py[, lapply(.SD, sum), by = .(studyno), .SDcols = cod][,-1] %>% unlist %>% sum

	# Analytic dataset ####
	ltab_obs <- as.data.frame(cohort_py)
	setDT(ltab_obs)
	ltab_obs[, `:=`(calendar = factor(ltab_calendar(year)),
									age = factor(ltab_age(age)))]

	return(ltab_obs)
}