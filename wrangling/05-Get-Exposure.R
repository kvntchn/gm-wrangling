# Get job history ####
# Kevin Chen
# 7/19/2019

library(tidyverse)
library(data.table)
library(boxr)
box_auth()

get.exposure <- function(rebuild = F,
												 source_version = 3) {
	if (rebuild) {
		# Get exposure data ####
		if (!('san.E' %in% ls())) {
			# Gear & Axle: 16061 entries
			# ganexp2.sas7bdat 291024652584
			# ganexp3.sas7bdat 291020648189
			# ganexp_pm4.sas7bdat 291022617105
			gan.E <- box_read(
				ifelse(source_version == 3, 291020648189,
							 291024652584))

			# Hydramatic: 17268 entries
			# hanexp2.sas7bdat 291024785505
			# nrow(han.E2)
			# hanexp3.sas7bdat 291020798024
			# hanexp_pm4.sas7bdat 291022471219
			han.E <- box_read(
				ifelse(source_version == 3, 291020798024,
							 291024785505))

			# Saginaw: 12739 entries
			# sanexp2.sas7bdat 291024820446
			# san.E2 <- box_read(291024820446)
			# nrow(san.E2)
			# sanexp3.sas7bdat 291020956905
			# sanexp_pm4.sas7bdat 291022636607
			san.E <- box_read(
				ifelse(source_version == 3, 291020956905,
							 291024820446))

			sapply(c('san.E', 'han.E', 'gan.E'), function(i) {
				tmp <- get(i)
				names(tmp) <- tolower(names(tmp))
				assign(i, as.data.table(tmp), inherits = T)
			})
		}

			# data.frame(
			# 	Plant  = c('Gear & Axle', 'Hydramatic', 'Saginaw'),
			# 	Exposure = c(
			# 		nrow(gan.E),
			# 		nrow(han.E),
			# 		nrow(san.E)),
			# 	'Cohort' = c(
			# 		nrow(cohort[plant == 1]),
			# 		nrow(cohort[plant == 2]),
			# 		nrow(cohort[plant == 3])),
			# 	'Cohort (WH = 1)' = c(
			# 		nrow(cohort[plant == 1 & wh == 1]),
			# 		nrow(cohort[plant == 2 & wh == 1]),
			# 		nrow(cohort[plant == 3 & wh == 1])),
			# 	'Cohort (WH = 1 & NOHIST = 0)' = c(
			# 		nrow(cohort[plant == 1 & nohist == 0 & wh == 1]),
			# 		nrow(cohort[plant == 2 & nohist == 0 & wh == 1]),
			# 		nrow(cohort[plant == 3 & nohist == 0 & wh == 1])),
			# 	check.names = F
			# )

		# Multiple plant folks
		# sort(intersect(san.E$studyno, han.E$studyno))
		# sort(intersect(san.E$studyno, gan.E$studyno))
		# sort(intersect(gan.E$studyno, han.E$studyno))

		# Wide to long
		sapply(c('gan.E', 'han.E', 'san.E'), function (i = "gan.E") {
			years <- sort(as.numeric(unique(substring(
				names(gan.E),	(gregexpr('[0-9]*$', names(gan.E)))
			))[-1]))
			tmp <- melt(
				get(i),
				id.vars = c("studyno", "factory"),
				measure.vars = list(
					paste0('missyr', years),
					paste0('solexp', years),
					paste0('strexp', years),
					paste0('synexp', years),
					paste0('grexp', years),
					paste0('macexp', years),
					paste0('goexp', years),
					paste0('moexp', years)
					),
				value.name = c(
					'missyr',
					'soluble',
					'straight',
					'synthetic',
					'grinding',
					'machining',
					'grinding soluble',
					'machining soluble'
				),
				variable.name = 'year',
				variable.factor = F
			)
			names(tmp)[grep('factory', names(tmp))] <- "plant"
			tmp$year <- as.numeric(tmp$year) + 1900 + years[1] - 1
			assign(paste0(i, '.melt'), tmp, inherits = T)
		})

		# Multiple plant, same year folks
		# sort(intersect(paste0(san.E.melt$studyno, '-', san.E.melt$year),
		# 							 paste0(han.E.melt$studyno, '-', han.E.melt$year)))
		# sort(intersect(paste0(san.E.melt$studyno, '-', san.E.melt$year),
		# 							 paste0(gan.E.melt$studyno, '-', gan.E.melt$year)))
		# sort(intersect(paste0(gan.E.melt$studyno, '-', gan.E.melt$year),
		# 							 paste0(han.E.melt$studyno, '-', han.E.melt$year)))

		exposure <- rbindlist(list(# gan.E ~ plant 1
			gan.E.melt,
			# han.E ~ plant 2
			han.E.melt,
			# san.E ~ plant 3
			san.E.melt))

		# Collapse exposure accrued over multiple plants ####
		multiple_plants.who <-
			exposure[, .(n = n_distinct(plant)), by = .(studyno)][n > 1, studyno]
		# exposure[studyno %in% multiple_plants.who, plant := 9]

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

		# exposure[,.(
		# 	missyr   = sum(missyr),
		# 	soluble  = sum(soluble),
		# 	straight = sum(straight),
		# 	synthetic = sum(synthetic),
		# 	grinding = sum(grinding),
		# 	machining = sum(machining)
		# ), by = .(studyno, plant, year)]

		exposure[missyr > 1, missyr := 1]

		# rm(list = c(
		# 	'gan.E',
		# 	'han.E',
		# 	'san.E',
		# 	'gan.E.melt',
		# 	'han.E.melt',
		# 	'san.E.melt'
		# ))

		assign('exposure', exposure, envir = .GlobalEnv)

		box_save(exposure, dir_id = 113431246688, file_name = ifelse(
			source_version == 3,
			"exposure.rdata",
			"exposure2.rdata"))
	} else {
		box_load(ifelse(source_version == 3,
										518504437785,
										536810363286))
	}
}


# get.exposure(rebuild = T,
# 						 source_version = 3)