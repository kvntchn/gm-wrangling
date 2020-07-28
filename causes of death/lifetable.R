# Extraction and wrangling of lifetables ####
# Kevin Chen
# July 11, 2019

library(data.table)
library(here)
library(tidyverse)

# rm(list = ls()[-which(ls() == 'cohort')])

if (!('death_type' %in% ls())) {
	source(here::here('../gm-wrangling/causes of death', 'icd.R'))
}

og.ls <- ls()

ltab_calendar <- function(x) {
	if (lubridate::is.Date(x)) {
		x <-
			year(x) + (month(x) - 1) / 12 + day(x) / 365.25
	}
	cut(x, c(seq(1915, 2016, by = 5), Inf), right = F)
}

ltab_age <- function(x) {
	cut(x, c(10, seq(20, 85, by = 5), Inf), right = F)
}

# Causes of interest ####
cod.levels <- c(
	"All causes",
	spec_icd_codes()$icd10_descriptions,
	'Suicide',
	"Overdose",
	spec_icd_codes(natural_causes = T, totals = F)$icd10_descriptions
	)
cod.levels <- cod.levels[
	c(1,length(cod.levels),2:(length(cod.levels) - 1))]

# Get LTAS-extracted data ####

# Names
niosh.name <- paste0('niosh', sprintf('%02d', c(1:92)))

# Load
sapply(1:length(niosh.name), function(x) {
	assign(niosh.name[x],
				 fread(here::here(
				 	'causes of death/NIOSH rates',
				 	paste0(substring(niosh.name[x], 6), '.csv')
				 )),
				 inherits = T)
	if (x != 1) {
		setcolorder(get(niosh.name[x]),	c(names(get(niosh.name[1]))))
	}
})

# Clean up workshpace
for (i in 1:length(niosh.name)) {
	if (i == 1) {
		niosh <- get(niosh.name[i])
	} else {
		niosh <- rbind(niosh, get(niosh.name[i]))
	}
	rm(list = niosh.name[i])
}
rm(niosh.name)
rm(i)

# Get CDC Wonder-extracted data ####

# Names
cdc.name <-
	sapply(list.files(here::here("causes of death/cdc wonder"), ".*txt"),
				 function(x) {
				 	paste0('cdc', substr(x, 12, nchar(x) - 4))
				 })

cdc.name <- sort(cdc.name)

# Load
for (i in 1:length(cdc.name)) {
	assign(cdc.name[i],
				 fread(here::here(
				 	'causes of death/cdc wonder',
				 	paste0(names(cdc.name[i]))
				 ),
				 fill = T))
	# Remove notes
	assign(cdc.name[i],
				 get(cdc.name[i])[1:(which(Notes == '---')[1] - 1),])
	assign(cdc.name[i],
				 get(cdc.name[i])[,-'Notes'])
	# In-reading changes
	tmp <- get(cdc.name[i])
	# Add ICD version
	if (max(tmp$Year, na.rm = T) > 1998) {
		tmp$v_icd = 10
	}
	if (max(tmp$Year, na.rm = T) <= 1998 &
			min(tmp$Year, na.rm = T) > 1978) {
		tmp$v_icd = 9
	}
	if (max(tmp$Year, na.rm = T) <= 1978) {
		tmp$v_icd = 8
	}
	# Easy age group names
	names(tmp) <- substring(names(tmp), gregexpr('Age Groups',
																							 names(tmp)))
	if ('Age Group' %in% names(tmp)) {
		names(tmp) <- gsub("Age Group", "Age Groups", names(tmp))
	}
	# Remove wild spaces
	tmp$`Age Groups` <- gsub(" $", "", tmp$`Age Groups`)
	# Make Population numeric
	tmp$Population <- as.character(tmp$Population)
	# Merge back
	assign(cdc.name[i], tmp)
	rm(tmp)
	if (i != 1 &
			ncol(get(cdc.name[1])) ==
			ncol(get(cdc.name[i]))) {
		setcolorder(get(cdc.name[i]),	c(names(get(cdc.name[1]))))
	}
}

# Clean up workspace
for (i in 1:length(cdc.name)) {
	if (i == 1) {
		cdc <- get(cdc.name[i])
		rm(list = cdc.name[i])
	} else {
		if (ncol(get(cdc.name[i])) == ncol(cdc)) {
			cdc <- rbindlist(list(cdc, get(cdc.name[i])), use.names = T)
			rm(list = cdc.name[i])
		}
	}
}
rm(cdc.name)
rm(i)

# Get CDC All-cause deaths ####
cdcAllCause <- rbindlist(
	list(
		# cdcAllCause_79_98,
		cdcAllCause_99_08,
		cdcAllCause_09_16,
		cdcAllCauseElderly_99_08,
		cdcAllCauseElderly_09_16
	),
	use.names = T
)

rm(
	list = c(
		# "cdcAllCause_79_98",
		"cdcAllCause_99_08",
		"cdcAllCause_09_16",
		"cdcAllCauseElderly_99_08",
		"cdcAllCauseElderly_09_16"
	)
)

cdcAllCause <- cdcAllCause[Population != 'Not Applicable', ]
cdcAllCause[, Population := as.numeric(Population)]

# Remove rows for age groups we don't want ####
cdc <- cdc[(
	`Age Groups` %in% c(
		"15-19 years",
		"20-24 years",
		"25-29 years",
		"30-34 years",
		"35-39 years",
		"40-44 years",
		"45-49 years",
		"50-54 years",
		"55-59 years",
		"60-64 years",
		# Note the extra space was removed
		"65-69 years",
		"70-74 years",
		"75-79 years",
		"80-84 years",
		"85+ years"
	)
),]

cdcAllCause <- cdcAllCause[(
	`Age Groups` %in% c(
		"15-19 years",
		"20-24 years",
		"25-29 years",
		"30-34 years",
		"35-39 years",
		"40-44 years",
		"45-49 years",
		"50-54 years",
		"55-59 years",
		"60-64 years",
		"65-69 years",
		"70-74 years",
		"75-79 years",
		"80-84 years",
		"85+ years"
	)
),]

# Clean up datasets ####

# Clean up icd10 codes by removing '.' for icd.R ####
cdc[, icd := gsub('\\.', '', `Cause of death Code`)]

# Remove anything 2010 or later from niosh ####
niosh <- niosh[`Calendar Period` <= 14, ]

# CDC to general coding ####
cdc[, `:=`(
	Race = factor(
		ifelse(Race == "White", 1, 0),
		levels = c(0, 1),
		labels = c("Not white", 'White')
	),
	Gender = factor(
		Gender,
		levels = c("Female", "Male"),
		labels = c('F', 'M')
	),
	Calendar = cut(Year, c(seq(1940, 2016, by = 5), Inf), right = F),
	Age = factor(`Age Groups`),
	Population = as.numeric(Population),
	Deaths = as.numeric(Deaths)
)]

cdcAllCause[, `:=`(
	Race = factor(
		Race,
		levels = c("Black or African American", "White"),
		labels = c("Not white", 'White')
	),
	Gender = factor(
		Gender,
		levels = c("Female", "Male"),
		labels = c('F', 'M')
	),
	Calendar = cut(Year, c(seq(1940, 2016, by = 5), Inf), right = F),
	Population = as.numeric(Population),
	Deaths = as.numeric(Deaths)
)]

cdcAllCause <- cdcAllCause[(`Age Groups` %in% levels(cdc$Age)), ]

cdcAllCause[, Age := factor(`Age Groups`)]

# NIOSH to general coding ####

niosh <- niosh[, .(
	cod = Minor,
	Race = factor(
		Race,
		levels = c(2, 1),
		labels = c('Not white', 'White')
	),
	Gender = factor(Gender,
									levels = c(2, 1),
									labels = c('F', 'M')),
	Age = factor(Age,
							 levels = 1:15,
							 labels = levels(cdc$Age)),
	Calendar = factor(
		`Calendar Period`,
		levels = 1:16,
		labels = levels(cdc$Calendar)
	),
	Rate = Rate
)]

# Remove anything before 2010 from cdc ####
cdcDespair <- as.data.table(as.data.frame(cdc[Year >= 2000]))
cdc <- cdc[Year >= 2010, ]
cdcAllCause <- cdcAllCause[Year >= 2010, ]

# CDC Despair ####
despair.icd <- self_injury.function()
# cdc.despair <- death_type(
# 	cdcDespair$icd,
# 	cdcDespair$v_icd,
# 	codes = list(
# 		icd9_codes = list(
# 			Suicide = despair.icd$suicide_codes$icd[
# 				despair.icd$suicide_codes$v_icd == 9],
# 			Overdose =  despair.icd$overdose_codes$icd[
# 				despair.icd$overdose_codes$v_icd == 9]),
# 		icd10_codes = list(
# 			Suicide = despair.icd$suicide_codes$icd[
# 				despair.icd$suicide_codes$v_icd == 10],
# 			Overdose =  despair.icd$overdose_codes$icd[
# 				despair.icd$overdose_codes$v_icd == 10]),
# 		icd9_descriptions = c("Suicide", "Overdose"),
# 		icd10_descriptions = c("Suicide", "Overdose")
# 														 ))
# save(cdc.despair, file =
# 		 to_drive_D(here::here('causes of death/resources', 'cdc.despair.rdata')))
load(to_drive_D(here::here("causes of death/resources", "cdc.despair.rdata")))

setDT(cdc.despair$description)

cdcDespair$cod <- factor(cdc.despair$description[, major],
									levels = cod.levels)

cdcDespair <- cdcDespair[!is.na(cod), ]

cdcDespair <- cdcDespair[, .(Deaths = sum(Deaths),
							 Population = unique(Population),
							 Calendar = unique(Calendar)),
					 by = .(cod, Race, Gender, Age, Year)]

# CDC All natural causes ####
cdcAllNatural <- as.data.table(as.data.frame(cdc))

cdcAllNatural <- cdcAllNatural[icd %in% spec_icd_codes(
	totals = F, natural_causes = T)$icd10_codes$`All natural causes`,]

cdcAllNatural$cod <- 'All natural causes'

cdcAllNatural <- cdcAllNatural[, .(
	Deaths = sum(Deaths),
	Population = unique(Population),
	Calendar = unique(Calendar),
	cod = cod
),
by = .(Age, Gender, Race, Year)]

# CDC Aggregates ####
cdc_total <- as.data.frame(cdc)
setDT(cdc_total)

cdc_total$cod <- NA_character_

cdc_total[icd %in% spec_icd_codes(totals_only = T)$icd10_codes$`All cancers`,
					cod := 'All cancers']

cdc_total[icd %in%
						spec_icd_codes(totals_only = T)$icd10_codes$`All nonmalignant respiratory diseases`,
					cod := 'All nonmalignant respiratory diseases']

cdc_total[icd %in% spec_icd_codes(totals_only = T)$icd10_codes$`All heart diseases`,
					cod := 'All heart diseases']

cdc_total[icd %in%
						spec_icd_codes(
							totals_only = T)$icd10_codes$`All external causes`,
					cod := 'All external causes']

cdc_total <- cdc_total[!is.na(cod), ]

cdc_total <- cdc_total[, .(
	Deaths = sum(Deaths),
	Population = unique(Population),
	Calendar = unique(Calendar)
),
by = .(Age, Gender, Race, Year, cod)]


# CODs for cdc dataset ####
# cdc.death_type <- death_type(cdc$icd, cdc$v_icd,
# 														 codes = spec_icd_codes(totals = F))
# save(cdc.death_type,
# 		 file =	to_drive_D(here::here('causes of death/resources', 'cdc.death_type.rdata'))
# )
load(to_drive_D(here::here('causes of death/resources', 'cdc.death_type.rdata')))

setDT(cdc.death_type$description)

cdc$cod <- factor(cdc.death_type$description[, major],
									levels = cod.levels)

cdc <- cdc[!is.na(cod), ]

cdc <- cdc[, .(Deaths = sum(Deaths),
							 Population = unique(Population),
							 Calendar = unique(Calendar)),
					 by = .(cod, Race, Gender, Age, Year)]

# NIOSH All-cause deaths ####

nioshAllCause <- as.data.frame(niosh)
setDT(nioshAllCause)
nioshAllCause <- nioshAllCause[, .(cod = 'All causes',
																	 Rate = sum(Rate)),
															 by = .(Race, Gender, Age, Calendar)]

# NIOSH All natural causes ####

nioshAllNatural <- as.data.frame(niosh)
setDT(nioshAllNatural)
nioshAllNatural <- nioshAllNatural[cod %in% c(1:83, 92),
																	 .(cod = 'All natural causes',
																	 	Rate = sum(Rate)),
																	 by = .(Race, Gender, Age, Calendar)]

# NIOSH Aggregates ####

niosh.key <-
	fread(
		here::here('causes of death/NIOSH rates', 'minor-key.tsv'),
		sep = '\t',
		fill = T,
		na.strings = ''
	)

niosh.key <- niosh.key[!is.na(Description), ]


niosh_total <- as.data.frame(niosh)
setDT(niosh_total)

# All cancers
niosh_total[cod %in% 3:41,
						cod_total := "All cancers"]
# All nonmalignant respiratory diseases
niosh_total[cod %in% c(1, 2, 59:64),
						cod_total := "All nonmalignant respiratory diseases", ]
# All heart diseases
niosh_total[cod %in% c(51:58),
						cod_total := "All heart diseases", ]
# All External causes
niosh_total[cod %in% c(84:90),
						cod_total := "All external causes", ]

niosh_total[, cod := cod_total]
niosh_total <- niosh_total[!is.na(cod_total), -'cod_total']

niosh_total <- niosh_total[, .(Rate = sum(Rate)), by = .(cod,
																												 Race,
																												 Gender,
																												 Age,
																												 Calendar)]

# NIOSH Accidents ####
nioshAccidents <- as.data.frame(niosh)
setDT(nioshAccidents)
# Accidents (vehicle, falls, medical)
nioshAccidents[cod %in% c(84, 86, 88),
						cod_total := "Accidents (transportation, falls, and medical)", ]

nioshAccidents[, cod := cod_total]
nioshAccidents <- nioshAccidents[!is.na(cod_total), -'cod_total']

nioshAccidents <- nioshAccidents[, .(Rate = sum(Rate)), by = .(cod,
																												 Race,
																												 Gender,
																												 Age,
																												 Calendar)]


# CODs for NIOSH dataset ####
niosh <- niosh[(cod %in% niosh.key$Code), ]

niosh[, cod := as.character(factor(cod,
											levels = niosh.key$Code,
											labels = niosh.key$Description))]

# Factor CODs ####
cdc$cod <- factor(cdc$cod, levels = cod.levels)
cdcDespair$cod <- factor(cdcDespair$cod, levels = cod.levels)
cdc_total$cod <- factor(cdc_total$cod, levels = cod.levels)
cdcAllCause$cod <- 'All causes'
cdcAllCause$cod <- factor(cdcAllCause$cod, levels = cod.levels)
cdcAllNatural$cod <- 'All natural causes'
cdcAllNatural$cod <- factor(cdcAllNatural$cod, levels = cod.levels)

niosh$cod <- factor(niosh$cod, levels = cod.levels)
niosh_total$cod <- factor(niosh_total$cod, levels = cod.levels)
nioshAllCause$cod <- 'All causes'
nioshAllCause$cod <- factor(nioshAllCause$cod, levels = cod.levels)
nioshAllNatural$cod <- 'All natural causes'
nioshAllNatural$cod <- factor(nioshAllNatural$cod, levels = cod.levels)
nioshAccidents$cod <- "Accidents (transportation, falls, and medical)"
nioshAccidents$cod <- factor(nioshAccidents$cod, levels = cod.levels)

# Rate Calculations ####
cdc <- cdc[, .(Rate = sum(Deaths)/sum(Population)),
		by = .(Race, Gender, Age, Calendar, cod)]

cdcDespair <- cdcDespair[, .(Rate = sum(Deaths)/sum(Population)),
		by = .(Race, Gender, Age, Calendar, cod)]

cdc_total <- cdc_total[, .(Rate = sum(Deaths)/sum(Population)),
		by = .(Race, Gender, Age, Calendar, cod)]

cdcAllNatural <- cdcAllNatural[, .(Rate = sum(Deaths)/sum(Population)),
		by = .(Race, Gender, Age, Calendar, cod)]

cdcAllCause <- cdcAllCause[, .(Rate = sum(Deaths)/sum(Population)),
		by = .(Race, Gender, Age, Calendar, cod)]


# Variable pruning and renaming ####
cdc <-
	cdc[, .(
		cod,
		race = Race,
		sex = Gender,
		age = Age,
		calendar = Calendar,
		rate = Rate
	)]

cdcDespair <-
	cdcDespair[, .(
		cod,
		race = Race,
		sex = Gender,
		age = Age,
		calendar = Calendar,
		rate = Rate
	)]

cdc_total <-
	cdc_total[, .(
		cod,
		race = Race,
		sex = Gender,
		age = Age,
		calendar = Calendar,
		rate = Rate
	)]

niosh <- niosh[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]

niosh_total <- niosh_total[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]

nioshAccidents <- nioshAccidents[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]

# Clean all cause rates ####

cdcAllCause <- cdcAllCause[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]

nioshAllCause <- nioshAllCause[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]

cdcAllNatural <- cdcAllNatural[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]

nioshAllNatural <- nioshAllNatural[, .(
	cod,
	race = Race,
	sex = Gender,
	age = Age,
	calendar = Calendar,
	rate = Rate
)]


# Bind ####
cdc <- rbindlist(list(cdc, cdcAllCause, cdc_total,
											cdcAllNatural, cdcDespair),
								 use.names = T)
niosh <- rbindlist(list(niosh, nioshAllCause, niosh_total,
												nioshAllNatural, nioshAccidents),
									 use.names = T)
ltab <- rbindlist(list(cdc, niosh), idcol = T)

# Calendar/age categories ####
ltab[, age := factor(
	age,
	labels = c(
		'[15,20)',
		'[20,25)',
		'[25,30)',
		'[30,35)',
		'[35,40)',
		'[40,45)',
		'[45,50)',
		'[50,55)',
		'[55,60)',
		'[60,65)',
		'[65,70)',
		'[70,75)',
		'[75,80)',
		'[80,85)',
		'[85,Inf)'
	)
)]

# Clean up workspace ####
rm.ls <-
	c(ls()[!(ls() %in% c(og.ls,
											 grep('ltab', ls(), value = T),
											 'niosh.key',
											 'cod.levels'))], 'rm.ls')
rm(list = rm.ls)

