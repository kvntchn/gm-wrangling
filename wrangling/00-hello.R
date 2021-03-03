# Run basic cleaning ####
# Suzanne Dufault, adapted by Kevin Chen

# Packages and preliminaries ####
library(boxr)
library(lubridate)
library(tidyverse)
library(data.table)
# library(date)
library(xtable)
library(sas7bdat)
library(Hmisc)
library(tikzDevice)
source("~/headRs/00-my-theme.R")

if (!"og.dir" %in% ls()) {og.dir <- getwd()}
if ("here" %in% .packages()) {
	detach("package:here", unload = T)
}
setwd('../gm-wrangling')
library(here)

boxr::box_auth()
dta <- box_read(520881275183) # calls in the auto_vs_15.Rdata file

is.auto_vs_15 <- T

drive_D <- T

# fixes discrepancies (sent to by Liza)
source(here::here("wrangling", "01-Administrative.R"))
cohort <- dta

# Make names lower-case
names(cohort) <- tolower(names(cohort))
setDT(cohort)

# changing race to binary; assume all non-black folks are white
cohort[finrace != 2, race:= 1]
cohort[finrace == 2, race:= 0]

if (is.auto_vs_15) {
	cohort[, yod := yod15]
	cohort[, icd := cod_15]
	# Remove the 10 individuals indicated as dead,
	# but have unknown cause or date of death
	cohort[icd == "" | icd == "N/A", icd := NA]
	# Exclude people who are classified as dead, but have no
	#   cause of death or date of death
	# cohort[(status15 == 6 & (is.na(icd) | is.na(yod))), .(
	# 	studyno, status15, icd, yod, v_icd,
	# 	possdiscr_new, wh, nohist, cancinccoh15_new, tenure = yout16 - yin16)]
	cohort <- cohort[!(status15 == 6 & (is.na(yod)))]
}

# removing individuals who were employed a negative amount of time (?)
# cohort[(yout16 < yin16),]

# changing age of 8 year old employee to 13
cohort[studyno == 118137, yob := yob - 5]

# tikz(here::here('wrangling/exploratory', 'years_at_work_under3.tex'),
# 		 width = 4, height = 3, standAlone = T)
# cohort[yout16 - yin16 < 3,
# 			 .(`Years at work` = yout16 - yin16)] %>% ggplot(
# 			 	aes(x = `Years at work`)
# 			 ) +
# 	geom_histogram(bins = 15) +
# 	labs(y = "Count") +
# 	mytheme
# dev.off()

# nrow(cohort)
# nrow(cohort[yout16 - yin16 < 3,])
# nrow(cohort[wh == 1 & nohist == 0 & possdiscr_new != 3 & flag77 == 0 & oddend == 0 & (is.na(yod15) | yod15 >= 85) & (yout16 - yin16 >= 3)])

cohort <- cohort[yout16 - yin16 >= 3,]

# For building anlaytic dataset ####
source(here::here('causes of death', 'icd.R'))
sapply(sprintf("%02d", 2:6), function(x = "02") {
	source(here::here(
		"wrangling",
		grep(x, list.files('wrangling'), value = T)))
})
get.jobhist()
get.exposure()

# Additional cancer outcomes ####
box_load(782029330903)
additional_cancers <- c("cer", "ute", "ova", "vag", "vul", "ofe")
cohort <- merge(cohort,
			mcr[, c("studyno", as.vector(sapply(additional_cancers, grep, names(mcr), value = T))),
					with = F],
			by = "studyno",
			all.x = T, all.y = F)

for (x in additional_cancers) {
	cohort[is.na(get(paste0("canc_", x))), (paste0("canc_", x)) := 0]
}


detach("package:here", unload = T)
setwd(og.dir)
library(here)
