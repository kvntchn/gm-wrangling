# Get job jobhistory ####
# Suzanne Dufault, adapted by Kevin Chen
# 7/8/2019

library(boxr)
box_auth()
library(tidyverse)
library(data.table)
library(lubridate)

if (!"gm.to.date" %in% ls()) {
	source(here::here("wrangling/02-Get-person-year.R"))
}

get.jobhist <- function(
	rebuild = F,
	save.jobhist = T) {

	if (rebuild) {
		if (!("dta" %in% ls(envir = .GlobalEnv))) {
			dta <- box_read(520881275183) # calls in the auto_vs_15.Rdata file
			setDT(dta)
			assign("dta", dta, envir = .GlobalEnv)
		}

		names(dta) <- tolower(names(dta))

		# Get employment data ####
		if (!(
			"gan.job" %in% ls() &
			"han.job" %in% ls() &
			"san.job" %in% ls()
		)) {
			# gan.job (gpp84_3)
			gan.job <- box_read(291022249034) # Suzanne: 291022142739
			names(gan.job) <- tolower(names(gan.job))
			setDT(gan.job)
			# han.job (hpp84_3)
			han.job <- box_read(291022147710) # Suzanne: 291023875891
			names(han.job) <- tolower(names(han.job))
			setDT(han.job)
			# san.job (spp84_3)
			san.job <- box_read(291022251725) # Suzanne: 291022073876
			names(san.job) <- tolower(names(san.job))
			setDT(san.job)

			# Add plant id
			gan.job$plant <- 1
			han.job$plant <- 2
			san.job$plant <- 3
		}

		# Merge plants ####
		jobhist <- rbindlist(list(gan.job, han.job, san.job),
												 use.names = T)

		# Remove those not in main cohort #
		# jobhist <- jobhist[studyno %in% unique(cohort$studyno),]

		# Clean up dates
		jobhist[, `:=`(datein = as.Date(datein, "ymd"),
									 dateout = as.Date(dateout, "ymd"))]

		# Pretty order and cull variables
		jobhist <- jobhist[order(studyno, datein)]
		jobhist <- jobhist[, .(
			studyno, years, datein, dateout, histcode, plant, mach,
			bio, cl, ea, tea, trz, s, no2,
			NULL)]

		# No need to conisder machining codes; collapse by studyno, datein, dateout, histcode, plant
		jobhist <- jobhist[!duplicated(jobhist)]

		# Simplify histcode by cleaning up histcode and mach code
		jobhist[, `:=`(
			histcode = ifelse(
				histcode %in% c("OFF", "SR"),
				"off",
				ifelse(histcode %in% c("MSS", "SD"), "missing",
							 ifelse(histcode == "MD", "discrepancy", "numeric"))
			),
			machining = ifelse(mach == "AS", "assembly",
												 ifelse(mach == "" | mach == "NO", as.character(NA),
												 			 "machining")),
			grinding = as.numeric(mach == "GR")
		)]

		# Just a check for duplicates; there are none
		# nrow(jobhist[duplicated(jobhist)])
		jobhist <- jobhist[!duplicated(jobhist[,-"mach", with = F])]

		# Cast machining
		# table(jobhist$machining)
		# table(jobhist$grinding, useNA = "always")
		jobhist <- dcast(jobhist,
										 as.formula(paste(
										 	paste(names(jobhist)[!names(jobhist) == "machining"], collapse = " + "),
										 	" ~ machining")),
										 value.var = "machining",
										 fun.aggregate = function(x) {as.numeric(length(x) >= 1)})

		# All NA machinining codes have histcode off, missing, or discrepancy OR mach NO
		# jobhist[`NA` > 0 & assembly == 0 & machining == 0, histcode] %>% table
		# jobhist[`NA` > 0 & assembly == 0 & machining == 0 & histcode == "numeric", mach] %>% table
		jobhist <- jobhist[,-"NA"]

		# Convert dichotomous flags to integer
		flags.names <- c("bio", "cl", "ea", "tea", "trz", "s", "no2")
		jobhist[,(flags.names):=(
			lapply(flags.names, function(x) {
				x <- get(x)
				x <- as.numeric(factor(x, c("N", "Y"))) - 1
				x[is.na(x)] <- 0
				x
			})
		)]

		# Collapse by studyno, datein, datout, histcode, plant
		jobhist <- jobhist[, .(years = sum(years),
													 assembly,
													 grinding, machining,
													 mach,
													 bio, cl, ea, tea, trz, s, no2),
											 by = .(studyno, datein, dateout, histcode, plant)]

		# Cast by studyno-datein-dateout ####
		# table(jobhist$histcode)
		jobhist <- dcast(
			jobhist,
			studyno + datein + dateout + plant + assembly + grinding + machining +
				bio + cl + ea + tea + trz + s + no2 ~ histcode,
			value.var = "histcode",
			fun.aggregate = function(x) {as.numeric(length(x) >= 1)})

		jobhist[is.na(jobhist)] <- 0

		# # No entries where there are different "exposure years" with different codes
		# 		jobhist[(missing > 0) + (off > 0) + (discrepancy > 0) + (numeric > 0) > 1]

		# Making sure we have unique datein-dateout-studyno-plant rows
		jobhist <- jobhist[, .(
			assembly = max(assembly),
			grinding = max(grinding),
			machining = max(machining),
			numeric = max(numeric),
			off = max(off),
			missing = max(missing),
			discrepancy = max(discrepancy),
			bio = max(bio),
			cl = max(cl),
			ea = max(ea),
			tea = max(tea),
			trz = max(trz),
			s = max(s),
			no2 = max(no2)
			),
			by = .(studyno, plant, datein, dateout)]

		# Many plants?
		multiple.who <- unique(jobhist[!is.na(plant), .(
				nplants = n_distinct(plant)),
				by = studyno][nplants > 1, studyno])

		# Check for contiguosness?
		interval.shift <- jobhist[,.(
			datein = dateout[-length(dateout)],
			dateout = datein[-1]
		), by = .(studyno)]
		interval.shift <- interval.shift[datein != dateout &
																		 	datein < dateout]

		# Fill discontinuities
		# ignore obvious holidays for now
		jobhist <- merge(jobhist, interval.shift, all = T,
										 by = c("studyno", "datein", "dateout"))

		# jobhist[is.na(numeric), .(
		# 	difference = time_length(difftime(dateout, datein), "days"))]$difference %>% summary

		# Fill in plants
		setorder(jobhist, studyno, datein, dateout, plant)
		jobhist[, `:=`(
			plant = zoo::na.locf(plant)), by = .(studyno)]

		# # Multiple plants in the same record period?
		# jobhist[!is.na(plant),
		# 				.(n = n_distinct(plant)), by = .(
		# 					studyno, datein, dateout
		# 				)][n > 1]
		# #  	 studyno     datein    dateout n
		# # 1:  142471 1992-06-23 1992-06-23 2
		# # 2:  144766 1992-09-14 1992-09-28 2

		# Remove off because there is numeric
		jobhist <- jobhist[!(studyno == 142471 & datein == as.Date("1992-06-23") &
												 	dateout == as.Date("1992-06-23") & off == 1)]

		# Remove missing because there is off
		jobhist <- jobhist[!(studyno == 144766 & datein == as.Date("1992-09-14") &
												 	missing == 1)]

		# Filled discontinuities get assigned missing
		# (even though the entries of length 1 day are probably off)
		jobhist[is.na(numeric) & is.na(missing) & is.na(discrepancy) & is.na(off), `:=`(
			assembly = 0,
			grinding = 0,
			machining = 0,
			numeric = 0,
			missing = 1,
			discrepancy = 0,
			off = 0,
			imputed = 1
		)]
		jobhist[is.na(imputed), imputed := 0]
		jobhist[imputed == 1 & is.na(bio) & is.na(cl) &
							is.na(ea) & is.na(tea) & is.na(trz) & is.na(s) & is.na(no2),
						`:=`(
							bio = 0,
							cl = 0,
							ea = 0,
							tea = 0,
							trz = 0,
							s = 0,
							no2 = 0
						)]

		# # studyno, datein, dateout gives unique row? YES
		# jobhist[,.N, by = .(studyno, datein, dateout)][N > 1]

		# End of active history/employment? ####
		# jobhist <- jobhist[, -c("N")]
		setorder(jobhist, studyno, datein, dateout)
		jobhist[imputed == 0, `:=`(
			I = 1:.N,
			N = .N), by = .(studyno)]

		# # Commented out code for end of active history
		# # i.e. not relevant for end of employment work
		# # No work history
		# nohist.who <- jobhist[, .(nohist = sum(numeric) == 0),
		# 					 by = .(studyno)][nohist == T]$studyno
		# jobhist[studyno %in% nohist.who, `:=`(
		# 	active_end.date = dateout[.N]
		# ), by = .(studyno)]
		#
		# # Has work history, final record off
		# jobhist[!(studyno %in% nohist.who), `:=`(
		# 	active_end.date = ifelse(
		# 		# final off
		# 		off[.N] == 1, ifelse(
		# 			datein[.N] != dateout[.N],
		# 			# final off and datein not dateout
		# 			dateout[.N], ifelse(
		# 				# final off and datein is dateout
		# 				time_length(difftime(
		# 					max(dateout[-.N][missing[-.N] == 0]),
		# 					datein[.N]), "days") < 1,
		# 				# final off and datein is dateout and roughly no gap
		# 				ifelse(off[.N - 1] == 0,
		# 							 # penultimate not off
		# 							 dateout[.N],
		# 							 # penultimate off
		# 							 max(dateout[off == 0])),
		# 				# final off and datein is dateout and has gap
		# 				dateout[.N - 1]
		# 			)
		# 		), as.numeric(NA))
		# ), by = .(studyno)]

		# jobhist.og <- data.table::copy(jobhist)
		# jobhist <- data.table::copy(jobhist.og)
		setDT(dta)
		censored.who <- dta[yout16 == 95, studyno]

		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		# Last dateout ####
		# 2233 folks who were censored had an off record as last record
		# With datein == dateout
		jobhist[studyno %in% censored.who, `:=`(
			dateout.date = {
				if ((missing[!is.na(I) & I == N] +
						discrepancy[!is.na(I) & I == N] +
						off[!is.na(I) & I == N]) > 0) {
					dateout[!is.na(I) & I == N]
				} else {as.Date("1995-01-01")}}
		), by = .(studyno)]
		jobhist[!(studyno %in% censored.who), `:=`(
			dateout.date = dateout[!is.na(I) & I == N]
		), by = .(studyno)]

		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		# Suzanne's YOUT ####
		jobhist[studyno %in% censored.who, `:=`(
			employment_end.date = as.Date("1995-01-01")
		)]
		# Folks whose last entry is non-numeric
		nonum.who <- unique(jobhist[!is.na(I) & I == N & numeric == 0, studyno])
		jobhist[
			# Not censored
			!(studyno %in% censored.who) &
				# Non-numeric last
				studyno %in% nonum.who, `:=`(
					employment_end.date = min(
						dateout[!is.na(I) & I == N],
						datein[!is.na(I) & I == N] %m+% years(10))),
			by = .(studyno)]

		# Final record numeric
		num.who <- unique(jobhist[!is.na(I) & I == N & numeric > 0, studyno])
		jobhist[
			# Not censored
			!(studyno %in% censored.who) &
				# Numeric last
				studyno %in% num.who, `:=`(
					employment_end.date = dateout[!is.na(I) & I == N]
				), by = .(studyno)]

		library(lubridate)

		# Save "old" employment_end.date
		jobhist[, employment_end.date.legacy := employment_end.date]

		# Special attention for non-numeric datein == dateout ####
		in_is_out.who <- jobhist[(
			I == N & datein == dateout & numeric == 0 &
				year(employment_end.date) != 1995), .(studyno)][
					studyno %in% jobhist[
						(I < N & I > (N - 3) & datein == dateout & numeric == 0), studyno],
					studyno]

		# If penultimate also non-numeric datein == dateout, use that (1720 changed)
		jobhist[studyno %in% in_is_out.who,	`:=`(
			employment_end.date = {
				dateout.tmp <- dateout[
					!is.na(I) &
						# penultimate record
						I == (N - 1) &
						# equal datein dateout
						dateout == datein &
						# Missing
						numeric == 0]
				if (length(dateout.tmp) > 0) {
					dateout.tmp
				} else {
					employment_end.date
				}},
			changed = T
		), by = .(studyno)]

		jobhist[changed == 1,.(
			studyno, datein, dateout,
			employment_end.date, I, N)]$studyno %>% table %>% length

		# 3 from last? (0 changed)
		jobhist[studyno %in% in_is_out.who,	.(
			employment_end.date2 = {
				dateout.tmp <- dateout[
					!is.na(I) &
						# penultimate record
						I == (N - 2) &
						# equal datein dateout
						dateout == datein &
						# Missing
						numeric == 0]
				if (length(dateout.tmp) > 0 &
						sum(!is.na(I) &
								# penultimate record
								I == (N - 1) &
								# equal datein dateout
								dateout == datein &
								# Missing
								numeric == 0) > 0) {
					dateout.tmp
				} else {
					employment_end.date
				}},
			changed = T,
			employment_end.date
		), by = .(studyno)][employment_end.date != employment_end.date2]
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		# Liza's YOUT, additionally adjusted with Suzanne's ideas ####
		# jobhist <- jobhist[,-c("yout_recode")]

		# No work history OR has work history, but no numeric ####
		nonum.who <- unique(c(
			dta[wh == 0, studyno],
			jobhist[studyno %in% dta[wh == 1, studyno],.(
				who = sum(numeric) == 0),
				by = .(studyno)][who == T, studyno]))
		length(nonum.who)

		# Final off ####
		# Final record off, datein != dateout
		in_not_out.who <- unique(jobhist[
			!is.na(I) & I == N & datein != dateout &
				off > 0 & numeric == 0 &
				!(studyno %in% nonum.who), studyno
			])
		length(in_not_out.who)
		# Final record off, datein == dateout
		in_is_out.who <- unique(jobhist[
			!is.na(I) & I == N & datein == dateout &
				!(studyno %in% nonum.who) & off > 0, studyno])
		length(in_is_out.who)
		# Final record off, datein == dateout, penultimate != off, no gap
		in_is_out1.who <- unique(jobhist[
			!is.na(I) &
				(studyno %in% in_is_out.who),
			.(who = {
				# Penultimate not off
				off[I == (N - 1)] == 0 &
					# No gap
					dateout[I == (N - 1)] == datein[I == N]
			}
			), by = .(studyno)
			][who == T, studyno])
		length(in_is_out1.who)

		# Last dateout, cap at 10 years ####
		jobhist[studyno %in% c(
			nonum.who,
			in_not_out.who,
			in_is_out1.who), `:=`(
				yout_recode = min(datein[
					# Last record
					!is.na(I) & I == N] %m+% years(10),
					dateout[
						# Last record
						!is.na(I) & I == N])
			), by = .(studyno)]

		# Final off, datein == dateout, yes gap
		in_is_out2.who <- unique(jobhist[
			!is.na(I) &
				studyno %in% in_is_out.who[
					!(in_is_out.who %in% in_is_out1.who)],
			.(who = dateout[I == (N - 1)] != datein[I == N]
			), by = .(studyno)][who == T, studyno])
		length(in_is_out2.who)
		# Penultimate dateout; if off, cap at 10 years ####
		jobhist[studyno %in% in_is_out2.who, `:=`(
			yout_recode = {
				if (off[!is.na(I) & I == (N - 1)] > 0) {
					min(datein[
						# Penultimate record
						!is.na(I) & I == (N - 1)] %m+% years(10),
						dateout[
							# Penultimate record0
							!is.na(I) & I == (N - 1)])
				} else {dateout[
					# Penultimate record
					!is.na(I) & I == (N - 1)]}
			}), by = .(studyno)]

		# Final off, datein == dateout, penultimate off, no gap
		in_is_out3.who <- unique(jobhist[
			!is.na(I) &
				studyno %in% in_is_out.who[
					!(in_is_out.who %in% in_is_out1.who)],
			.(who = (dateout[I == (N - 1)] == datein[I == N]) &
					# Penultimate off
					(off[I == (N - 1)] > 0)
			), by = .(studyno)][who == T, studyno])
		length(in_is_out3.who)

		# dateout of last non-off record ####
		jobhist[studyno %in% in_is_out3.who, `:=`(
			yout_recode = max(dateout[numeric > 0])
		), by = .(studyno)]

		# Final missing or discrepancy ####
		mss_md.who <- jobhist[
			!is.na(I) & I == N & (missing > 0 | discrepancy > 0) & numeric == 0, studyno]

		in_85_3.1.94.who <- unique(dta[
			yout95 > 85 & yout95 < date.to.gm(as.Date("1994-03-01")) &
				studyno %in% mss_md.who, studyno])

		jobhist[studyno %in% in_85_3.1.94.who, `:=`(
			yout_recode = as.Date("1995-01-01")
		)]

		# YOUT95 not in previous, then group by dateout
		out_after_92.who <- unique(dta[
			!(studyno %in% in_85_3.1.94.who) &
				studyno %in% mss_md.who, .(studyno)][studyno %in% jobhist[
				!is.na(I) & I == N & dateout >= as.Date("1992-01-01"), studyno], studyno])

		jobhist[studyno %in% out_after_92.who, `:=`(
			yout_recode = as.Date("1995-01-01")
		)]

		# Final dateout; cap at 10 years
		out_before_92.who <- unique(dta[
			!(studyno %in% in_85_3.1.94.who) &
				studyno %in% mss_md.who, .(studyno)][studyno %in% jobhist[
				!is.na(I) & I == N & dateout < as.Date("1992-01-01"), studyno], studyno])
		jobhist[studyno %in% out_before_92.who, `:=`(
			yout_recode = min(
				datein[!is.na(I) & I == N] %m+% years(10),
				dateout[!is.na(I) & I == N])
		), by = .(studyno)]

		# Final is numeric ####
		# Allow numeric to supercede off/missing/discrepancy (~200 people)
		finalnum.who <- jobhist[!is.na(I) & I == N &  numeric > 0, studyno]
		# Final dateout >= 1985
		out_after_85.who <- jobhist[
			!is.na(I) & I == N & studyno %in% finalnum.who, studyno[year(dateout) >= 1985]
			]
		length(out_after_85.who)
		out_before_85.who <- jobhist[
			!is.na(I) & I == N & studyno %in% finalnum.who, studyno[year(dateout) < 1985]
			]
		length(out_before_85.who)
		# If final dateout >= 92, censor; else last
		jobhist[studyno %in% out_after_85.who |
							studyno %in% out_before_85.who, `:=`(
								yout_recode = {
									if (dateout[!is.na(I) & I == N] >= as.Date("1992-01-01")) {
										as.Date("1995-01-01")
									} else {
										# Last
										dateout[!is.na(I) & I == N]
									}
								}
							), by = .(studyno)]

		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		table(jobhist[dateout.date >= as.Date("1995-01-01"),
									dateout.date])
		table(jobhist[employment_end.date >= as.Date("1995-01-01"),
						employment_end.date])
		table(jobhist[employment_end.date.legacy >= as.Date("1995-01-01"),
						employment_end.date.legacy])
		table(jobhist[yout_recode >= as.Date("1995-01-01"),
						yout_recode])
		table(jobhist[yout_recode >= as.Date("1995-01-01"),
									yout_recode])

		# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		# Make person-year
		jobhist_py <- jobhist[,.(
			year = seq(year(datein), year(dateout)),
			plant,
			assembly,
			grinding,
			machining,
			numeric,
			missing,
			discrepancy,
			off,
			bio,
			cl,
			ea,
			tea,
			trz,
			s,
			no2,
			# Use of unique() slows down runtime
			dateout.date = dateout.date[1],
			employment_end.date = employment_end.date[1],
			employment_end.date.legacy = employment_end.date.legacy[1],
			yout_recode = yout_recode[1]
		), by = .(studyno, datein, dateout)]

		# Calculate years accounted for in each row
		jobhist_py[,`:=`(
			year.start = as.Date(paste0(year, "-01-01")),
			year.end = as.Date(paste0(year + 1, "-01-01")))]
		jobhist_py[,`:=`(
			year.days = time_length(difftime(year.end, year.start), "day"))]

		# Less than 1 year
		jobhist_py[year(datein) == year & year(dateout) == year, `:=`(
			record.days = time_length(difftime(dateout, datein), "day")
		)]
		# greater than one year start
		jobhist_py[year(datein) == year & year(dateout) > year, `:=`(
			record.days = time_length(difftime(year.end, datein), "day")
		)]
		# greater than one year middle
		jobhist_py[year(datein) < year & year(dateout) > year, `:=`(
			record.days = 1
		)]
		# greater than one year end
		jobhist_py[year(datein) < year & year(dateout) == year, `:=`(
			record.days = time_length(difftime(dateout, year.start), "day")
		)]

		# Collapse by year
		jobhist_py <- jobhist_py[, .(
			assembly = sum(assembly * record.days),
			grinding = sum(grinding * record.days),
			machining = sum(machining * record.days),
			numeric = sum(numeric * record.days),
			missing = sum(missing * record.days),
			discrepancy = sum(discrepancy * record.days),
			off = sum(off * record.days),
			bio = sum(bio * record.days),
			cl = sum(cl * record.days),
			ea = sum(ea * record.days),
			tea = sum(tea * record.days),
			trz = sum(trz * record.days),
			s = sum(s * record.days),
			no2 = sum(no2 * record.days),
			dateout.date = dateout.date[1],
			employment_end.date = employment_end.date[1],
			employment_end.date.legacy = employment_end.date.legacy[1],
			yout_recode = yout_recode[1],
			record.days = sum(record.days),
			year.days = year.days[1]
		), by = .(studyno, year, plant)]

		jobhist_py[,`:=`(
			assembly = assembly/year.days,
			grinding = grinding/year.days,
			machining = machining/year.days,
			numeric = numeric/year.days,
			missing = missing/year.days,
			discrepancy = discrepancy/year.days,
			off = off/year.days,
			bio = bio / year.days,
			cl = cl / year.days,
			ea = ea / year.days,
			tea = tea / year.days,
			trz = trz / year.days,
			s = s / year.days,
			no2 = no2 / year.days,
			record.years = record.days/year.days
		)]

		# Cast plant
		jobhist_py.cast <- dcast(jobhist_py,
														 studyno + year ~ plant,
														 value.var = c("assembly",
														 							"grinding",
														 							"machining",
														 							"numeric",
														 							"missing",
														 							"discrepancy",
														 							"off",
														 							"bio",
														 							"cl",
														 							"ea",
														 							"tea",
														 							"trz",
														 							"s",
														 							"no2",
														 							"record.days",
														 							"record.years"))
		jobhist_py.cast[is.na(jobhist_py.cast)] <- 0
		names(jobhist_py.cast) <- gsub("_1", ".gan",
																	 names(jobhist_py.cast))
		names(jobhist_py.cast) <- gsub("_2", ".han",
																	 names(jobhist_py.cast))
		names(jobhist_py.cast) <- gsub("_3", ".san",
																	 names(jobhist_py.cast))

		jobhist <<- jobhist
		jobhist_py <<- jobhist_py
		jobhist_py.cast <<- jobhist_py.cast

		if (save.jobhist) {
			box_save(jobhist_py,
							 dir_id = 113431246688,
							 file_name = "jobhist_py.rdata")
			box_save(jobhist_py.cast,
							 dir_id = 113431246688,
							 file_name = "jobhist_py.cast.rdata")
			box_save(jobhist,
							 dir_id = 113431246688,
							 file_name = "jobhist.rdata")
		}

	} else {
		# jobhist_py
		box_load(536840278296)
		# jobhist_py.cast
		box_load(536832710883)
		# jobhist
		box_load(518505215996)
	}
}

# # New jobhist files ####
# get.jobhist(
# 	save.jobhist = T, rebuild = T
# )

# dta[,yout16.date := gm.to.date(yout16)]
# Comparing youts ####
# tikz("~/Desktop/YOUTs.tex", standAlone = T, width = 5, height = 3)
# print(ggplot(data.frame(), aes(
# 	x = yout,
# 	color = `Which yout`)) +
# 	geom_histogram(
# 		data = jobhist[
# 			studyno %in% dta$studyno, .(
# 				yout = employment_end.date[1],
# 				`Which yout` = "Suzanne"),
# 			by = .(studyno)][year(yout) < 1995],
# 		bins = 1995 - 1938,
# 		aes(fill = `Which yout`),
# 		alpha = 0.5) +
# 	geom_histogram(
# 		data = jobhist[
# 			studyno %in% dta$studyno,.(yout = employment_end.date.legacy[1],
# 																 `Which yout` = "Legacy"),
# 			by = .(studyno)][year(yout) < 1995],
# 		bins = 1995 - 1938,
# 		aes(fill = `Which yout`),
# 		alpha = 0.5) +
# 	# geom_histogram(
# 	# 	data = dta[
# 	# 		studyno %in% jobhist$studyno,.(yout = yout16.date,
# 	# 																	 `Which yout` = "Original"),
# 	# 		by = .(studyno)][year(yout) < 1995],
# 	# 	bins = 1995 - 1938,
# 	# 	aes(fill = `Which yout`),
# 	# 	alpha = 0.5) +
# 		scale_color_viridis(discrete = T) +
# 		scale_fill_viridis(discrete = T) +
# 	mytheme)
# dev.off()