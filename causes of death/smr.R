# Standardized rate ratios ####
# Kevin Chen
# July 12, 2019

# rm(list = ls()[-which(ls() == 'cohort')])
library(here)

# Source 00-hello.R and lifetable.R
if (!('cohort' %in% ls())) {
	source(here::here('wrangling', '00-hello.R'))
	# Remove people with unknown cause of death
	cohort <- cohort[!(status15 == 6 & (is.na(icd)))]
}

# SMR function ####
smr <-
	function(outcome = 'Lung cancer',
					 obs = as.data.frame(ltab_obs),
					 ref = as.data.frame(ltab),
					 detail = T,
					 significance.level = 0.95,
					 verbose = F) {

		obs <- as.data.table(as.data.frame(obs))
		ref <- as.data.table(as.data.frame(ref))


		if (grepl("Prosta", outcome)) {
			obs <- obs[sex == "M"]
			ref <- ref[sex == "M"]
		}

		if (grepl("Breast", outcome)) {
			obs <- obs[sex == "F"]
			ref <- ref[sex == "F"]
		}


		if (!is.data.table(obs)) {
			message('Please provide the analytic dataset as an object of class data.table')
		}

		if (verbose) {
			message(paste0("Computing SMR for '", outcome, "'"))
		}


		obs <- obs[,
							 .(
							 	count = sum(get(outcome)),
							 	rate = sum(get(outcome)) / sum(py),
							 	py = sum(py)
							 ),
							 by = .(sex,
							 			 race,
							 			 calendar,
							 			 age)]
		exp <- ref[cod == outcome]
		exp <- exp[obs, .(
			sex,
			race,
			calendar,
			age,
			ref.rate = rate,
			obs.py = py,
			exp.count = rate * py,
			count = count
		),
		on = .(race, sex, age, calendar)]

		excluded <- exp[is.na(exp.count)]
		exp <- exp[!is.na(exp.count)]

		smr <- sum(exp$count) / sum(exp$exp.count)
		if (sum(exp$count) == 0) {
			message(paste0("No observed deaths for '", outcome, "'"))
		}
		# Assuming A_i ~ Pois(A_i/(T_i^2))
		# Whether you assume population estimates makes a small
		# difference in the point estimates of the variance
		lsmr.var <-	1 / sum(exp$count) #+ 1 / sum(exp$exp.count)
		lsmr.se  <- sqrt(lsmr.var)
		smr.lower2.5 <-
			exp(log(smr) + qnorm((1 - significance.level) / 2) * lsmr.se)
		smr.upper2.5 <-
			exp(log(smr) - qnorm((1 - significance.level) / 2) * lsmr.se)

		# smd <- sum(exp$count) - sum(exp$exp.count)
		#
		# smd.var <- sum(exp$count) / sum(exp$obs.py) ^ 2 +
		# 	sum(exp$exp.count) / sum(exp$obs.py) ^ 2
		#
		# smd.se  <- sqrt(smd.var)
		# smd.lower2.5 <-
		# 	smd + qnorm((1 - significance.level) / 2) * smd.se
		# smd.upper2.5 <-
		# 	smd - qnorm((1 - significance.level) / 2) * smd.se

		N <- sum(exp$count)

		output <- list(
			observed = obs,
			expected = exp,
			excluded = excluded,

			smr = smr,
			lsmr.se = lsmr.se,
			smr.lower2.5 = smr.lower2.5,
			smr.upper2.5 = smr.upper2.5,

			# smd = smd,
			# smd.se = smd.se,
			# smd.lower2.5 = smd.lower2.5,
			# smd.upper2.5 = smd.upper2.5,

			N = N
		)

		if (!detail) {
			output[-(1:3)]
		}
		return(output)
	}

# Monson 1986-style startifications ####
by_followup <- function(outcome = 'All causes',
												lower = c(0, seq(10, 70, by = 10)),
												upper = c(seq(10, 70, by = 10), Inf),
												data = as.data.frame(ltab_obs),
												ref = as.data.frame(ltab),
												total = T) {
	data <- as.data.table(as.data.frame(data))

	smr.tab <- c()
	N <- c()
	smr.lower2.5 <- c()
	smr.upper2.5 <- c()

	if (total) {
		lower[length(lower) + 1] <- -Inf
		upper[length(upper) + 1] <- Inf
	}

	tmp.smr <- list()
	for (i in 1:length(lower)) {
		tmp.smr <-
			smr(outcome, data[(year - year(start)) >= lower[i]  &
													(year - year(start)) < upper[i]],
					ref = ref)
		smr.tab[i] <- tmp.smr$smr
		N[i] <- tmp.smr$N
		smr.lower2.5[i] <- tmp.smr$smr.lower2.5
		smr.upper2.5[i] <- tmp.smr$smr.upper2.5
	}

	rm(tmp.smr)

	bounds <- paste0('[', lower, ', ',  upper, ')')
	if (!total) {
		bounds[length(bounds)] <- paste0(lower[length(bounds)], '+')
	} else {
		bounds[length(bounds) - 1] <- paste0(lower[length(bounds) - 1], '+')
		bounds[length(bounds)] <- "Total"
	}

	return(
		list(
			bounds = bounds,
			smr = smr.tab,
			N   = N,
			smr.lower2.5 = smr.lower2.5,
			smr.upper2.5 = smr.upper2.5
		)
	)

}

by_entryage <- function(outcome = 'All causes',
												lower = c(-Inf, seq(10, 60, by = 10)),
												upper = c(seq(10, 60, by = 10), Inf),
												data = as.data.frame(ltab_obs),
												ref = as.data.frame(ltab),
												total = T) {
	data <- as.data.table(as.data.frame(data))

	smr.tab <- c()
	N <- c()
	smr.lower2.5 <- c()
	smr.upper2.5 <- c()


	if (total) {
		lower[length(lower) + 1] <- -Inf
		upper[length(upper) + 1] <- Inf
	}

	tmp.smr <- list()
	for (i in 1:length(lower)) {
		tmp.smr <-
			smr(outcome, data[(year(start) - year(yob)) >= lower[i]  &
													(year(start) - year(yob)) < upper[i]],
					ref = ref)
		smr.tab[i] <- tmp.smr$smr
		N[i] <- tmp.smr$N
		smr.lower2.5[i] <- tmp.smr$smr.lower2.5
		smr.upper2.5[i] <- tmp.smr$smr.upper2.5
	}

	rm(tmp.smr)

	bounds <- paste0('[', lower, ', ',  upper, ')')
	if (!total) {
		bounds[length(bounds)] <- paste0(lower[length(bounds)], '+')
	} else {
		bounds[length(bounds) - 1] <-  paste0(lower[length(bounds) - 1], '+')
		bounds[length(bounds)] <- 'Total'
	}
	return(
		list(
			bound = bounds,
			smr = smr.tab,
			N   = N,
			smr.lower2.5 = smr.lower2.5,
			smr.upper2.5 = smr.upper2.5
		)
	)

}

by_entryyear <- function(outcome = 'All causes',
												 lower = c(seq(1940, 1975, by = 5)),
												 upper = c(seq(1945, 1975, by = 5), Inf),
												 data = as.data.frame(ltab_obs),
												 ref = as.data.frame(ltab),
												 total = T) {
	data <- as.data.table(as.data.frame(data))

	smr.tab <- c()
	N <- c()
	smr.lower2.5 <- c()
	smr.upper2.5 <- c()


	if (total) {
		lower[length(lower) + 1] <- -Inf
		upper[length(upper) + 1] <- Inf
	}

	tmp.smr <- list()
	for (i in 1:length(lower)) {
		tmp.smr <-
			smr(outcome, data[year(yin16) >= lower[i]  &
													year(yin16) < upper[i]],
					ref = ref)
		smr.tab[i] <- tmp.smr$smr
		N[i] <- tmp.smr$N
		smr.lower2.5[i] <- tmp.smr$smr.lower2.5
		smr.upper2.5[i] <- tmp.smr$smr.upper2.5
	}

	rm(tmp.smr)

	bounds <- paste0(lower, ' to ',  upper - 1)
	if (!total) {
		bounds[length(bounds)] <- paste0(lower[length(bounds)], ' onwards')
	} else {
		bounds[length(bounds) - 1] <-
			paste0(lower[length(bounds) - 1], ' onwards')
		bounds[length(bounds)] <- 'Total'
	}
	return(
		list(
			bound = bounds,
			smr = smr.tab,
			N   = N,
			smr.lower2.5 = smr.lower2.5,
			smr.upper2.5 = smr.upper2.5
		)
	)

}

by_calendar <- function(outcome = 'All causes',
												lower = c(seq(1940, 2010, by = 10)),
												upper = c(seq(1950, 2010, by = 10), Inf),
												data = as.data.frame(ltab_obs),
												ref = as.data.frame(ltab),
												total = T) {
	data <- as.data.table(as.data.frame(data))

	smr.tab <- c()
	N <- c()
	smr.lower2.5 <- c()
	smr.upper2.5 <- c()


	if (total) {
		lower[length(lower) + 1] <- -Inf
		upper[length(upper) + 1] <- Inf
	}

	tmp.smr <- list()
	for (i in 1:length(lower)) {
		tmp.smr <-
			smr(outcome, data[year >= lower[i]  &	year < upper[i]],
					ref = ref)
		smr.tab[i] <- tmp.smr$smr
		N[i] <- tmp.smr$N
		smr.lower2.5[i] <- tmp.smr$smr.lower2.5
		smr.upper2.5[i] <- tmp.smr$smr.upper2.5
	}

	rm(tmp.smr)

	bounds <- paste0(lower, ' to ',  upper - 1)
	if (!total) {
		bounds[length(bounds)] <- paste0(lower[length(bounds)], ' onwards')
	} else {
		bounds[length(bounds) - 1] <-
			paste0(lower[length(bounds) - 1], ' onwards')
		bounds[length(bounds)] <- 'Total'
	}
	return(
		list(
			bound = bounds,
			smr = smr.tab,
			N   = N,
			smr.lower2.5 = smr.lower2.5,
			smr.upper2.5 = smr.upper2.5
		)
	)

}

by_followup_age <- function(outcome = 'All causes',
														lower.age = c(0, seq(20, 60, by = 10)),
														upper.age = c(seq(20, 60, by = 10), Inf),
														lower.followup = c(0, seq(15, 75, by = 15)),
														upper.followup = c(seq(15, 75, by = 15), Inf),
														data = as.data.frame(ltab_obs),
														ref = as.data.frame(ltab),
														total = T) {
	data <- as.data.table(as.data.frame(data))
	ref  <- as.data.table(as.data.frame(ref))

	if (total) {
		lower.age[length(lower.age) + 1] <- -Inf
		lower.followup[length(lower.followup) + 1]  <- -Inf
		upper.age[length(upper.age) + 1] <- Inf
		upper.followup[length(upper.followup) + 1] <- Inf
	}

	smr.tab <- as.data.frame(matrix(
		NA,
		nrow = length(lower.age),
		ncol = length(lower.followup)
	))

	rownames(smr.tab) <- paste0('[', lower.age, ', ',  upper.age, ')')
	if (!total) {
		rownames(smr.tab)[nrow(smr.tab)] <-
			paste0(lower.age[nrow(smr.tab)], '+')
	} else {
		rownames(smr.tab)[nrow(smr.tab) - 1] <-
			paste0(lower.age[nrow(smr.tab) - 1], '+')
		rownames(smr.tab)[nrow(smr.tab)] <- "Total"
	}

	colnames(smr.tab) <-
		paste0('[', lower.followup, ', ',  upper.followup, ')')
	if (!total) {
		colnames(smr.tab)[ncol(smr.tab)] <-
			paste0(lower.followup[ncol(smr.tab)], '+')
	} else {
		colnames(smr.tab)[ncol(smr.tab) - 1] <-
			paste0(lower.followup[ncol(smr.tab) - 1], '+')
		colnames(smr.tab)[ncol(smr.tab)] <- "Total"
	}

	N <- smr.lower2.5 <- smr.upper2.5 <- smr.tab

	tmp.smr <- list()
	for (i in 1:length(lower.age)) {
		tmp.smr <- by_followup(
			outcome = outcome,
			lower = lower.followup,
			upper = upper.followup,
			data = data[(year(start) - year(yob)) >= lower.age[i]  &
										(year(start) - year(yob)) < upper.age[i]],
			total = F,
			ref = ref
		)
		smr.tab[i, 1:ncol(smr.tab)] <- tmp.smr$smr
		N[i,] <- tmp.smr$N
		smr.lower2.5[i,] <- tmp.smr$smr.lower2.5
		smr.upper2.5[i,] <- tmp.smr$smr.upper2.5
	}

	rm(tmp.smr)

	return(list(
		smr = smr.tab,
		N   = N,
		smr.lower2.5 = smr.lower2.5,
		smr.upper2.5 = smr.upper2.5
	))
}

by_followup_year <- function(outcome = 'All causes',
														 lower.year = c(seq(1940, 1975, by = 5)),
														 upper.year = c(seq(1944, 1974, by = 5), Inf),
														 lower.followup = c(0, seq(15, 75, by = 15)),
														 upper.followup = c(seq(15, 75, by = 15), Inf),
														 data = as.data.frame(ltab_obs),
														 ref = as.data.frame(ltab),
														 total = T) {
	data <- as.data.table(as.data.frame(data))
	ref <- as.data.table(as.data.frame(ref))

	if (total) {
		lower.year[length(lower.year) + 1] <- -Inf
		lower.followup[length(lower.followup) + 1]  <- -Inf
		upper.year[length(upper.year) + 1] <- Inf
		upper.followup[length(upper.followup) + 1] <- Inf
	}

	# Initialize output
	smr.tab <- as.data.frame(matrix(
		NA,
		nrow = length(lower.year),
		ncol = length(lower.followup)
	))

	# Output rownames
	rownames(smr.tab) <- paste0(lower.year, ' to ',  upper.year)
	if (!total) {
		rownames(smr.tab)[nrow(smr.tab)] <-
			paste0(lower.year[nrow(smr.tab)], ' onwards')
	} else {
		rownames(smr.tab)[nrow(smr.tab) - 1] <-
			paste0(lower.year[nrow(smr.tab) - 1], ' onwards')
		rownames(smr.tab)[nrow(smr.tab)] <- "Total"
	}

	# Output colnames
	colnames(smr.tab) <-
		paste0('[', lower.followup, ', ',  upper.followup, ')')
	if (!total) {
		colnames(smr.tab)[ncol(smr.tab)] <-
			paste0(lower.followup[ncol(smr.tab)], '+')
	} else {
		colnames(smr.tab)[ncol(smr.tab) - 1] <-
			paste0(lower.followup[ncol(smr.tab) - 1], '+')
		colnames(smr.tab)[ncol(smr.tab)] <- "Total"
	}

	# Other stats, same output shape
	N <- smr.lower2.5 <- smr.upper2.5 <- smr.tab

	tmp.smr <- list()
	for (i in 1:length(lower.year)) {
		tmp.smr <- by_followup(
			outcome = outcome,
			lower = lower.followup,
			upper = upper.followup,
			data = data[year(yrin16) >= lower.year[i] &
										year(yrin16) < upper.year[i]],
			total = F,
			ref = ref
		)
		smr.tab[i, 1:ncol(smr.tab)] <- tmp.smr$smr
		N[i,] <- tmp.smr$N
		smr.lower2.5[i,] <- tmp.smr$smr.lower2.5
		smr.upper2.5[i,] <- tmp.smr$smr.upper2.5
	}

	rm(tmp.smr)

	return(list(
		smr = smr.tab,
		N   = N,
		smr.lower2.5 = smr.lower2.5,
		smr.upper2.5 = smr.upper2.5
	))
}
# Sanity checks
# smr('All causes')$smr
# smr('All natural causes')$smr
# smr('All causes', obs = ltab_obs[year <= 2009], verbose = T)$smr
# smr('All natural causes', obs = ltab_obs[year <= 2009], verbose = T)$smr
#
# smr('All causes', obs = ltab_obs[year > 2009], verbose = T)$smr
# smr('All natural causes', obs = ltab_obs[year > 2009], verbose = T)$smr

# smr('All CVD', obs = ltab_obs[year <= 2009])$smr
# smr('All CVD', obs = ltab_obs[year > 2009])$smr
#
# smr('Esophageal cancer')$smr
# smr('Esophageal cancer', obs = ltab_obs[year <= 2009])$smr
# smr('Esophageal cancer', obs = ltab_obs[year > 2009])$smr
