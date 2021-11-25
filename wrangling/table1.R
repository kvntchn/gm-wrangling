# Table 1 ####
# 9/6/2019
# Requires cohort_analytic

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

get.tab1 <- function(
	df = as.data.table(as.data.frame(cohort_analytic)),
	timescale = "work",
	exposure_lag = 21,
	# save_tab1 = T,
	table_engine = "xtable",
	since_leavework = F,
	use_finrace = T,
	incidence = F,
	py = "py",
	mathmode = T,
	nrow_as_fu = F) {

	df <- as.data.table(as.data.frame(df))

	setorder(df, studyno, year)
	# Individual-level summary
	tab1.sum <- df[,.(
		'Race' = NA,
		'\\hspace{10pt}White' = if (use_finrace) {
			ifelse(finrace[1] == 1, 1, 0)} else {
				as.numeric(race[1] == "White")
			},
		'\\hspace{10pt}Black' = if (use_finrace) {
			ifelse(finrace[1] == 2, 1, 0)} else {
				as.numeric(race[1] == "Black")
			},
		'\\hspace{10pt}Unknown' = if (use_finrace) {
			ifelse(finrace[1] %in% c(0, 9), 1, 0)} else {
				as.numeric(race[1] == "Unknown")
			},
		'Sex' = NA,
		'\\hspace{10pt}Male' = ifelse(sex[1] == 'M', 1, 0),
		'\\hspace{10pt}Female' = ifelse(sex[1] == 'F', 1, 0),
		"Plant$^1$" = NA,
		"\\hspace{10pt}Plant 1" = as.numeric(names(sort(table(plant[year < 1995]), decreasing = T)[1])) == 1,
		"\\hspace{10pt}Plant 2" = as.numeric(names(sort(table(plant[year < 1995]), decreasing = T)[1])) == 2,
		"\\hspace{10pt}Plant 3" = as.numeric(names(sort(table(plant[year < 1995]), decreasing = T)[1])) == 3,
		"Ever exposed to MWFs" = NA,
		# as.numeric(cum_soluble[.N] + cum_straight[.N] + cum_synthetic[.N] > 0)
		"\\hspace{10pt}Straight"  = as.numeric(cum_straight[.N] > 0),
		"\\hspace{10pt}Soluble"   = as.numeric(cum_soluble[.N] > 0),
		"\\hspace{10pt}Synthetic" = as.numeric(cum_synthetic[.N] > 0),
		"Employment status in 1995" = NA,
		"\\hspace{10pt}Left work" = as.numeric(jobloss.date[1] < as.Date("1995-01-01")),
		"\\hspace{10pt}Still at work" = as.numeric(jobloss.date[1] >= as.Date("1995-01-01")),
		"Deceased by end of follow-up" = {
			if (!incidence) {
				as.numeric(!is.na(yod[1]) & yod[1] <= as.Date(paste0(year[.N], "-12-31")))
			} else {
				as.numeric(!is.na(ddiag_first[1]) & ddiag_first[1] <= as.Date(paste0(year[.N], "-12-31")))
			}
		},
		# "\\hspace{10pt}ALD" = as.numeric(max(`Alcohol-related Liver Disease`) > 0),
		# "\\hspace{10pt}Suicide" = as.numeric(max(Suicide) > 0),
		# "\\hspace{10pt}Overdose" = as.numeric(max(Overdose) > 0),


		# Years since follow-up
		"\\hline Years of follow-up" = if (nrow_as_fu) {.N} else {
				time_length(difftime(
					min(yod[1],
							yoc[1],
							as.Date(paste0(max(year), "-12-31")), na.rm = T),
					min(as.Date(paste0(min(year[immortal == 0]), "-01-01")))), 'years')
				},
		'Year of birth' = yob.gm[1],
		'Year of hire' = yin.gm[1],
		'Age at hire (years)' = yin.gm[1] - yob.gm[1],
		'Year of leaving work$^2$' = date.to.gm(jobloss.date)[1],
		'Age at leaving work (years)$^2$' = date.to.gm(jobloss.date)[1] - yob.gm[1],
		"Years at work$^2$" = {
			if (year(jobloss.date[1]) == 1995) {
				NaN} else {time_length(
					difftime(jobloss.date[1], yin[1]), 'years'
				)}},
		'Year of death among deceased' = {
			if (!incidence) {
				as.numeric(unique(year(yod)))} else {
					as.numeric(unique(year(ddiag_first)))
				}},
		'Age at death (years) among deceased' = {
			if (!incidence) {
				time_length(difftime(unique(yod), unique(yob)), 'years')} else {
					time_length(difftime(unique(ddiag_first), unique(yob)), 'years')
				}},
		# "Maximum cumulative exposure$^3$ (mg/m$^3\\cdot$y)}" =
		# 	NA,
		# 	max(cum_soluble[cum_soluble != 0]) +
		# 	  max(cum_straight[cum_straight != 0]) +
		#   	max(cum_synthetic[cum_synthetic != 0])

		"Cumulative exposure$^3$ to MWFs (mg/m$^3\\cdot$y)" = NA,

		"\\hspace{10pt}Straight "  = as.numeric(ifelse(
			sum(cum_straight != 0) > 0,
			max(cum_straight[cum_straight != 0], na.rm = T),
			NA)),

		"\\hspace{10pt}Soluble "   = as.numeric(ifelse(
			sum(cum_soluble != 0) > 0,
			max(cum_soluble[cum_soluble != 0], na.rm = T),
			NA)),

		"\\hspace{10pt}Synthetic " = as.numeric(ifelse(
			sum(cum_synthetic != 0) > 0,
			max(cum_synthetic[cum_synthetic != 0], na.rm = T),
			NA))

	),
	by = .(studyno)][,-'studyno']

	# Correct names if looking at incidence outcome
	if (incidence) {
		names(tab1.sum) <- gsub(
			"Deceased by end of follow-up",
			"Diagnosed with cancer by end of follow-up",
			names(tab1.sum))
		names(tab1.sum) <- gsub(
			"Year of death among deceased",
			"Year of first cancer diagnosis",
			names(tab1.sum))
		names(tab1.sum) <- gsub(
			"Age at death \\(years\\) among deceased",
			"Age at first cancer diagnosis (years)",
			names(tab1.sum))
		tab1.sum <- tab1.sum[,-c(
			"\\hspace{10pt}ALD",
			"\\hspace{10pt}Suicide",
			"\\hspace{10pt}Overdose"), with = F]
	}

	# Population-level summary
	table.break <- "\\hline Years of follow-"
	tab1 <- rbind(
		t(apply(tab1.sum[,1:(grep(table.break, names(tab1.sum)) - 1)], 2, function(x) {
			return(
				c(mean(x, na.rm = T) * sum(!is.na(x)),
					mean(x, na.rm = T) * 100,
					NA
					# NA,
					# NA
				)
			)
		})),
		t(apply(tab1.sum[,grep(table.break, names(tab1.sum)):ncol(tab1.sum)], 2,
						function(x) {
							return(
								c(
									# mean(x, na.rm = T),
									# sd(x, na.rm = T)
									# min(x, na.rm = T),
									median(as.numeric(x), na.rm = T),
									quantile(as.numeric(x), 0.25, na.rm = T),
									quantile(as.numeric(x), 0.75, na.rm = T)
									# max(x, na.rm = T)
								)
							)
						}))
	)

	tab1[!is.finite(as.matrix(tab1))] <- NA

	# Table names
	colnames(tab1) <- c('center', 'spread', " ")#, 'Minimum', 'Median', 'Maximum')

	# Digits
	tab1.digits <- matrix(2, ncol = 3,#5
												nrow = nrow(tab1))
	tab1.digits[grep("Age", rownames(tab1), ignore.case = T), ] <- 1
	tab1.digits[grep("years ", rownames(tab1), ignore.case = T), ] <- 1
	tab1.digits[grep("year ", rownames(tab1), ignore.case = T), ] <- 0
	tab1.digits[1:(grep(table.break, rownames(tab1), ignore.case = T) - 1), 1] <- 0
	tab1.digits[1:(grep(table.break, rownames(tab1), ignore.case = T) - 1), -1] <- 0

	# more digits if small %s
	tab1.digits[1:nrow(tab1.digits) < grep(table.break, rownames(tab1), ignore.case = T) &
								(tab1[,2] < 2 & tab1[,2] != 0), -1] <- 2

	tab1.digits[1:nrow(tab1.digits) < grep(table.break, rownames(tab1), ignore.case = T) &
								(tab1[,2] < 1 & tab1[,2] != 0), -1] <- 2

	tab1.digits[1:nrow(tab1.digits) < grep(table.break, rownames(tab1), ignore.case = T) &
								(tab1[,2] > 99 & tab1[,2] != 100), -1] <- 1

	# more digits if numbers
	tab1.digits[1:nrow(tab1.digits) >= grep(table.break, rownames(tab1), ignore.case = T) &
								(tab1[,2] < .1 & tab1[,2] != 0), -1] <- 2

	tab1.digits[1:nrow(tab1.digits) >= grep(table.break, rownames(tab1), ignore.case = T) &
								(tab1[,2] == 0), -1] <- 0

	tab1.which_year <- matrix(F, ncol = 3, nrow = nrow(tab1))
	tab1.which_year[grep("year ", rownames(tab1), ignore.case = T), ] <- T

	tab1 <- matrix(
		sapply(1:length(tab1), function(i) {
			if (is.na(as.vector(tab1)[i])) {NA} else {
			formatC(as.vector(tab1)[i], as.vector(tab1.digits)[i], format = "f",
							big.mark = if (!i %in% which(as.vector(tab1.which_year))) {
								if (mathmode) {"\\\\,"} else {","}} else {""})
		}}),
		ncol = ncol(tab1),
		nrow = nrow(tab1),
		dimnames = dimnames(tab1)
	)

	# Pretty percents
	tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2] <- sapply(
		tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2],
		function (x) {
			if (!is.na(x)) {
				paste0(x, '\\%')
			} else {NA}
		})

	# Math mode
	if (mathmode) {
	tab1 <- matrix(
		sapply(1:length(tab1), function(i) {
			if (!is.na(as.vector(tab1)[i])) {
				paste0('$', as.vector(tab1)[i], '$')
			} else {NA}
		}),
		ncol = ncol(tab1),
		nrow = nrow(tab1),
		dimnames = dimnames(tab1))
	}

	tab1 <- rbind(
		matrix(c(
			paste0(
				if (mathmode) {"$"} else {""},
				prettyNum(n_distinct(df$studyno),
									ifelse(mathmode, '\\\\,', ",")),
				if (mathmode) {"$"} else {""}),
			paste0(
				if (mathmode) {"$"} else {""},
				formatC(ifelse(nrow_as_fu, nrow(df), sum(df[,py])),
								format = "f", digits = 0,
								big.mark = if (mathmode) {"\\\\,"} else {","}),
				if (mathmode) {"$"} else {""}),
			NA
			# , NA, NA
		),
		nrow = 1,
		dimnames = list(c("Study population size (person-years)"), NULL)),
		tab1)

	# Make column indicating stat type
	tab1 <- cbind(tab1, spread.which = c(
		rep(paste0(if (mathmode) {"$"} else {""}, "n", if (mathmode) {"$"} else {""}, "\\%"),
				grep("\\hline", rownames(tab1)) - 1),
		rep("median, Q1, Q3", nrow(tab1) - grep("\\hline", rownames(tab1)) + 1))
	)

	# Clean up rownames
	rownames(tab1)[duplicated(rownames(tab1))] <- paste0(rownames(tab1)[duplicated(rownames(tab1))], " ")
	tab1 <- as.data.frame(tab1, make.names = F)

	# if (save_tab1) {
	# saveRDS(tab1,
	# 	 file = to_drive_D(
	# 	 	here::here(paste0('reports/paper/resources/lag ', exposure_lag),
	# 	 						 paste0('lag', gsub(" ", "", exposure_lag), ".tab1.rds"))))
	# 	}

	print(matrix(as.vector(sapply(tab1[,1:3], function(x) gsub("\\$|\\\\", "", x))),
				 ncol = 3,
				 dimnames = list(rownames(tab1), colnames(tab1)[1:3])))

	return(tab1)
}

render.tab1 <- function(
	tab1,
	tab1.cap = NULL,
	table_engine = 'xtable',
	space_for_comma = T,
	exposure_lag = 21,
	df = as.data.table(as.data.frame(cohort_analytic[
		immortal == 0 &
			nohist == 0 &
			wh == 1 &
			right.censored == 0])),
	table.break = "Years of follow-",
	description.width = 7,
	column.width = 1.5,
	table.break.header = paste0('\\hline ', '& Median & 25\\textsuperscript{th} \\%tile & 75\\textsuperscript{th} \\%tile \\\\ \n'),
	table.align = NULL,
	notes = T,
	...,
	return_table = F) {

	if (is.null(tab1.cap)) {
		tab1.cap <- paste0(
			'Summary of study population characteristics ($N =',
			prettyNum(n_distinct(df$studyno), ifelse(space_for_comma, '\\\\,', ",")),
			'$; $',
			round(nrow(df) / 10 ^ 6, 2),
			'$ million person-years). The cohort was restricted to individuals who were hired in or after ',
			year(min(df$yin)),
			' and for whom at least half of their work history data was available. Individuals were considered lost to follow-up once they reach the maximum observed age at death.'
		)
	}

	if (return_table) {
		if (table_engine == 'pander') {
			# Remove/replace LaTeX commands
			rownames(tab1) <- gsub(
				"\\\\hspace\\{.*\\}", "&#9;", rownames(tab1))
			rownames(tab1) <- gsub(
				"\\\\hline ", "", rownames(tab1))
			# Footnotes
			# rownames(tab1) <- gsub(
			# 	"\\$\\^\\\\flat\\$", "$^1$", rownames(tab1))
			rownames(tab1) <- gsub(
				"\\$\\^\\\\sharp\\$", "$^3$", rownames(tab1))
			rownames(tab1) <- gsub(
				"\\$\\^\\*\\$", "$^2$", rownames(tab1))
			rownames(tab1) <- gsub(
				"\\$\\^\\\\natural\\$", "$^1$", rownames(tab1))

			tab1 <- rbind(
				tab1[1:(which(grepl(table.break, rownames(tab1))) - 1),],
				matrix(c("Median", "Q1", "Q3", NA), T),
				tab1[(which(grepl(table.break, rownames(tab1)))):nrow(tab1),]
				)

			rownames(tab1)[grep("^Median$", tab1[,1])] <- "&nbsp;"
		}
		return(as.data.frame(tab1, make.names = F))
	} else {

		if (table_engine == 'xtable') {
			tab1 %>% as.data.frame(make.names = F) %>% xtable(
				label = "tab1.tab",
				align = {if (is.null(table.align)) {
					paste0('p{', description.width, 'cm}',
								 paste0(rep(
								 	paste0('R{', column.width, 'cm}'), ncol(.)
								 ),
								 collapse = ''))
				} else {table.align}},
				caption = if (nchar(tab1.cap) > 0) {tab1.cap} else {NULL}
			) %>% print(
				comment = F,
				include.rownames = T,
				add.to.row = list(
					pos = {
						if (!is.null(table.break.header)) {
							list(grep(table.break, rownames(tab1)) - 1, nrow(.))
						} else {list(nrow(.))}},
					command = c(
						table.break.header,
						if (notes) {paste0(
							'\\hline ',
							'\\multicolumn{',
							ncol(.) + 1,
							'}{p{',
							description.width + ncol(tab1) * column.width + 1,
							'cm}}{\\footnotesize{',
							'$^1$ For individuals who worked at several plants, plant was taken to be the site where they accrued the most work record time.',
							'}}\\\\',
							'\\multicolumn{',
							ncol(.) + 1,
							'}{p{',
							description.width + ncol(tab1) * column.width + 1,
							'cm}}{\\footnotesize{',
							'$^2$ Among those with known date of worker exit.',
							'}}\\\\',
							'\\multicolumn{',
							ncol(.) + 1,
							'}{p{',
							description.width + ncol(tab1) * column.width + 1,
							'cm}}{\\footnotesize{',
							'$^3$ Summary statistics calculated for ever-exposed individuals at end of follow-up only. Exposures were lagged ', exposure_lag, ' year', ifelse(exposure_lag != 1, 's', ''), '.',
							'}}\\\\'
						)}
					)
				)
				# ...
			) # end print table
		}

		if (table_engine == 'pander') {
			pander(tab1,
						 justify = {if (is.null(table.align)) {
						 	c('left', 'right', 'right', 'right')
						 } else {table.align}},
						 emphasize.rownames = F,
						 missing = "&nbsp;",
						 caption = tab1.cap
			) %>% cat

			if (notes) {
				paste0(
					'^1^ For individuals who worked at several plants, plant was taken to be the site where they accrued the most work record time.\n\n',
					'^2^ Among those with known date of worker exit.\n\n',
					'^3^ Summary statistics calculated for exposed individuals at end of follow-up only. Exposures were lagged ', exposure_lag, ' years\n\n') %>% cat}
		}
	}
}

get.ips_tab1 <- function(
	df,
	timescale = "work",
	save_tab1 = T,
	table_engine = "xtable",
	since_leavework = F,
	use_finrace = T,
	mathmode = T,
	nrow_as_fu = F) {

	df <- as.data.table(as.data.frame(df[filler == 0]))

	setorder(df, studyno, year)

	df[,`:=`(
		yin = as.Date(yin, origin = '1970-01-01'),
		yob = as.Date(yob, origin = '1970-01-01'),
		yod = as.Date(yod, origin = '1970-01-01'),
		jobloss.date = as.Date(jobloss.date, origin = '1970-01-01')
	)]

	# Individual-level summary
	tab1.sum <- df[,.(

		'\\textbf{Race}, $n$ (\\%)' = NA,
		'\\hspace{10pt}White' = if (use_finrace) {
			ifelse(finrace[1] == 1, 1, 0)} else {
				ifelse(finrace[1] == 1 | finrace[1] == 9, 1, 0)
			},
		'\\hspace{10pt}Black' = ifelse(finrace[1] == 2, 1, 0),
		'\\hspace{10pt}Unknown' = if (use_finrace) {
			ifelse(finrace[1] == 9, 1, 0)},
		'\\textbf{Sex}, $n$ (\\%)' = NA,
		'\\hspace{10pt}Male' = ifelse(sex[1] == 'M', 1, 0),
		'\\hspace{10pt}Female' = ifelse(sex[1] == 'F', 1, 0),
		"\\textbf{Plant}$^1$, $n$ (\\%)" = NA,
		"\\hspace{10pt}Plant 1" = as.numeric(names(sort(table(plant[year < 1995]), decreasing = T)[1])) == 1,
		"\\hspace{10pt}Plant 2" = as.numeric(names(sort(table(plant[year < 1995]), decreasing = T)[1])) == 2,
		"\\hspace{10pt}Plant 3" = as.numeric(names(sort(table(plant[year < 1995]), decreasing = T)[1])) == 3,
		'\\textbf{Complete work records}' = as.numeric(
			year(jobloss.date[1]) < 1995),
		"\\textbf{Deceased by end of follow-up}" = as.numeric(!is.na(yod) & yod <= as.Date(paste0(year, "-12-31"))),

		# Years since follow-up
		"\\textbf{Years of follow-up}" =  if (nrow_as_fu) {.N} else {
			if (since_leavework) {
				time_length(
					difftime(min(yod[1], yoc[1], na.rm = T), jobloss.date[1]), 'years') } else {
						time_length(
							difftime(min(yod[1], yoc[1], as.Date("2017-01-01"), na.rm = T), max(yin[1] + 3, as.Date("1941-01-01"))), 'years')
					}
		},
		# "\\textbf{Years at work}" = {if (since_leavework) {
		# 	time_length(difftime(jobloss.date[1], yin[1]), 'year')}},
		'\\textbf{Year of hire}' = yin.gm[1],
		'\\textbf{Age at hire}' = yin.gm[1] - yob.gm[1],
		'\\textbf{Year of birth}' = yob.gm[1],
		'\\textbf{Year of worker exit}' = year(jobloss.date[1]),
		'\\textbf{Age at worker exit}' = time_length(difftime(jobloss.date[1],	yob[1]), 'years'),
		'\\textbf{Age at death among deceased}' = time_length(difftime(
			yod[1], yob[1]), 'years'),
		'\\textbf{Year of death among deceased}' =
			year(yod[1])
	),
	by = .(studyno)][,-'studyno']

	# Population-level summary
	table.break <- "Years of follow-"
	tab1 <- rbind(
		t(apply(tab1.sum[,1:(grep(table.break, names(tab1.sum)) - 1)], 2, function(x) {
			return(
				c(mean(x, na.rm = T) * sum(!is.na(x)),
					mean(x, na.rm = T) * 100,
					NA
					# NA,
					# NA
				)
			)
		})),
		t(apply(tab1.sum[,grep(table.break, names(tab1.sum)):ncol(tab1.sum)], 2,
						function(x) {
							return(
								c(
									# mean(x, na.rm = T),
									# sd(x, na.rm = T)
									# min(x, na.rm = T),
									median(as.numeric(x), na.rm = T),
									quantile(as.numeric(x), 0.25, na.rm = T),
									quantile(as.numeric(x), 0.75, na.rm = T)
									# max(x, na.rm = T)
								)
							)
						}))
	)

	tab1[!is.finite(as.matrix(tab1))] <- NA

	# Table names
	colnames(tab1) <- c('n', '%', "temp")#, 'Minimum', 'Median', 'Maximum')

	# Digits
	tab1.digits <- matrix(2, ncol = 3,#5
												nrow = nrow(tab1))
	tab1.digits[grep("Age", rownames(tab1), ignore.case = T), ] <- 0
	tab1.digits[grep("year ", rownames(tab1), ignore.case = T), ] <- 0
	tab1.digits[1:(grep(table.break, rownames(tab1), ignore.case = T) - 1), -1] <- 0

	tab1 <- matrix(
		sapply(1:length(tab1), function(i) {
			as.character(round(as.vector(tab1)[i], as.vector(tab1.digits)[i]))
		}),
		ncol = ncol(tab1),
		nrow = nrow(tab1),
		dimnames = dimnames(tab1)
	)

	# Pretty counts
	tab1[1:grep("Years of", rownames(tab1)), 1] <- sapply(
		tab1[1:grep("Years of", rownames(tab1)), 1],
		function (i) {
			if (!is.na(i)) {
				prettyNum(as.numeric(i), big.mark = '\\\\,')
			} else {NA}
		})

	# Pretty percents
	tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2] <- sapply(
		tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2],
		function (i) {
			if (!is.na(i)) {
				paste0(as.numeric(i), '\\%')
			} else {NA}
		})

	# Quartiles one column
	tab1[!is.na(tab1[,3]),2] <- paste0(
		tab1[!is.na(tab1[,3]),2],
		",\\,",
		tab1[!is.na(tab1[,3]),3]
	)

	tab1 <- tab1[,-3]

	# Second column gets parentheses
	tab1[!is.na(tab1[,1]), 2] <- paste0(
		"(", tab1[!is.na(tab1[,1]), 2], ")"
	)

	# Math mode
	if (mathmode) {
		tab1 <- matrix(
			sapply(1:length(tab1), function(i) {
				if (!is.na(as.vector(tab1)[i])) {
					paste0('$', as.vector(tab1)[i], '$')
				} else {NA}
			}),
			ncol = ncol(tab1),
			nrow = nrow(tab1),
			dimnames = dimnames(tab1))

		# Counts
		tab1 <- rbind(
			c(paste0('$',
							 prettyNum(n_distinct(df$studyno), big.mark = '\\\\,'),
							 '$'),
				paste0('$(',
							 prettyNum(nrow(df[(is.na(yod) | year <= year(yod)) |
							 										(is.na(yoc) | year <= year(yoc))]), big.mark = '\\\\,'),
							 ')$')
			),
			tab1
		)

		rownames(tab1)[1] <- "$N$ (person$\\cdot$years)"

		# # Collapse columns
		# tab1[!is.na(tab1[,1]), 1] <- paste(
		# 	tab1[!is.na(tab1[,1]), 1],
		# 	tab1[!is.na(tab1[,1]), 2]
		# )
		# tab1 <- tab1[,-2]

		tab1.rownames <- c(rownames(tab1),
											 "\\textbf{Suicide cases}",
											 "\\textbf{Fatal overdose cases}")
		tab1 <- rbindlist(list(
			as.data.frame(tab1),
			data.frame(
				n = c(paste0("$", sum(df$Suicide), "$"),
							paste0("$", sum(df$Overdose), "$")),
				'%' = c("", ""),
				check.names = F
			)
		))
	}

	tab1 <- as.data.frame(tab1)

	rownames(tab1) <- tab1.rownames

	colnames(tab1) <- c('V1', 'V2')

	return(tab1)
}
