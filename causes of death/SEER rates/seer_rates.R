# Parsing SEER mortality rates ####
# Kevin Chen
# March 2, 2020

if (!grepl("gm", getwd(), ignore.case = T)) {
	if ("here" %in% .packages()) {
		detach("package:here", unload = T)
	}
	setwd('gm')
	library(here)
}

source(here::here("wrangling", "00-my-theme.R"))
library(data.table)

# Site mappings
site.key <- fread(here::here(
	"SEER rates",
	"site-key.tsv"
))
names(site.key) <- c("site", "description")
incidence.key <- fread(here::here("cancer incidence", 'cancer-key.tsv'))

# Match to incidence.key
cbind(sapply(incidence.key$description, function(x) {
	if (!grepl("can", x, ignore.case = T)) {
		x <- paste(x, "cancer")
	}
	grep(paste0("^", substr(x, 1, unlist(gregexpr(" can", x)) - 1)), site.key$description, ignore.case = T, value = T)[1]
}),
sapply(incidence.key$description, function(x) {
	if (!grepl("can", x, ignore.case = T)) {
		x <- paste(x, "cancer")
	}
	grep(paste0("^", substr(x, 1, unlist(gregexpr(" can", x)) - 1)), site.key$description, ignore.case = T)[1]
})
)
mapping <- sapply(incidence.key$description, function(x) {
	if (!grepl("can", x, ignore.case = T)) {
		x <- paste(x, "cancer")
	}
	grep(paste0("^", substr(x, 1, unlist(gregexpr(" can", x)) - 1)), site.key$description, ignore.case = T)[1]
})

# Manual fixes
mapping[names(mapping) == "Brain cancer"] <- 74
mapping[names(mapping) == "Non-Hodgkin's lymphoma"] <- 83

mapping[grepl("Male", names(mapping))] <- 61

mapping.dat <- as.data.table(cbind(
	incidence.key,
	site.key[mapping,]
))

# Write site mappings
apply(data.frame(
	race = c("white", "white", "black", "black", "allraces",  "allraces"),
	gender = rep(c("male", "female"), 3)
), 1, function(x){
	race <- unlist(x[1]); gender <- unlist(x[2])
	tmp <- fread(here::here("seer rates/rates csv", paste0(race, "_", gender, ".csv")))
	tmp <- tmp[site %in% mapping.dat$site]
	tmp[, site := factor(
		site, levels = sort(unique(site)),
		labels = mapping.dat[order(site), code]
	)]
	names(tmp)[names(tmp) == "site"] <- "code"
	names(tmp)[names(tmp) == "period"] <- "year"
	names(tmp)[names(tmp) == "race"] <- "Race"
	names(tmp)[names(tmp) == "gender"] <- "Sex"
	fwrite(tmp, here::here("seer rates/rates cleaned", paste0(race, "_", gender, ".csv")))
})

