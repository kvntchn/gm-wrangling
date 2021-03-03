# Get Cancer site coding from SEER Website
# Kevin Chen
# April 29, 2020
library(here); library(data.table); library(boxr)
box_auth()

source(here::here("cancer incidence", "seer_key.R"))

# Get SEER data
seer <- box_read(447866882720)
setDT(seer)
names(seer)[names(seer) == "Study.no"] <- "studyno"

# Remove rogue spaces
seer[,`:=`(Site = gsub("  ", " ", SEER.site.desc))]

# Give a compatible `Site` to those not in seer.key
seer[!Site %in% seer.key$Type, `:=`(
	Site = sapply(apply(
		data.frame(
			as.numeric(gsub("C", "", ICD.site.code)), ICD.hist.code
		), 1, function(x) {
			seer.tbl$Type[
				sapply(seer.tbl$`ICD-O-3 Site`, function(sites) {x[1] %in% sites}) &
					sapply(seer.tbl$`ICD-O-3 Histology (Type)`, function(types) {x[2] %in% types})]
		}), function(x) {if (length(x) > 0) {x} else {as.character(x[1])}}))]

# Get site descripion based on ICD site and histology codes
# View(seer.tbl)
seer[, Site2 := (
	apply(data.frame(
		ICD.site.code,
		ICD.hist.code
	), 1, function(d) {
		hist <- d[2]
		icd <- d[1]
		seer.tbl[
			unlist(lapply(`ICD-O-3 Histology (Type)`, function(x) {
				length(which(x %in% hist)) > 0
			})) &
				unlist(lapply(`ICD-O-3 Site`, function(x) {
					length(which(x %in% as.numeric(gsub("C", "", icd)))) > 0
				}))
			, Type]
	})
)]

seer[,Site2 := sapply(Site2, function(x) {
	if (length(x) == 0) {NA} else {x}})]
# seer[,.(studyno, Site, Site2, ICD.site.code, ICD.hist.code)][Site2 != Site | (is.na(Site2) & !is.na(Site))]
seer[, Site := Site2]
seer <- seer[,-"Site2"]

# Add cancer codes to seer data (3 Groups)
lapply(1:3, function(i) {
	seer[Site %in% seer.key[group == i, Type],
			 (paste0("code", i)) := unlist(sapply(Site, function(x) {
			 	seer.key[Type == x, code[group == i]]}
			 ))]
})


# Cast for each group, keeping only malignant cases
lapply(1:3, function(i = 1) {
	assign(
		paste0("group", i),
		seer[!is.na(get(paste0("code", i))) & !is.na(Dx.date) & ICD.behav.code %in% c(
			# 2: Carcinoma in situ
			2,
			# 3: Malignant, primary site
			3
		), .(
			studyno,
			ddiag = Dx.date,
			ddiag_code = paste0("ddiag_", get(paste0("code", i))),
			canc_code = paste0("canc_", get(paste0("code", i)))
		)], envir = .GlobalEnv)
	assign(
		paste0("group", i),
		merge(
			# Date of diagnosis
			dcast(get(paste0("group", i))[!duplicated(get(paste0("group", i)))],
						studyno ~ ddiag_code,
						value.var = "ddiag", fun.aggregate = min),
			# Outcome indicator
			dcast(get(paste0("group", i))[!duplicated(get(paste0("group", i)))],
						studyno ~ canc_code,
						fun.aggregate = function(x) {as.numeric(length(x) > 0)}),
		), envir = .GlobalEnv)
})

seer <- merge(group1, group2, by = "studyno", all = T)
seer <- merge(seer, group3, by = "studyno", all = T)

# Collapse by studyno
seer[,`:=`(studyno.i = studyno)]
seer[,(names(seer[,-c("studyno", "studyno.i"), with = F])) := (
	lapply(seer[,-c("studyno"), with = F][studyno.i == studyno][,-"studyno.i", with = F],
				 function(x) {
				 	if (sum(!is.na(x)) >= 1) {
				 		unique(x[!is.na(x)]) } else {NA}
				 })), by = .(studyno)]


# Add columns to seer if they exist in mcr
mcr <- box_read(781758660783)
need_to_add <- names(mcr)[!names(mcr) %in% names(seer)]
seer[, (need_to_add) := lapply(need_to_add, function(x) {
	if (grepl("canc", x)) {
		return(as.numeric(0))
	} else {
		return(as.IDate(NA))
	}
	NA})]

# all NA canc to 0
for(x in paste0("canc_", cancer.key$code)) {
	seer[is.na(get(x)), (x) := 0]
}

# all NA ddiag to NA
for(x in paste0("ddiag_", cancer.key$code)) {
	seer[!is.finite(as.numeric(get(x))), (x) := NA]
}

seer <- seer[,c("studyno",
								paste0(c("canc_",
												 "ddiag_"),
											 rep(cancer.key$code,
											 		each = 2))), with = F]

# Save to box ####
box_save(seer,
				 dir_id = 113431246688,
				 file_name = "SEER Incidence.rdata",
				 description = "SEER_Matches.csv with columns corresponding to the SEER categories (as coded up in auto_vs).")

box_write(seer,
					dir_id = 113431246688,
					file_name = "SEER Incidence.csv",
					description = "SEER_Matches.csv with columns corresponding to the SEER categories (as coded up in auto_vs).")

# name        : SEER Incidence.rdata
# file id     : 660223470027
# uploaded by : kevchen@berkeley.edu
# owned by    : spa-ehsadmin@berkeley.edu
# parent folder name :  SEER Cancer Data
# parent folder id   :  113431246688
# seer <- box_read(660223470027)
