# Get Cancer site coding from SEER Website
# Kevin Chen
# April 29, 2020
library(here); library(data.table); library(boxr)
box_auth()

source(here::here("cancer incidence", "seer_key.R"))

# Get mcr data
mcr <- box_read(512727972584)
setDT(mcr)

# Remove "C" from icd codes
mcr[, `:=`(
	ICD.site.code = gsub("C", "", icdoii),
	ICD.hist.code = morph,
	Dx.date = ddiag,
	ICD.behav.code = behave3)]

# Get site description based on ICD site and histology codes
# View(seer.tbl)
mcr[, Site := (
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

mcr[,Site := sapply(Site, function(x) {
	if (length(x) == 0) {NA} else {x}})]
# mcr[,.(studyno, Site, ICD.site.code, ICD.hist.code)]

# Add cancer codes to seer data (3 Groups)
lapply(1:3, function(i) {
	mcr[Site %in% seer.key[group == i, Type],
			 (paste0("code", i)) := unlist(sapply(Site, function(x) {
			 	seer.key[Type == x, code[group == i]]}
			 ))]
})


# Cast for each group, keeping only malignant cases
lapply(1:3, function(i = 1) {
	assign(
		paste0("group", i),
		mcr[!is.na(get(paste0("code", i))) & !is.na(Dx.date) & ICD.behav.code %in% c(
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
						studyno ~ canc_code, fun.aggregate = function(x) {as.numeric(length(x) > 0)}),
		), envir = .GlobalEnv)
})

mcr <- merge(group1, group2, by = "studyno", all = T)
mcr <- merge(mcr, group3, by = "studyno", all = T)

# Collapse by studyno and diagnosis
mcr[,`:=`(studyno.i = studyno)]
mcr[,(names(mcr[,-c("studyno", "studyno.i"), with = F])) := (
	lapply(mcr[,-c("studyno"), with = F][studyno.i == studyno][,-c("studyno.i"), with = F],
				 function(x) {
				 	if (sum(!is.na(x)) >= 1) {
				 		unique(x[!is.na(x)]) } else {NA}
				 })), by = .(studyno)]

# all NA canc to 0
for(x in paste0("canc_", cancer.key$code)) {
	mcr[is.na(get(x)), (x) := 0]
}

# all NA ddiag to NA
for(x in paste0("ddiag_", cancer.key$code)) {
	mcr[!is.finite(as.numeric(get(x))), (x) := NA]
}

mcr <- mcr[,c(
	"studyno",
	paste0(c("canc_", "ddiag_"),
				 rep(cancer.key$code, each = 2))
	), with = F]

# Save to box ####
box_save(mcr, dir_id = 113431246688, file_name = "MCR Incidence.rdata")
box_write(mcr, dir_id = 113431246688, file_name = "MCR Incidence.csv")

 # name        : MCR Incidence.rdata
 # file id     : 782029330903
 # version     : V1
 # size        : 110 kB
 # modified at : 2021-03-01 18:58:41
 # created at  : 2021-03-01 18:58:41
 # uploaded by : kevchen@berkeley.edu
 # owned by    : spa-ehsadmin@berkeley.edu
 # shared link : None
 #
 # parent folder name :  Data
 # parent folder id   :  113431246688
# box_load(782029330903)
