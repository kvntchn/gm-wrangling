# Get Cancer site coding from SEER Website
# Kevin Chen
# April 29, 2020
library(here); library(data.table); library(boxr)

cancer.key <- fread(here::here("cancer incidence", 'cancer-key.tsv'))

# # Get SEER codes from internet ####
# seer.url <- "https://seer.cancer.gov/siterecode/icdo3_dwhoheme/index.html"
# seer.xpath <- "//*[@id=\"content\"]/div/table"
#
# # Read html
# library(rvest)
# seer.tbl <- html_table(html_node(
# 	read_html(seer.url),
# 	xpath = seer.xpath), fill = T)
# setDT(seer.tbl)

# Get SEER codes from file ####
seer.tbl <- fread("/Users/kevinchen/eisen/gm-wrangling/cancer incidence/index.txt", fill = T, sep = ";", skip = 1)

# Keep only useful categories
seer.tbl <- seer.tbl[!is.na(Recode) & Recode != 99999]

# Rename Site column; it's not actually Site Group
names(seer.tbl)[names(seer.tbl) == "Site Group"] <- "Type"

# Skip rows indicating super category
seer.tbl <- seer.tbl[
	!apply(data.frame(Type,
										`ICD-O-3 Site`,
										`ICD-O-3 Histology (Type)`,
										Recode), 1, function(j) {
											length(table(j)) == 1
										}), ]
seer.tbl[grepl("C", `ICD-O-3 Histology (Type)`), `:=`(
	`ICD-O-3 Site` = `ICD-O-3 Histology (Type)`,
	`ICD-O-3 Histology (Type)` = "excluding 9050-9055,  9140,  9590-9992")]
# Remove invalid
seer.tbl <- seer.tbl[!grepl("Invalid", Type, T),]

# Make column `ICD-O-3 Site` a list of vectors
seer.tbl[, `:=`(
	`ICD-O-3 Site` = {
		codes <- `ICD-O-3 Site`
		codes <- gsub("C", "", codes)
		codes <- gsub("-", ":", codes)
		# When there is no textual annotation
		codes[!grepl("[a-z]|^$", tolower(codes))] <- paste0(
			"c(", codes[!grepl("[a-z]|^$", tolower(codes))], ")")
		# When site is "All sites except"
		codes[grepl("except", codes, T)] <- paste0(
			"(0:999)[!0:999 %in% c(", gsub("[a-z]", "", tolower(codes[grepl("except", codes, T)])), ")]")
		# For extranodal Hodgkin lymphoma, use all sites not in node
		codes[grepl("Hodgkin - Extranodal", Type, T)] <- paste0(
			"(0:999)[!0:999 %in% c(", codes[grepl("Hodgkin - Nodal", Type, T)], ")]")
		codes[grepl("Invalid", Type, T)] <- NA
		codes[codes == ""] <- NA
		codes <- gsub("  ", " ", codes)
		codes <- lapply(codes, function(x) {eval(parse(text = x))})
		codes
	},
	`ICD-O-3 Histology (Type)` = {
		codes <- `ICD-O-3 Histology (Type)`
		codes <- gsub("-", ":", codes)
		# When there is no textual annotation
		codes[!grepl("[a-z]|^$", tolower(codes))] <- paste0(
			"c(", codes[!grepl("[a-z]|^$", tolower(codes))], ")")
		# When histology type says "excluding"
		codes[grepl("excluding", codes, T)] <- paste0(
			"(8000:9993)[!8000:9993 %in% c(", gsub("[a-z]", "", tolower(codes[grepl("excluding", codes, T)])), ")]")
		codes <- lapply(codes, function(x) {eval(parse(text = x))})
		codes
	}
)]
# If ICD-O-3 code NA, then it takes all site codes
seer.tbl[is.na(`ICD-O-3 Site`), `ICD-O-3 Site` := list(0:900)]

seer.tbl[,`:=`(Type = gsub("  ", " ", Type))]
seer.tbl[,`:=`(Type = gsub("  ", " ", Type))]

seer.key <- data.table(
	Type = unique(seer.tbl$Type))

# Assign descriptions to rows; the more general, the higher the group number
{seer.key[11:33, `:=`(Group1 = "di")] # "All digestive cancers")]
	seer.key[14:25, `:=`(Group2 = "corec")] #"Colorectal cancer")]
	seer.key[14:22, `:=`(Group3 = "co")]#"Colon cancer")]
	seer.key[23:25, `:=`(Group3 = "re")]#"Rectal cancer")]
	seer.key[26:33, `:=`(Group2 = "li")]#"Liver and intrahepatic bile duct cancers")]
	seer.key[30, `:=`(Group3 = "pa")]#"Pancreatic cancer")]
	seer.key[11, `:=`(Group2 = "es")]#"Esophageal cancer")]
	seer.key[12, `:=`(Group2 = "st")]#"Stomach cancer")]
	seer.key[34:38, `:=`(Group1 = "res")]#"All respiratory cancers")]
	seer.key[35, `:=`(Group2 = "la")]#"Laryngeal cancer")]
	seer.key[36, `:=`(Group2 = "lu")]#"Lung and bronchial cancers")]
	seer.key[43, `:=`(Group1 = "br")]#"Breast cancer")]
	seer.key[44:50, `:=`(Group1 = "fe")]#"Female genital cancers")]
	seer.key[44, `:=`(Group2 = "cer")]#"Cerival cancer")]
	seer.key[45:46, `:=`(Group2 = "ute")]#"Uteral cancer")]
	seer.key[47, `:=`(Group2 = "ova")]#"Ovarian cancer")]
	seer.key[48, `:=`(Group2 = "vag")]#"Vaginal cancer")]
	seer.key[19, `:=`(Group2 = "vul")]#"Vulval cancer")]
	seer.key[50, `:=`(Group2 = "ofe")]#"Other female cancer")]
	seer.key[51:54, `:=`(Group1 = "ma")]#"Male genital cancers")]
	seer.key[51, `:=`(Group2 = "pr")]#"Prostate cancer")]
	seer.key[62:63, `:=`(Group1 = "en")]#"Endocrine cancers")]
	seer.key[55:58, `:=`(Group1 = "ur")]#"Urinary cancers")]
	seer.key[56, `:=`(Group2 = "ki")]#"Kidney and renal pelvic cancers")]
	seer.key[55, `:=`(Group2 = "bl")]#"Bladder cancer")]
	seer.key[41:42, `:=`(Group1 = "sk")]#"Skin cancer")]
	seer.key[41, `:=`(Group2 = "me")]#"Melanoma")]
	seer.key[60:61, `:=`(Group1 = "ner")]#"Brain and nervous system cancers")]
	seer.key[60, `:=`(Group2 = "brn")]#"Brain cancer")]
	seer.key[68, `:=`(Group1 = "my")]#"Myeloma")]
	seer.key[69:77, `:=`(Group1 = "leu")]#"Leukemia")]
	seer.key[69:71, `:=`(Group2 = "ll")]#"Lymphocytic leukemia")]
	seer.key[70, `:=`(Group3 = "cll")]#"Chronic lymphocytic leukemia")]
	seer.key[72:75, `:=`(Group2 = "mml")]#"Myeloid and monocytic leukemia")]
	seer.key[72, `:=`(Group3 = "aml")]#"Acute myeloid leukemia")]
	seer.key[64:67, `:=`(Group1 = "lym")]#"Lymphoma")]
	seer.key[66:67, `:=`(Group2 = "nhl")]#"Non-Hodgkin lymphoma")]
}

# Make seer.key long
seer.key <- rbindlist(lapply(paste0("Group", 1:3), function(x) {
	seer.key[!is.na(get(x)),.(Type, code = get(x), group = as.numeric(substring(x, 6)))]
}), use.names = T)
seer.key[,`:=`(
	most.specific = max(group)
), by = .(Type)]

# # Merge ICD and Group objects
# seer.key <- merge(seer.key, seer.tbl, on = "Type")
