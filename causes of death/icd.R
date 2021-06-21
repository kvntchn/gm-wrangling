# Additional outcomes ####
additional_outcomes <- function(v_icd, outcome, list_all = F) {

	expand_icd9 <- function(code) {
		c(code,
			apply(expand.grid(code, c(0:9, 'x')), 1, function(x) {
				paste0(x, collapse = "")
			}))
	}
	expand_icd10 <- function(code) {
		c(code,
			apply(expand.grid(code, c(0:9)), 1, function(x) {
				paste0(x, collapse = "")
			}))
	}

	icd.list <- list(
		"Biliary cancer" = list(
			icd9 = c(1551, 1561, 1562, 1568, 1569),
			icd10 = c("C221", paste0("C24", c(0:1, 8:9)))
		),

		'Liver and intrahepatic bile duct cancer' = list(
			icd9 = expand_icd9(c(155)),
			icd10 =  c(expand_icd10(c('C21', 'C22')))
		),

		'Liver cancer' = list(
			icd9 = expand_icd9(c(155))[
				expand_icd9(c(155)) != '1551'],
			icd10 = c(expand_icd10('C22'))
		),

		'Alcohol-related Liver Disease' = list(
			# Case & Deaton definition, Farah's study
			icd9 = paste0(c("571"), c(0:6, 9, "x")),
			icd10 = c(paste0('K70', c(0:4, 9)),
								paste0('K73', c(0:2)),
								expand_icd10(paste0("K", c(738:746))))
		),

		'Gallbladder and extrahepatic bile duct cancer' = list(
			icd9 = c('156x', 1560, 1561, 1562, 1568, 1569),
			icd10 = c(expand_icd10('C23'), expand_icd10('C24'))
		),

		'Brain cancer' = list(
			icd9 = expand_icd9(191),
			icd10 = c(expand_icd10('C70'), expand_icd10('C71'))
		),

		'Colorectal cancer' = list(
			icd9 = expand_icd9(c(153, 154)),
			icd10 = expand_icd10(c("C18", 'C19', 'C20', 'C21'))
		),

		'Colon cancer' = list(
			icd9 = expand_icd9(c(153)),
			icd10 = expand_icd10(c("C18"))
		),

		'Small intestinal cancer' = list(
			icd9 = expand_icd9(c(152)),
			icd10 = expand_icd10(c("C17"))
		),

		'Bladder cancer' = list(
			icd9 = expand_icd9(c(188)),
			icd10 = expand_icd10(c("C67"))
		),

		'Melanoma' = list(
			icd9 = expand_icd9(c(172)),
			icd10 = expand_icd10(c("C43"))
		),

		'Lymphoid leukemia' = list(
			icd9 = expand_icd9(c(204)),
			icd10 = expand_icd10(c('C91'))
		),

		"Alzheimer's disease" = list(
			icd9 = c(3310),
			icd10 = expand_icd10(c('G30'))
		),

		"Hemorrhagic stroke" = list(
			icd9 = expand_icd9(c(431, 432)),
			icd10 = c("I61", "I62")
		),

		"Ischemic stroke" = list(
			icd9 = expand_icd9(c(433, 434)),
			icd10 = expand_icd10("I63") # ICD-10 not validated
		),

		"Ischemic stroke (broad)" = list(
			icd9 = expand_icd9(c(433, 434, 436, 437)),
			icd10 = expand_icd10(c("I63", "I64", "I69")) # ICD-10 not validated
		),

		"Stroke" = list(
			icd9 = expand_icd9(c(431, 432, 433, 434)),
			icd10 = c("I61", "I62", "I63")
		),

		"Non-Hodgkin lymphoma" = list(
			icd9 = expand_icd9(c(200, 202)),
			icd10 = expand_icd10(c(paste0("C", c(82:85, 96))))
		)
	)

	if (list_all) {
		return(names(icd.list))
	} else {
		return(icd.list[[outcome]][[ifelse(v_icd == 9, "icd9", "icd10")]])
		}

}

# Self injury (adapte from Suzanne Dufault) ####
self_injury.function <- function(alcohol = F) {
	# TAKES NOTHING BUT RETURNS A LIST OF SUICIDE AND OVERDOSE CODES
	# (as of 7/2/2019)

	# Suicide ICD-9 ####
	suicideCodes9 <-
		c(950:959, as.vector(sapply(950:959, function(x) {
			paste0(x, 0:9)
		})), paste0(950:959, "x"))

	suicideCodes9 <- c(suicideCodes9, paste0('E', suicideCodes9))

	# Overdose ICD-9 ####
	# Accidental
	accidentalPoison9 <-
		c(850:858, as.vector(sapply(850:858, function(x) {
			paste0(x, 0:9)
		})), paste0(850:858, "x"))

	if (alcohol) {
		accidentalPoison9 <-
			c(accidentalPoison9, paste0(860, c(0, "x")), 860)
	}

	accidentalPoison9 <-
		c(accidentalPoison9, paste0('E', accidentalPoison9))

	# Intent unknown
	intentUnknown9 <-
		c(980, as.vector(sapply(980, function(x) {
			paste0(x, 0:9)
		})), paste0(980, "x"))

	intentUnknown9 <- c(intentUnknown9, paste0('E', 	intentUnknown9))

	# Suicicde ICD-10 ####
	suicideCodes10 <- c(paste0("X", 60:84), "Y87", "U03")
	suicideCodes10 <-
		c(suicideCodes10, apply(expand.grid(suicideCodes10, 0:9), 1,
														function(x) {
															paste0(x, collapse = '')
														}))

	# Overdose ICD-10 ####
	# Accidental
	accidentalPoison10 <- c(paste0("X", 40:44))

	if (alcohol) {
		accidentalPoison10 <- c(accidentalPoison10, "X45")
	}

	accidentalPoison10 <-
		c(accidentalPoison10, apply(expand.grid(accidentalPoison10, 0:9), 1, function(x) {
			paste0(x, collapse = '')
		}))

	# Intent unknown
	intentUnknown10 <- c(
		paste0("Y", c(10:14)))

	intentUnknown10 <-
		c(intentUnknown10, apply(expand.grid(intentUnknown10, 0:9), 1, function(x) {
			paste0(x, collapse = '')
		}))

	# Intent therapeutic
	intentTherapeutic10 <- c(paste0("Y", c(45,47,49)))

	intentTherapeutic10 <-
		c(intentTherapeutic10, apply(expand.grid(intentTherapeutic10, 0:9), 1, function(x) {
			paste0(x, collapse = '')
		}))

	suicide_codes <- c(suicideCodes9, suicideCodes10)

	overdose_codes <- c(
		accidentalPoison9, accidentalPoison10,
		intentUnknown9, intentUnknown10
		#, intentTherapeutic10
	)

	output <- list(
		suicide_codes = list(icd = suicide_codes,
												 v_icd = c(rep(
												 	9, length(suicideCodes9)
												 ),
												 rep(
												 	10, length(suicideCodes10)
												 ))),
		overdose_codes = list(icd = overdose_codes,
													v_icd = c(
														rep(9, length(accidentalPoison9)),
														rep(10, length(accidentalPoison10)),
														rep(9, length(intentUnknown9)),
														rep(10, length(intentUnknown10))#,
														#rep(10, length(intentTherapeutic10))
													))
	)

	return(output)
}

# ICD broad categories  ####

icd_codes.function <- function(cancer_detail = F) {
	# ICD-9 Major categories ####

	icd9_descriptions <- c(
		"Infectious and parasitic dis",
		"All cancers",
		"Endocrine, nutritional and metabolic diseases, and immunity disorders",
		"Blood and blood-forming organs",
		"Mental disorders, behavioral and neurodevelopmental disorders",
		"Nervous system",
		"Circulatory system",
		"Respiratory system",
		"Digestive system",
		"Genitourinary system",
		"Pregnancy, childbirth, and the puerperium",
		"Skin and subcutaneous tissue",
		"Musculoskeletal system and connective tissue",
		"Congenital anomalies",
		"Perinatal period",
		"Not elsewhere classified",
		"Injury, poisoning, and external causes"
	)

	icd9_cancer <- c(
		"Malignant neoplasms of lip, oral cavity and pharynx",
		"Malignant neoplasms of digestive organs and peritoneum",
		"Malignant neoplasms of respiratory and intrathoracic organs",
		"Malignant neoplasms of bone, articular cartilage, and connective tissue",
		"Malignant neoplasms of skin",
		"Malignant neoplasms of breast",
		"Malignant neoplasms of male and female genital organs",
		"Malignant neoplasms of urinary tract",
		"Malignant neoplasms of ill-defined, other secondary and unspecified sites",
		"Malignant neoplasms of lymphoid, hematopoietic and related tissue",
		"Benign neoplasms",
		"In situ neoplasms",
		"Neoplasms of uncertain behavior (ICD-10 includes polycythemia vera, and myelodysplastic syndromes)",
		"Neoplasms of unspecified behavior"
	)

	icd9_codes <- list()
	icd9_codes[icd9_descriptions] <- list(
		# 001–139: infectious and parasitic diseases
		sprintf("%03d", 001:139),
		# 140–239: neoplasms
		140:239,
		# 240–279: endocrine, nutritional and metabolic diseases, and immunity disorders
		240:279,
		# 280–289: diseases of the blood and blood-forming organs
		280:289,
		# 290–319: mental disorders
		290:319,
		# 320–389: diseases of the nervous system and sense organs
		320:389,
		# 390–459: diseases of the circulatory system
		390:459,
		# 460–519: diseases of the respiratory system
		460:519,
		# 520–579: diseases of the digestive system
		520:579,
		# 580–629: diseases of the genitourinary system
		580:629,
		# 630–679: complications of pregnancy, childbirth, and the puerperium
		630:679,
		# 680–709: diseases of the skin and subcutaneous tissue
		680:709,
		# 710–739: diseases of the musculoskeletal system and connective tissue
		710:739,
		# 740–759: congenital anomalies
		740:759,
		# 760–779: certain conditions originating in the perinatal period
		760:779,
		# 780–799: symptoms, signs, and ill-defined conditions
		780:799,
		# 800–999: injury, poisoning, and external causes)
		# For the NDI data, we assume codes 800-999 carry the prefix E
		800:999
	)

	if (cancer_detail) {
		icd9_codes <- icd9_codes[-2]
		icd9_codes[icd9_cancer] <- list(
			# 140-149		malignant neoplasms of lip, oral cavity and pharynx
			c(140:149),
			# 150-159		malignant neoplasms of digestive organs and peritoneum
			c(150:159),
			# 160-165		malignant neoplasms of respiratory and intrathoracic organs
			c(160:165),
			# 170-172		malignant neoplasms of bone, articular cartilage, and connective tissue
			c(170:172),
			# 172-173		melanoma and other malignant neoplasms of skin
			c(172:173),
			# 174-175		malignant neoplasms of breast
			c(174:175),
			# 179-187		malignant neoplasms of male and female genital organs
			c(179:187),
			# 188-189		malignant neoplasms of urinary tract
			c(188:189),
			# 190-199		malignant neoplasms of ill-defined, other secondary and unspecified sites
			c(190:199),
			# 200-208		malignant neoplasms of lymphoid, hematopoietic and related tissue
			c(200:208),
			# 210-229		benign neoplasms
			c(210:229),
			# 230-234		in situ neoplasms
			c(230:234),
			# 235-238		neoplasms of uncertain behavior (ICD-10 includes polycythemia vera, and myelodysplastic syndromes)
			c(235:238),
			# 239				neoplasms of unspecified behavior
			239
		)
	}

	icd9_codes <- sapply(names(icd9_codes), function(x) {
		c(icd9_codes[[x]], apply(expand.grid(icd9_codes[[x]], c(0:9, 'x')), 1,
														 function(y) {
														 	paste0(y, collapse = '')
														 }))
	})

	# ICD-10 Major categories ####

	icd10_descriptions <- c(
		"Infectious and parasitic dis",
		"All cancers",
		"Blood and blood-forming organs and certain disorders involving the immune mechanism",
		"Endocrine, nutritional and metabolic diseases",
		"Mental, behavioral and neurodevelopmental disorders",
		"Nervous system",
		"Eye and adnexa",
		"Ear and mastoid process",
		"Circulatory system",
		"Respiratory system",
		"Digestive system",
		"Skin and subcutaneous tissue",
		"Musculoskeletal system and connective tissue",
		"Genitourinary system",
		"Pregnancy, childbirth and the puerperium",
		"Perinatal period",
		"Congenital anomalies",
		"Not elsewhere classified",
		"Injury, poisoning and certain other consequences of external causes",
		"External causes of morbidity",
		"Factors influencing health status and contact with health services"
	)

	icd10_cancer <- c(
		"Malignant neoplasms of lip, oral cavity and pharynx",
		"Malignant neoplasms of digestive organs and peritoneum",
		"Malignant neoplasms of respiratory and intrathoracic organs",
		"Malignant neoplasms of bone and articular cartilage",
		"Melanoma and other malignant neoplasms of skin",
		"Malignant neoplasms of mesothelial and soft tissue",
		"Malignant neoplasms of breast",
		"Malignant neoplasms of genital organs",
		"Malignant neoplasms of urinary tract",
		"Malignant neoplasms of eye, brain and other parts of central nervous system",
		"Malignant neoplasms of thyroid and other endocrine glands",
		"Malignant neoplasms of ill-defined, other secondary and unspecified sites",
		"Malignant neuroendocrine tumors",
		"Secondary neuroendocrine tumors",
		"Malignant neoplasms of lymphoid, hematopoietic and related tissue",
		"In situ neoplasms",
		"Benign neoplasms",
		"Neoplasms of uncertain behavior (ICD-10 includes polycythemia vera, and myelodysplastic syndromes)",
		"Neoplasms of unspecified nature or behavior"
	)


	icd10_codes <- list()
	icd10_codes[icd10_descriptions] <- list(
		# 1 Certain infectious and parasitic diseases (A00-B99)
		c(paste0("A", sprintf("%02d", 00:99)),
			paste0("B", sprintf("%02d", 00:99))),
		# 2 Neoplasms (C00-D49)
		c(paste0("C", sprintf("%02d", 00:99)),
			paste0("D", sprintf("%02d", 00:49))),
		# 3 Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism (D50-D89)
		c(paste0("D", sprintf("%02d", 50:89))),
		# 4 Endocrine, nutritional and metabolic diseases (E00-E89)
		c(paste0("E", sprintf("%02d", 00:89))),
		# 5 Mental, Behavioral and Neurodevelopmental disorders (F01-F99)
		c(paste0("F", sprintf("%02d", 01:99))),
		# 6 Diseases of the nervous system (G00-G99)
		c(paste0("G", sprintf("%02d", 00:99))),
		# 7 Diseases of the eye and adnexa (H00-H59)
		c(paste0("H", sprintf("%02d", 00:59))),
		# 8 Diseases of the ear and mastoid process (H60-H95)
		c(paste0("H", sprintf("%02d", 60:95))),
		# 9 Diseases of the circulatory system (I00-I99)
		c(paste0("I", sprintf("%02d", 00:99))),
		# 10 Diseases of the respiratory system (J00-J99)
		c(paste0("J", sprintf("%02d", 00:99))),
		# 11 Diseases of the digestive system (K00-K95)
		c(paste0("K", sprintf("%02d", 00:95))),
		# 12 Diseases of the skin and subcutaneous tissue (L00-L99)
		c(paste0("L", sprintf("%02d", 00:99))),
		# 13 Diseases of the musculoskeletal system and connective tissue (M00-M99)
		c(paste0("M", sprintf("%02d", 00:99))),
		# 14 Diseases of the genitourinary system (N00-N99)
		c(paste0("N", sprintf("%02d", 00:99))),
		# 15 Pregnancy, childbirth and the puerperium (O00-O9A)
		c(paste0("O", sprintf("%02d", 00:99))),
		# 16 Certain conditions originating in the perinatal period (P00-P96)
		c(paste0("P", sprintf("%02d", 00:96))),
		# 17 Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)
		c(paste0("Q", sprintf("%02d", 00:99))),
		# 18 Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)
		c(paste0("R", sprintf("%02d", 00:99))),
		# 19 Injury, poisoning and certain other consequences of external causes (S00-T88)
		c(paste0("S", sprintf("%02d", 00:99)),
			paste0("T", sprintf("%02d", 00:88))),
		# 20 External causes of morbidity (V00-Y99)
		c(
			paste0("V", sprintf("%02d", 00:99)),
			paste0("W", sprintf("%02d", 00:99)),
			paste0("X", sprintf("%02d", 00:99)),
			paste0("Y", sprintf("%02d", 00:99))
		),
		# 21 Factors influencing health status and contact with health services (Z00-Z99)
		c(paste0("Z", sprintf("%02d", 00:99)))
	)

	if (cancer_detail) {
		icd10_codes <- icd10_codes[-2]
		icd10_codes[icd10_cancer] <- list(
			# C00-C14  Malignant neoplasms of lip, oral cavity and pharynx
			c(paste0("C", sprintf("%02d", 00:14))),
			# C15-C26  Malignant neoplasms of digestive organs
			c(paste0("C", 15:26)),
			# C30-C39  Malignant neoplasms of respiratory and intrathoracic organs
			c(paste0("C", 30:39)),
			# C40-C41  Malignant neoplasms of bone and articular cartilage
			c(paste0("C", 40:41)),
			# C43-C44  Melanoma and other malignant neoplasms of skin
			c(paste0("C", 43:44)),
			# C45-C49  Malignant neoplasms of mesothelial and soft tissue
			c(paste0("C", 45:49)),
			# C50-C50  Malignant neoplasms of breast
			c(paste0("C", 50)),
			# C51-C58  Malignant neoplasms of male and female genital organs
			c(paste0("C", c(51:58, 60:63))),
			# C64-C68  Malignant neoplasms of urinary tract
			c(paste0("C", 64:68)),
			# C69-C72  Malignant neoplasms of eye, brain and other parts of central nervous system
			c(paste0("C", 69:72)),
			# C73-C75  Malignant neoplasms of thyroid and other endocrine glands
			c(paste0("C", 73:75)),
			# C76-C80  Malignant neoplasms of ill-defined, other secondary and unspecified sites
			c(paste0("C", 76:80)),
			# C7A-C7A  Malignant neuroendocrine tumors
			c(paste0("C7A")),
			# C7B-C7B  Secondary neuroendocrine tumors
			c(paste0("C7B")),
			# C81-C96  Malignant neoplasms of lymphoid, hematopoietic and related tissue
			c(paste0("C", 81:96)),
			# D00-D09  In situ neoplasms
			c(paste0("D", sprintf("%02d", 0:9))),
			# D10-D36  Benign neoplasms, except benign neuroendocrine tumors
			# D3A-D3A  Benign neuroendocrine tumors
			c(paste0("D", 10:36), paste0("D3A")),
			# D37-D48  Neoplasms of uncertain behavior, polycythemia vera and myelodysplastic syndromes
			c(paste0("D", 37:48)),
			# D49-D49  Neoplasms of unspecified behavior
			c(paste0("D", 49))
		)

	}

	icd10_codes <- sapply(icd10_descriptions, function(x) {
		c(icd10_codes[[x]], apply(expand.grid(icd10_codes[[x]], c(0:9)), 1,
															function(y) {
																paste0(y, collapse = '')
															}))
	})

	# Render output

	if (cancer_detail) {
		output <- list(
			icd9_codes = icd9_codes,
			icd9_descriptions = names(icd9_codes),
			icd9_cancer = icd9_cancer,
			icd10_codes = icd10_codes,
			icd10_descriptions = names(icd10_codes),
			icd10_cancer = icd10_cancer
		)
	} else {
		output <- list(
			icd9_codes = icd9_codes,
			icd9_descriptions = names(icd9_codes),
			icd10_codes = icd10_codes,
			icd10_descriptions = names(icd10_codes)
		)
	}
	return(output)
}

# ICD specific conditions ####

spec_icd_codes <- function(totals = T,
													 totals_only = F,
													 natural_causes = F,
													 ...) {
	if (!totals) {totals_only <- F}
	if ((natural_causes & totals) |
			(natural_causes & totals_only)) {
		totals <- F
		totals_only <- F
		message('Tabulating All natural causes only. Avoid error by setting \n totals = F')
	}
	self_injury <- self_injury.function()
	# All changes here should also be reflected in
	# "GM\causes of death\NIOSH rates\minor-key.tsv"
	icd_descriptions <- c(

		"All cancers",
		"Esophageal cancer",
		"Stomach cancer",
		"Intestinal cancer",
		"Rectal cancer",
		"Kidney cancer",
		"Bladder and urinary organ cancers",
		"Bile duct, liver, and gallbladder cancers",
		"Pancreatic cancer",
		"Laryngeal cancer",
		"Lung cancer",
		"Skin cancer",
		# get melanoma/non-melanoma
		"Prostate cancer",
		"Breast cancer",
		"Brain and nervous system cancers",
		"Leukemia",
		"All nonmalignant respiratory diseases",
		"Chronic obstructive pulmonary disease",
		# "Influenza",
		"Pneumonia",
		"Cirrhosis and other chronic liver disease",
		"All heart diseases",
		"Ischemic heart disease",
		"Rheumatic heart disease",
		"Pulmonary heart disease",
		"Heart failure",
		"Cerebrovascular disease",
		"All external causes",
		"Accidents (transportation, falls, and medical)",
		"Other accidents",
		"Homicide"
		# "Suicide"
		# "Overdose"
	)

	totals_only.which <- grep('All', icd_descriptions)
	if (totals_only) {
		icd_descriptions <- icd_descriptions[totals_only.which]
	}

	if (natural_causes) {
		icd_descriptions <- 'All natural causes'
	}

	# ICD-9 ####

	icd9_codes <- list()
	expand_icd9 <- function(code) {
		c(code,
			apply(expand.grid(code, c(0:9, 'x')), 1, function(x) {
				paste0(x, collapse = "")
			}))
	}

	# ICD-9 All natural causes ####
	if (natural_causes) {
		icd9_codes[icd_descriptions] <- list(
			# All natural causes
			expand_icd9(sprintf('%03d', 1:799))
		)
	} else {
		# ICD-9 Totals only ####
		if (totals_only) {
			icd9_codes[icd_descriptions] <- list(
				# All cancer
				expand_icd9(140:239),
				# All nonmalignant respiratory diseases
				expand_icd9(c(
					# Acute Respiratory Infection
					460:466,
					# Influenza
					487,
					# Pneumonia
					480:486,
					# COPD
					490:492, 496,
					# Asthma
					493,
					# Pneumoconiosis and other
					470:478, 494, 495, 497:519
				)),
				# All heart diseases
				expand_icd9(c(
					# Rheumatic
					390:398,
					# IHD
					410:414,
					# Chronic disease of endocardium
					424,
					# Hypertension w/ heart disease
					402, 404,
					# Other diseases of heart
					420:423, 425:428, 429
				)),
				# All external causes
				c(
					# Accidents
					expand_icd9(c(
						# Transportation accidents and other
						800:848, 929,
						# THESE ARE POISONINGS
						850:869,
						# Accidental falls
						880:888,
						# Other accidents (includes some of our poisoning codes)
						890:928, 980:999,
						# Medical complications
						870:879, 930:949,
						# Homicide
						960:978)),
					# Suicide
					self_injury$suicide_codes$icd[self_injury$suicide_codes$v_icd == 9]
				)
			)

		} else {
			# ICD-9 Full list ####
			icd9_codes.tmp <- list()
			icd9_codes.tmp[icd_descriptions] <- list(
				# All cancer
				expand_icd9(140:239),
				# Esophageal cancer
				c(expand_icd9(150)#, 2301
				),
				# Stomach cancer
				c(expand_icd9(151)#, 2302
				),
				# Intestinal cancer
				c(expand_icd9(c(152, 153))#, 2303
				),
				# Rectal cancer
				c(expand_icd9(154)#, 2304
				),
				# Kidney cancer
				c(1890:1892),
				# Bladder and uretic cancers
				c(expand_icd9(c(188)), 1893:1899#, 2337, 2367, 2394
				),
				# Bile duct, liver, and gallbladder cancers
				c(expand_icd9(c(155, 156))#, 2353
				),
				# Pancreatic cancer
				c(expand_icd9(157)),
				# Laryngeal cancer
				c(expand_icd9(161)#, 2310, 2356
				),
				# Lung cancer
				c(expand_icd9(162)#, 2312, 2357
				),
				# Skin cancer
				c(expand_icd9(c(172, # Melanoma
												173#, 232
				))#, 2382
				),
				# Prostate cancer
				c(expand_icd9(185)#, 2334, 2365
				),
				# Breast cancer
				expand_icd9(c(174, 175)),
				# Brain and nervous system cancers
				c(expand_icd9(c(191, 192))#, 2375, 2396
				),
				# Leukemia
				c(#2024, 2031,
					expand_icd9(c(
						204:208
					))),
				# All nonmalignant respiratory diseases
				expand_icd9(c(
					# Acute Respiratory Infection
					460:466,
					# Influenza
					487,
					# Pneumonia
					480:486,
					# COPD
					490:492, 496,
					# Asthma
					493,
					# Pneumoconiosis and other
					470:478, 494, 495, 497:519
				)),
				# Chronic obstructive pulmonary disease
				expand_icd9(c(490:492, 496)),
				# Influenza
				# expand_icd9(487),
				# Pneumonia
				expand_icd9(480:486),
				# Cirrhosis and other chronic liver disease
				expand_icd9(571),
				# All heart diseases
				c(expand_icd9(c(
					# Rheumatic
					390:398,
					# IHD
					410:414,
					# Chronic disease of endocardium
					424,
					# Hypertension w/ heart disease
					402, 404,
					# Other diseases of heart
					420:423, 425:428, 429
				))),
				# IHD
				expand_icd9(410:414),
				# RHD
				expand_icd9(390:398),
				# Pulmonary heart disease
				expand_icd9(415:417),
				# Heart failure
				expand_icd9(428),
				# Cerebrovascular disease
				expand_icd9(430:438),
				# All external causes
				c(
					# Accidents
					expand_icd9(c(
						# Transportation accidents and other
						800:848, 929,
						# THESE ARE POISONINGS
						850:869,
						# Accidental falls
						880:888,
						# Other accidents (includes some of our poisoning codes)
						890:928, 980:999,
						# Medical complications
						870:879, 930:949,
						# Homicide
						960:978)),
					# Suicide
					self_injury$suicide_codes$icd[self_injury$suicide_codes$v_icd == 9]
				),
				# Accidents
				expand_icd9(c(
					# Transportation accidents and other
					800:848, 929,
					# THESE ARE POISONINGS
					# 850:869,
					# Accidental falls
					880:888,
					# Other accidents (includes some of our poisoning codes)
					# 890:928, 980:999,
					# Medical complications
					870:879, 930:949)),
				# Other accidents
				expand_icd9(c(890:928, 980:999)),
				# Homicide
				expand_icd9(c(960:978))
				# Suicide
				# self_injury$suicide_codes$icd[self_injury$suicide_codes$v_icd == 9]
				# Overdose
				# self_injury$overdose_codes$icd[self_injury$overdose_codes$v_icd == 9]
			)

			if (totals) {
				icd9_codes <- icd9_codes.tmp
			} else {
				icd9_codes <- icd9_codes.tmp[-totals_only.which]
			}
		}
	}

	# ICD-10 ####
	icd10_codes <- list()
	expand_icd10 <- function(code) {
		c(code,
			apply(expand.grid(code, c(0:9)), 1, function(x) {
				paste0(x, collapse = "")
			}))
	}

	# ICD-10 All natural causes ####
	if (natural_causes) {
		icd10_codes[icd_descriptions] <- list(
			# All natural causes
			expand_icd10(
				apply(expand.grid(LETTERS[-c(19:20, 22:25)],
													sprintf('%02d', 0:99)), 1,
							function (x) {paste0(x, collapse = '')}))
		)
	} else {
		# ICD-10 Totals only ####
		if (totals_only) {
			icd10_codes[icd_descriptions] <- list(
				# All cancer
				expand_icd10(c(
					paste0("C", sprintf("%02d", 0:99)),
					paste0("D", sprintf("%02d", 0:49))
				)),
				# All nonmalignant respiratory diseases
				c(
					# Acute respiratory
					expand_icd10(c("J00", 'J01')),
					"J028", "J029", "J038", "J039",
					expand_icd10(paste0('J', sprintf('%02d', 4:6))),
					expand_icd10(paste0('J', 20:22)),
					# Influenza
					expand_icd10(c('J10', 'J11')),
					# Pneumonia
					'A481',
					expand_icd10(paste0('J', c(12:18))),
					# COPD
					expand_icd10(paste0('J', 40:44)),
					# Asthma
					expand_icd10(paste0('J', 45:46)),
					# Pneumoconiosis and other
					expand_icd10(paste0('J', 30:33)),
					paste0('J34', 1:8),
					expand_icd10(paste0('J', 35:39)),
					expand_icd10('J47'),
					expand_icd10(paste0('J', 60:64)),
					expand_icd10(paste0('J', 66:95)),
					expand_icd10('J98'), "R091"
				),
				# All heart diseases
				c(
					# RHD
					expand_icd10(paste0('I', sprintf('%02d', 0:9))),
					# IHD
					expand_icd10(paste0("I", c(20:22, 24:25))), "I513", "I516",
					# Endocaridum
					expand_icd10(paste0('I', 34:38)),
					# Hypertension w/ heart disease
					expand_icd10(c('I11', "I13")),
					# Other heart
					expand_icd10(paste0(
						'I', c(30:33, 40, 42, 44:50, 52))),
					paste0('I51', c(0:2, 4:5, 7:9)),
					paste0('I97', c(0:1, 8:9)),
					"R001", "R008"
				),
				# All external causes
				c(
					# Accidents
					expand_icd10(c(
						# Transportation accidents
						paste0('V', sprintf('%02d', 1:99)), 'Y85',
						# THESE ARE POISONINGS
						paste0('X', 40:49),
						# Accidental falls
						paste0('W', sprintf('%02d', 0:19)),
						# Other accidents
						paste0('W', sprintf('%02d', 20:99)),
						paste0('X', sprintf('%02d', 0:39)),
						paste0('X', sprintf('%02d', 50:59)),
						paste0('Y', sprintf('%02d', c(10:34, 36, 86, 89))), "Y872",
						# Medical complications
						paste0('Y', sprintf('%02d', 40:84)), "Y88",
						# Homicide
						paste0("X", 85:99),
						paste0("Y", sprintf('%02d', 0:9)),
						"Y35")),
					"Y871",
					# Suicide
					self_injury$suicide_codes$icd[
						self_injury$suicide_codes$v_icd == 10])
			)
		} else {
			# ICD-10 Full list ####
			icd10_codes.tmp <- list()
			icd10_codes.tmp[icd_descriptions] <- list(
				# All cancer
				expand_icd10(c(
					paste0("C", sprintf("%02d", 0:99)),
					paste0("D", sprintf("%02d", 0:49))
				)),
				# Esophageal cancer
				expand_icd10("C15"),
				# Stomach cancer
				c(expand_icd10("C16")#,	"D002", "D371"
				),
				# Intestinal cancer
				c(expand_icd10(c('C17', 'C18'))#,
					# "D010",
					# "D374"
				),
				# Rectal cancer
				c(
					expand_icd10(c('C19', 'C20', 'C21'))#,
					# "D011",
					# "D012",
					# "D375"
				),
				# Kidney cancer
				c(
					expand_icd10(paste0('C', 64:66))
					# "D41",
					# "D410",
					# "D411",
					# "D495"
				),
				# Bladder and uretic cancers
				c(
					expand_icd10(paste0('C', 67:68))
					# "D09",
					# "D090",
					# "D414",
					# "D494"
				),
				# Bile duct, liver, and gallbladder cancers
				c(
					expand_icd10(paste0('C', 22:24))#,
					# "D015",
					# "D376"
				),
				# Pancreatic cancer
				expand_icd10('C25'),
				# Laryngeal cancer
				c(
					expand_icd10('C32')#,
					# "D02", "D020", "D38", "D380"
				),
				# Lung cancer
				c(
					expand_icd10(c('C33', 'C34'))#,
					# "D022",
					# "D381"
				),
				# Skin cancer
				c(
					expand_icd10(paste0('C', 43:44)),
					"C460", "C469"#,
					# # Kaposi
					# "D031",
					# # Melanoma
					# "D032",
					# # Melanoma
					# "D033",
					# # Melanoma
					# "D034",
					# # Melanoma
					# "D035",
					# # Melanoma
					# "D04",
					# "D040",
					# "D041",
					# "D042",
					# "D043",
					# "D044",
					# "D045",
					# "D046",
					# "D047",
					# "D048",
					# "D049",
					# "D485",
					# "D492"
				),
				# Prostate cancer
				c(expand_icd10('C61')#,
					# "D075", "D40", "D400"
				),
				# Breast cancer
				expand_icd10('C50'),
				# Brain and nervous system cancers
				c(
					expand_icd10(paste0('C', c(47, 70:72)))#,
					# "D43",
					# "D430",
					# "D431",
					# "D432",
					# "D496"
				),
				# Leukemia
				c(
					paste0('C', 910:913),
					paste0('C', 915:919),
					expand_icd10(paste0("C", c(92:95)))
				),
				# All nonmalignant respiratory diseases
				c(
					# Acute respiratory
					expand_icd10(c("J00", 'J01')),
					"J028", "J029", "J038", "J039",
					expand_icd10(paste0('J', sprintf('%02d', 4:6))),
					expand_icd10(paste0('J', 20:22)),
					# Influenza
					expand_icd10(c('J10', 'J11')),
					# Pneumonia
					'A481',
					expand_icd10(paste0('J', c(12:18))),
					# COPD
					expand_icd10(paste0('J', 40:44)),
					# Asthma
					expand_icd10(paste0('J', 45:46)),
					# Pneumoconiosis and other
					expand_icd10(paste0('J', 30:33)),
					paste0('J34', 1:8),
					expand_icd10(paste0('J', 35:39)),
					expand_icd10('J47'),
					expand_icd10(paste0('J', 60:64)),
					expand_icd10(paste0('J', 66:95)),
					expand_icd10('J98'), "R091"
				),
				# Chronic obstructive pulmonary disease
				expand_icd10(paste0('J', sprintf('%02d', 40:44))),
				# Influenza
				# expand_icd10(c('J10', 'J11')),
				# Pneumonia
				c('A481',
					expand_icd10(paste0('J', c(12:18)))
				),
				# Cirrhosis and other chronic liver disease
				c(expand_icd10(c('K70', 'K73', 'K74')), "K760"),
				# All heart diseases
				c(
					# RHD
					expand_icd10(paste0('I', sprintf('%02d', 0:9))),
					# IHD
					expand_icd10(paste0("I", c(20:22, 24:25))), "I513", "I516",
					# Endocaridum
					expand_icd10(paste0('I', 34:38)),
					# Hypertension w/ heart disease
					expand_icd10(c('I11', "I13")),
					# Other heart
					expand_icd10(paste0(
						'I', c(30:33, 40, 42, 44:50, 52))),
					paste0('I51', c(0:2, 4:5, 7:9)),
					paste0('I97', c(0:1, 8:9)),
					"R001", "R008"
				),
				# IHD (excluding complications arising from IHD)
				c(expand_icd10(paste0("I", c(20:22, 24:25))), "I513", "I516"),
				# RHD
				expand_icd10(paste0('I', sprintf('%02d', c(0:9)))),
				# Pulmonary heart disease
				c(
					"I26",
					"I260",
					"I269",
					"I27",
					"I270",
					"I271",
					"I272",
					"I278",
					"I278",
					"I279",
					"I28",
					"I280",
					"I281",
					"I288",
					"I289"
				),
				# Heart Failure
				c("I501", "I502", "I503", "I504", "I508", "I509"),
				# Cerebrovascular disease
				c(
					paste0("G45", c(0:2, 4:9)),
					expand_icd10(paste0('I', 60:69))
				),
				# All external causes
				c(
					# Accidents
					expand_icd10(c(
						# Transportation accidents
						paste0('V', sprintf('%02d', 1:99)), 'Y85',
						# THESE ARE POISONINGS
						paste0('X', 40:49),
						# Accidental falls
						paste0('W', sprintf('%02d', 0:19)),
						# Other accidents
						paste0('W', sprintf('%02d', 20:99)),
						paste0('X', sprintf('%02d', 0:39)),
						paste0('X', sprintf('%02d', 50:59)),
						paste0('Y', sprintf('%02d', c(10:34, 36, 86, 89))), "Y872",
						# Medical complications
						paste0('Y', sprintf('%02d', 40:84)), "Y88",
						# Homicide
						paste0("X", 85:99),
						paste0("Y", sprintf('%02d', 0:9)),
						"Y35")),
					"Y871",
					# Suicide
					self_injury$suicide_codes$icd[
						self_injury$suicide_codes$v_icd == 10]),
				# Accidents
				expand_icd10(c(
					# Transportation accidents
					paste0('V', sprintf('%02d', 1:99)), 'Y85',
					# THESE ARE POISONINGS
					# paste0('X', 40:49),
					# Accidental falls
					paste0('W', sprintf('%02d', 0:19)),
					# Other accidents
					# paste0('W', sprintf('%02d', 20:99)),
					# paste0('X', sprintf('%02d', 0:39)),
					# paste0('X', sprintf('%02d', 50:59)),
					# paste0('Y', sprintf('%02d', c(10:34, 36, 86, 89))), "Y872",
					# Medical complications
					paste0('Y', sprintf('%02d', 40:84)), "Y88"
				)),
				# Other accidents
				c(expand_icd10(c(paste0('W', sprintf('%02d', 20:99)),
												 paste0('X', sprintf('%02d', 0:39)),
												 paste0('X', sprintf('%02d', 50:59)),
												 paste0('Y', sprintf('%02d', c(10:34, 36, 86, 89)))
				)), "Y872"),
				# Homicide
				c(expand_icd10(c(
					paste0("X", 85:99),
					paste0("Y", sprintf('%02d', 0:9)),
					"Y35")),
					"Y871")
				# Suicide
				# self_injury$suicide_codes$icd[self_injury$suicide_codes$v_icd == 10]
				# Overdose
				# self_injury$overdose_codes$icd[self_injury$overdose_codes$v_icd == 10]
			)
			if (totals) {
				icd10_codes <- icd10_codes.tmp
			} else {
				icd10_codes <- icd10_codes.tmp[-totals_only.which]
			}
		}
	}

	# Render output ####

	output <- list(
		icd9_codes = icd9_codes,
		icd9_descriptions = names(icd9_codes),
		icd10_codes = icd10_codes,
		icd10_descriptions = names(icd10_codes)
	)
	return(output)
}

# Getting types ####

death_type <- function(icd,
											 v_icd,
											 description = T,
											 ind = F,
											 save_codes = F,
											 cancer_detail = F,
											 codes = spec_icd_codes(),
											 studyno = NULL) {
	if (is.null(codes)) {
		if (save_codes) {
			codes <<- icd_codes.function(cancer_detail = cancer_detail)
		} else {
			codes <- icd_codes.function(cancer_detail = cancer_detail)
		}
	}

	icd9_codes <- codes$icd9_codes
	icd9_descriptions <- codes$icd9_descriptions

	if (cancer_detail) {
		icd9_cancer <- codes$icd9_cancer
		cancer.index9 <-
			which(names(icd9_codes) %in% names(icd9_codes[icd9_cancer]))
	}

	icd10_codes <- codes$icd10_codes
	icd10_descriptions   <- codes$icd10_descriptions

	if (cancer_detail) {
		icd10_cancer <- codes$icd10_cancer
		cancer.index10 <-
			which(names(icd10_codes) %in% names(icd10_codes[icd10_cancer]))
	}

	if (sum(v_icd == 9, na.rm = T) > 0) {
		icd9  <- icd[v_icd ==  9 & !is.na(v_icd)]
		index9 <- list()
		index9 <-
			sapply(1:length(icd9), function(x) {
				unlist(sapply(1:length(icd9_codes), function(y) {
					if (icd9[x] %in% icd9_codes[[y]]) {
						y
					}
				}))
			})
	} else {
		index9 <- NULL
	}

	if (sum(v_icd == 10, na.rm = T) > 0) {
		icd10 <- icd[v_icd == 10 & !is.na(v_icd)]
		index10 <- list()
		index10 <-
			sapply(1:length(icd10), function(x) {
				unlist(sapply(1:length(icd10_codes), function(y) {
					if (icd10[x] %in% icd10_codes[[y]]) {
						y
					}
				}))
			})
	} else {
		index10 <- NULL
	}


	if (cancer_detail) {
		is.cancer <- rep(NA, length(icd))
		if (sum(v_icd == 9, na.rm = T) > 0) {
			is.cancer[v_icd == 9 &
									!is.na(v_icd)][index9 %in% cancer.index9] <- T
		}
		if (sum(v_icd == 10, na.rm = T) > 0) {
			is.cancer[v_icd == 10 &
									!is.na(v_icd)][index10 %in% cancer.index10] <- T
		}
		is.cancer[which(is.na(is.cancer))] <- F
	}


	index <- rep(NA, length(icd))
	if (sum(v_icd == 9, na.rm = T) > 0) {
		index[v_icd == 9  & !is.na(v_icd)] <- index9
	}
	if (sum(v_icd == 10, na.rm = T) > 0) {
		index[v_icd == 10 & !is.na(v_icd)] <- index10
	}

	if (description) {
		if (!is.null(index9)) {
			cat9 <- lapply(index9, function(x) {
				icd9_descriptions[x]
			})
		} else {
			cat9 <- NULL
		}
		if (!is.null(index10)) {
			cat10 <- lapply(index10, function(x) {
				icd10_descriptions[x]
			})
		} else {
			cat10 <- NULL
		}

		description.output <-	as.data.frame(matrix(NA, nrow = length(c(icd)),
																							 ncol = max(c(
																							 	unlist(lapply(cat9, length))
																							 ),
																							 unlist(
																							 	lapply(cat10, length)
																							 ))))

		if (sum(v_icd == 9, na.rm = T) > 0) {
			which9 <- which(v_icd == 9 & !is.na(v_icd))
			cat9 <- lapply(cat9, function(x) {
				sapply(1:ncol(description.output), function(y) {
					x[y]
				})
			})
			description.output[v_icd == 9 &
												 	!is.na(v_icd),] <-
				matrix(unlist(cat9),
							 nrow = length(cat9),
							 byrow = T)
		}
		if (sum(v_icd == 10, na.rm = T) > 0) {
			which10 <- which(v_icd == 10 & !is.na(v_icd))
			cat10 <- lapply(cat10, function(x) {
				sapply(1:ncol(description.output), function(y) {
					x[y]
				})
			})
			description.output[v_icd == 10 &
												 	!is.na(v_icd),] <-
				matrix(unlist(cat10),
							 nrow = length(cat10),
							 byrow = T)
		}
		description.output[apply(description.output, 1, function(x) {
			length(unique(x)) <= 1
		}), -1] <- NA

		if (!is.null(studyno)) {
			rownames(description.output) <- studyno
		}
	}

	colnames(description.output)[1] <- 'major'
	if (ncol(description.output) == 2) {
		colnames(description.output)[2] <- 'minor'
	}
	if (ncol(description.output) > 2) {
		colnames(description.output)[-1] <-
			paste0('minor', 1:(ncol(description.output) - 1))
	}

	# Render output

	if (cancer_detail & description) {
		output <-
			list(if (ind) {
				index = index
			},
			description = description.output,
			is.cancer = is.cancer)
	}
	if (description & !cancer_detail) {
		output <-
			list(if (ind) {
				index = index
			}, description = description.output)
	}
	if (!description & cancer_detail) {
		output <- list(if (ind) {
			index = index
		}, is.cancer = is.cancer)
	}
	if (!description & !cancer_detail) {
		output <- index
	}

	return(output)
}
