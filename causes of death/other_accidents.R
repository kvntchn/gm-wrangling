# "Other" accidents

other.icd9 <- matrix(c(800, 804, "Fracture Of Skull",
890, 899, "Accidents due to fires and flames",
900, 909, "Accidents due to natural and environmental factors",
910, 915, "Accidents caused by submersion, suffocation, and foreign bodies",
916, NA,  "Struck accidentally by falling object",
917, NA,  "Striking against or struck accidentally by objects or person",
918, NA,  "Caught accidentally in or between object",
919, NA,  "Accidents caused by machine",
920, NA,  "Accidents caused by cutting and piercing instruments or object",
921, 923, "Accidents caused by explosions, firearms, and projectiles",
# 921, NA,  "Accident caused by explosion of pressure vessel",
# 922, NA,  "Accident caused by firearm and air gun missiles",
# 923, NA,  "Accident caused by explosive material",
924, NA,  "Accident caused by hot substance or object caustic or corrosive material and steam",
925, NA,  "Accident caused by electric current",
926, NA,  "Exposure to radiation",
927, NA,  "Overexertion and strenuous movement",
928, NA,  "Other and unspecified environmental and accidental cause",
929, NA,  "Late effects of accidental injury",
980, 981, 'Poisoning, undetermined intent',
# 980, NA, "Poisoning by solid or liquid substances, undetermined intent",
981, 982, 'Accidental poisoning by and exposure to noxious substances',
# 981, NA, "Poisoning by gases in domestic use, undetermined intent",
# 982, NA, "Poisoning by other gases, undetermined intent",
983, NA, 'Hanging, strangulation and suffocation, undetermined intent',
984, NA, "Submersion (drowning), undetermined intent",
985, NA, "Injury by firearms air guns and explosivs, undetermined intent",
986, NA, "Injury by cutting and piercing instruments, undetermined intent",
987, NA, "Falling from high place, undetermined intent",
988, NA, "Injury by other and unspecified means, undetermined intent",
989, NA, "Late effcts of injury, undetermined intent",
990, 999, "Injury resulting from operations of war"
), ncol = 3, byrow = T)
colnames(other.icd9) <- c('lower', 'upper', 'description')
other.icd9 <- as.data.table(other.icd9)
other.icd9$prefix <- NA
other.icd9$v_icd <- 9


other.icd10 <- matrix(c(
'W', 20, 49,  'Exposure to inanimate mechanical forces',
'W', 50, 64,  'Exposure to animate mechanical forces',
'W', 65, 74,  'Accidental non-transport drowning and submersion',
'W', 75, 84,  'Other accidental threats to breathing',
'W', 85, 99,  'Exposure to electric current, radiation and extreme ambient air temperature and pressure',
'X', 00, 08,  'Exposure to smoke, fire and flames',
'X', 09, NA,  'Exposure to unspecified smoke, fire and flames',
'X', 10, 19,  'Contact with heat and hot substances',
'X', 20, 29,  'Contact with venomous animals and plants',
'X', 30, 39,  'Exposure to forces of nature',
'X', 40, 49,  'Accidental poisoning by and exposure to noxious substances',
'X', 50, 50,  'Overexertion and strenuous or repetitive movements',
'X', 51, NA,  'Travel and motion',
'X', 52, 58,  'Accidental exposure to other specified factors',
'X', 59, NA,  'Exposure to unspecified factor',
'Y', 10, 19,  'Poisoning, undetermined intent',
'Y', 20, NA,  'Hanging, strangulation and suffocation, undetermined intent',
'Y', 21, 33,  'Event of undetermined intent',
'Y', 34, NA,  'Unspecified event, undetermined intent',
'Y', 36, NA,  'Injury resulting from operations of war',
'Y', 86, NA,  'Sequelae of other accidents',
# 'Y', 87.2, NA, 'Sequelae of events of undetermined intent',
'Y', 89, NA,  'Sequelae of other external causes'
), ncol = 4, byrow = T)
colnames(other.icd10) <- c("prefix", "lower", "upper", "description")
other.icd10 <- as.data.table(other.icd10)
other.icd10$v_icd <- 10

other.icd <- rbindlist(
	list(other.icd9,
			 other.icd10),
	use.names = T
)

other.icd[,`:=`(
	lower = as.numeric(lower),
	upper = as.numeric(upper)
)]
