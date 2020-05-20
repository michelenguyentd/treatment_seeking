# Read in GBD2019 version from Z://:

rm(list = ls())
setwd('J:/Treatment_Seeking/')

data.path <- 'J:/Treatment_Seeking/Data/'

GBD2019_TS <- read.csv("Z:/GBD2019/Processing/Stages/02b_Model_TS/Checkpoint_Outputs/TS_predictions_aggreg.csv")
endemic_config <- read.csv("Z:/GBD2019/Processing/Config_Data/endemicity_config_2019.csv")

colnames(GBD2019_TS)
head(GBD2019_TS)
tail(GBD2019_TS)

# Country-level results, for all years, with columns for name, ISO3, Gaul (if that's easy to include), year, public, private, both, and none):

WHO_version <- GBD2019_TS[GBD2019_TS$Admin_Unit_Level == "ADMIN0", c("Country_Name", "ISO3", "IHME_location_id", "GAUL_Code", "t.Year", "Any_pred", "Any_pred_low", "Any_pred_high", "HMIS_pred", "HMIS_pred_low", "HMIS_pred_high", "HMISfrac_pred", "HMISfrac_pred_low", "HMISfrac_pred_high")]
head(WHO_version)
length(unique(WHO_version$ISO3))

# Include private treatment seeking: 
WHO_version$Priv_treat <- WHO_version$Any_pred*(1 - WHO_version$HMISfrac_pred)
summary(WHO_version$Priv_treat)
WHO_version$Priv_treat_low <- WHO_version$Any_pred_low*(1 - WHO_version$HMISfrac_pred_high)
summary(WHO_version$Priv_treat_low)
WHO_version$Priv_treat_high <- WHO_version$Any_pred_high*(1 - WHO_version$HMISfrac_pred_low)
summary(WHO_version$Priv_treat_low)

# Include no treatment seeking:
WHO_version$No_treat <- 1 - WHO_version$Any_pred
WHO_version$No_treat_low <- 1 - WHO_version$Any_pred_high
WHO_version$No_treat_high <- 1 - WHO_version$Any_pred_low

head(WHO_version)

# Remove non-endemic countries:

head(endemic_config)
endemic_units <- endemic_config$location_id[endemic_config$any_malaria_endemic == 1]

unique(WHO_version$Country_Name[!(WHO_version$IHME_location_id %in% endemic_units)]) # Cook Islands, French Guiana, Mayotte, Monaco, Nauru, Niue, Palau, Saint Kitts And Nevis, San Marino, Tokelau, sTuvalu.

# Keep French Guiana and Mayotte:
WHO_version <- WHO_version[WHO_version$IHME_location_id %in% endemic_units | WHO_version$Country_Name %in% c("French Guiana", "Mayotte"), ]
length(unique(WHO_version$ISO3)) # 106.

# Select relevant columns:
WHO_version <- WHO_version[, c("Country_Name", "ISO3", "GAUL_Code", "t.Year", "HMIS_pred", "HMIS_pred_low", "HMIS_pred_high", "Priv_treat", "Priv_treat_low", "Priv_treat_high", "Any_pred", "Any_pred_low", "Any_pred_high", "No_treat", "No_treat_low", "No_treat_high")]
colnames(WHO_version) <- c("Country_Name", "ISO3", "GAUL_Code", "Year", "Pub_treat", "Pub_treat_low", "Pub_treat_high",  "Priv_treat", "Priv_treat_low", "Priv_treat_high", "Any_treat", "Any_treat_low", "Any_pred_high", "No_treat", "No_treat_low", "No_treat_high")
head(WHO_version)

write.csv(WHO_version, paste(data.path, "TreatmentSeeking_WHO.csv", sep = ""))
