library(mgcv) # For GAM.
library(splitstackshape) # For stratified sampling on IHME regions.
library(MuMIn) # for AIC (model selection)
library(gtools) # for logit transform
library(VGAM) # for probit transform
library(survey) # for taking into account sampling weights
library(nortest) # Normality tests for residuals
library(xtable) # For exporting latex tables.
library(plotrix) # For plotCI.
library(MASS) # For Box-Cox transformation plots.

rm(list = ls())
setwd('J:/Treatment_Seeking/')

graphics.path <- 'J:/Treatment_Seeking/graphics/'
data.path <- 'J:/Treatment_Seeking/Data/'

# Years to model:
years <- 1980:2017

# Config data to select countries to model:
config_file <- read.csv("Z:/Config_Data/National_Config_Data.csv")
# Count number of countries to be modelled for treatment seeking: 
admin0_units <- config_file[config_file$MAP_Include == "Y", ]
nrow(admin0_units) # To include Mayotte.

# Create list of iso for countries to model:
country_list <- admin0_units$ISO3 

# Config data for subnational units:
sub_config_file <- read.csv("Z:/GBD2017/Processing/Config_Data/Archive/Subnational_Config_Summary_GBD2017.csv")
# sub_config_file <- read.csv("Z:/GBD2017/Processing/Config_Data/Combined_Config_Data.csv")

# Need a better way to determine which subnational units to model later...
subnational_API_data <- read.csv("Z:/GBD2017/Processing/Stages/04c_API_Data_Export/Checkpoint_Outputs/subnational_ihme_only.csv")
subnational_list <- unique(data.frame("ISO3" = subnational_API_data$iso3, "Admin_Unit_Name" = subnational_API_data$admin_unit_name, "IHME_location_id" = subnational_API_data$ihme_id))
six_minor_terr <- c("Andaman & Nicobar Islands", "Lakshadweep", "Chandigarh", "Dadra & Nagar Haveli", "Daman & Diu", "Puducherry")
# Remove Six Minor Territories and add them individually:
SMT_list <- data.frame("ISO3" = "IND", "Admin_Unit_Name" = six_minor_terr, "IHME_location_id" = NA)
for (i in 1:nrow(SMT_list)){
  SMT_list$IHME_location_id[i] <- sub_config_file$IHME_location_id[sub_config_file$IHME_Location_Name_Short == sub("&", "and", six_minor_terr[i])]
}
subnational_list <- rbind(subnational_list, SMT_list)
subnational_list <- subnational_list[-which(subnational_list$Admin_Unit_Name == "The Six Minor Territories"), ]

# Read in IHME covariates:
load('Z:/GBD2017/Processing/Static_Covariates/IHME/IHME_Subnational_Covariates_GBD2017/covariates_for_treatment_seeking/covariate_list_for_TS_2018_05_25.RData')
str(cov_list)

# Remove the non-aggregated education_yr_pc covariate:

cov_names <- rep(NA, length(cov_list))

for (i in 1:length(cov_names)){
  cov_names[i] <- unique(cov_list[[i]]$covariate_name_short)
}

names(cov_list) <- cov_names

cov_list <- cov_list[-which(cov_names == "education_yrs_pc")]

cov_names <- rep(NA, length(cov_list))

for (i in 1:length(cov_names)){
  cov_names[i] <- unique(cov_list[[i]]$covariate_name_short)
}

names(cov_list) <- cov_names

# Check structure:
ANC1 <- cov_list[[1]]
length(unique(ANC1$location_name)) # 887.
# Note: GUF does not have any IHME covariates - borrow Suriname later.

# Read in static accessibility and night time lights covariates:
DMSP_nighttime <- read.csv("J:/Treatment_Seeking/Data/weighted_rate_outputs/DMSP_F18_nighttime_lights_2010_pop_weighted.csv")
accessibility <- read.csv("J:/Treatment_Seeking/Data/weighted_rate_outputs/pop_weighted_accessibility.csv")
VIIRS_nighttime <- read.csv("J:/Treatment_Seeking/Data/weighted_rate_outputs/viirs_nighttime_lights_2012_pop_weighted.csv")
# Population averaged -> average travel time to city per capita. 

# Read in treatment seeking data:

national_data <- read.csv("Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/NationalSurveyResults_Weighted_With_UpdatedIndicators.csv") # ADMIN0. 
str(national_data)

subnational_data <- read.csv("J:/Treatment_Seeking/Data/subnational_TS.csv")

########## ---------------------- 1. CREATE FULL ORIGINAL DATASETS -------------------- ############

# ------------ Note: BD2011 and SN2008 have no svy CIs. ----------- #

full_TreatSeek <- data.frame("ISO2" = NA, "ISO3" = NA, "GAUL_Code" = NA, "Country_Name" = NA, "Admin_Unit_Level" = NA, "Admin_Unit_Name" = NA, "WHO_Region" = NA, "WHO_Subregion" = NA, 
                                          "IHME_location_id" = NA, "IHME_Super_Region_ID" = NA, "IHME_Super_Region_Name" = NA, "IHME_Region_ID" = NA, "IHME_Region_Name" = NA, 
                                          "Year" = NA, "SurveyName" = NA, "HMIS_treat" = NA, "HMIS_treat_low_SVY" = NA, "HMIS_treat_high_SVY" = NA, "Any_treat" = NA, "Any_treat_low_SVY" = NA, "Any_treat_high_SVY" = NA, 
                                          "ANC1_coverage_prop" = NA, "ANC4_coverage_prop" = NA, "DTP3_coverage_prop" = NA, "hospital_beds_per1000" = NA, "IFD_coverage_prop" = NA, 
                                          "LDI_pc" = NA, "measles_vacc_cov_prop" = NA, "SBA_coverage_prop" = NA, "educ_yrs_age_std_pc_1" = NA, "educ_yrs_age_std_pc_2" = NA,"GDPpc_id_b2010" = NA, "prop_urban" = NA, "oop_hexp_cap" = NA, 
                                          "frac_oop_hexp" = NA, "universal_health_coverage" = NA, "haqi" = NA, "measles_vacc_cov_prop_2" = NA, "ind_health" = NA, "education_all_ages_and_sexes_pc" = NA,
                                          "log_the_pc" = NA, "DMSP_nighttime" = NA, "accessibility" = NA, "VIIRS_nighttime" = NA)

# Fill in ADMIN0 data first:

for (i in 1:length(country_list)){
  country_iso <- country_list[i]
  country_id <- config_file$IHME_location_id[config_file$ISO3 == country_iso]
  if (is.na(country_id)){
    if (country_iso == "GUF"){
      country_id <- config_file$IHME_location_id[config_file$ISO3 == "SUR"] # GUF uses Suriname IHME covariates for now.
      end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == 338], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == 338], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == 338])
    }
    if (country_iso == "MYT"){
      country_id <- config_file$IHME_location_id[config_file$ISO3 == "COM"] # MYT uses Comoros IHME covariates for now.
      end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == 364], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == 364], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == 338])
    }
  }else{end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == country_id], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == country_id], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == country_id])}
  front_matter_1 <- config_file[config_file$ISO3 == country_iso, ][, c("ISO2", "ISO3", "GAUL_Code", "MAP_Country_Name")]
  front_matter_2 <- config_file[config_file$ISO3 == country_iso, ][, c("WHO_Region", "WHO_Subregion", "IHME_location_id", "IHME_Super_Region_ID", "IHME_Super_Region_Name", "IHME_Region_ID",
                                                                       "IHME_Region_Name")]
  front_matter <- cbind(front_matter_1, "ADMIN0", front_matter_1$MAP_Country_Name, front_matter_2)
  names(front_matter) <- names(full_TreatSeek)[1:ncol(front_matter)] 
  middle_matter <- rep(NA, 8)
  names(middle_matter) <- c("Year", "SurveyName", "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY")
  cov_matter <- rep(NA, 20)
  names(cov_matter) <- names(full_TreatSeek)[22:41]
  for (j in 1:length(years)){
    temp_middle_matter <- c(years[j], rep(NA, 7))
    if (country_iso == "TLS"){
      if (sum(subnational_data$Admin_Unit_Name == "East Timor" & subnational_data$SurveyYear == years[j], na.rm = TRUE)>0){
        country_TS <- na.omit(subnational_data[subnational_data$Admin_Unit_Name == "East Timor" & subnational_data$SurveyYear == years[j], ])
        temp_middle_matter <- country_TS[1, c("SurveyYear", "SurveyName", "fever_public_treat_wt.regionprop", "fever_public_treat_wt.regionprop_LCI_SVY", "fever_public_treat_wt.regionprop_UCI_SVY", "fever_any_treat_wt.regionprop", "fever_any_treat_wt.regionprop_LCI_SVY", "fever_any_treat_wt.regionprop_UCI_SVY")] 
      }
    }else{
      if (sum(national_data$ISO3 == as.character(country_iso) & national_data$Year == years[j])>0){
        country_TS <- national_data[national_data$ISO3 == as.character(country_iso) & national_data$Year == years[j], ]
        # E.g. DOM has two surveys in 2013. This will be reflected in the dataframe used for fitting but not in this dataframe.
        temp_middle_matter <- country_TS[1, c("Year", "SurveyName", "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY")] 
      }
    }
    names(temp_middle_matter) <- c("Year", "SurveyName", "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY")
    middle_matter <- rbind(middle_matter, temp_middle_matter)
    year_entry <- NA # Dynamic covariate year entry.
    for (k in 1:length(cov_names)){
      cov_values <- cov_list[[k]]
      if (length(unique(cov_values$sex_id)) > 1){
        year_entry <- append(year_entry, c(cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"], cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"]))
      }else{year_entry <- append(year_entry, cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j]])}
    }
    cov_matter <- rbind(cov_matter, year_entry[-1])
  }
  middle_matter <- middle_matter[-1, ]
  cov_matter <- cov_matter[-1, ]
  new_country_TS <- cbind(front_matter, middle_matter, cov_matter, end_matter)
  full_TreatSeek <- rbind(full_TreatSeek, new_country_TS)
}

full_TreatSeek <- full_TreatSeek[-1, ]
i # 106 if no problem.

full_TreatSeek$ISO2[full_TreatSeek$ISO3 == "NAM"] <- "NA"


# Add subnational data:

for (i in 1:nrow(subnational_list)){
  unit_name <- subnational_list$Admin_Unit_Name[i]
  unit_id <- subnational_list$IHME_location_id[i]
  if (unit_name %in% six_minor_terr){
    unit_id_2 <- 44538 # For IHME covariates.
  }else{unit_id_2 <- unit_id}
  unit_iso <- as.character(subnational_list$ISO3[i])
  end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == unit_id], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == unit_id], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == unit_id])
  front_matter_1 <- config_file[config_file$ISO3 == unit_iso, ][, c("ISO2", "ISO3", "GAUL_Code", "MAP_Country_Name")]
  front_matter_2 <- config_file[config_file$ISO3 == unit_iso, ][, c("WHO_Region", "WHO_Subregion")]
  front_matter_3 <- config_file[config_file$ISO3 == unit_iso, ][, c("IHME_Super_Region_ID", "IHME_Super_Region_Name", "IHME_Region_ID",
                                                                    "IHME_Region_Name")]
  front_matter <- cbind(front_matter_1, "ADMIN1", unit_name, front_matter_2, unit_id, front_matter_3)
  names(front_matter) <- names(full_TreatSeek)[1:ncol(front_matter)]
  middle_matter <- rep(NA, 8)
  names(middle_matter) <- c("Year", "SurveyName", "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY")
  cov_matter <- rep(NA, 20)
  names(cov_matter) <- names(full_TreatSeek)[22:41]
  for (j in 1:length(years)){
    temp_middle_matter <- c(years[j], rep(NA, 7))
    year_entry <- NA
    if (sum(subnational_data$Admin_Unit_Name == as.character(unit_name) & subnational_data$SurveyYear == years[j], na.rm = TRUE)>0){
      unit_TS <- na.omit(subnational_data[subnational_data$Admin_Unit_Name == as.character(unit_name) & subnational_data$SurveyYear == years[j], ])
      temp_middle_matter <- unit_TS[ , c("SurveyYear", "SurveyName", "fever_public_treat_wt.regionprop", "fever_public_treat_wt.regionprop_LCI_SVY", "fever_public_treat_wt.regionprop_UCI_SVY", "fever_any_treat_wt.regionprop", "fever_any_treat_wt.regionprop_LCI_SVY", "fever_any_treat_wt.regionprop_UCI_SVY")] 
    }
    names(temp_middle_matter) <- c("Year", "SurveyName", "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY")
    middle_matter <- rbind(middle_matter, temp_middle_matter)
    for (k in 1:length(cov_names)){
      cov_values <- cov_list[[k]]
      if (length(unique(cov_values$sex_id)) > 1){
        year_entry <- append(year_entry, c(cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"], cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"]))
      }else{year_entry <- append(year_entry, cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]])}
    }
    cov_matter <- rbind(cov_matter, year_entry[-1])
  }
  cov_matter <- cov_matter[-1, ]
  middle_matter <- middle_matter[-1, ]
  new_unit_TS <- cbind(front_matter, middle_matter, cov_matter, end_matter)
  full_TreatSeek <- rbind(full_TreatSeek, new_unit_TS)
}

i # Should be 211 if OK.

nrow(full_TreatSeek) == (length(country_list) + nrow(subnational_list)) *length(years)

# Add column for Six Minor Territories factor:

full_SixMinorTerr_Factor <- rep("Non_SMT", nrow(full_TreatSeek))
full_SixMinorTerr_Factor[full_TreatSeek$Admin_Unit_Name %in% six_minor_terr] <- full_TreatSeek$Admin_Unit_Name[full_TreatSeek$Admin_Unit_Name %in% six_minor_terr]
unique(full_SixMinorTerr_Factor)
full_TreatSeek$SMT_Factor <- full_SixMinorTerr_Factor

str(full_TreatSeek)

write.csv(full_TreatSeek, paste(data.path, "full_TreatSeek.csv", sep = ""))

# ------------- Re-read in full_TreatSeek --------- #

full_TreatSeek <- read.csv(paste(data.path, "full_TreatSeek.csv", sep = ""))

str(full_TreatSeek) # Reading in again creates factors as required.

tail(full_TreatSeek[full_TreatSeek$Admin_Unit_Name == "Daman & Diu", ])

# Correct NA for Nambia's ISO2:
full_TreatSeek$ISO2 <- as.character(full_TreatSeek$ISO2)
full_TreatSeek$ISO2[is.na(full_TreatSeek$ISO2)] <- "NA" 
full_TreatSeek$ISO2 <- as.factor(full_TreatSeek$ISO2)

# Check:
sum(is.na(full_TreatSeek$ANC1_coverage_prop))
sum(is.na(full_TreatSeek$accessibility))

# Normalise covariates:

full_TreatSeek_n <- full_TreatSeek

# Check the distributions of the covariates before normalisation:

boxcox(full_TreatSeek$LDI_pc ~ 1) # Use log/transformation that corresponds to the nearest lambda integer.

pdf(paste(graphics.path, 'covariate_transformations_prior_norm.pdf', sep = ''), width = 12, height = 8)

par(mfrow = c(2, 3))
hist(full_TreatSeek$ANC1_coverage_prop^2)
hist(full_TreatSeek$ANC4_coverage_prop)
hist(full_TreatSeek$DTP3_coverage_prop)
hist(log(full_TreatSeek$hospital_beds_per1000))
hist(full_TreatSeek$IFD_coverage_prop)
hist(log(full_TreatSeek$LDI_pc))


hist(full_TreatSeek$measles_vacc_cov_prop^2)
hist(full_TreatSeek$SBA_coverage_prop)
hist(log(full_TreatSeek$GDPpc_id_b2010))
hist(full_TreatSeek$prop_urban)
hist(log(full_TreatSeek$oop_hexp_cap))
hist(full_TreatSeek$frac_oop_hexp)

hist(log(full_TreatSeek$ind_health))
hist(full_TreatSeek$education_all_ages_and_sexes_pc)
hist(full_TreatSeek$log_the_pc) # Don't double log - doesn't look too bad.
hist(log(full_TreatSeek$DMSP_nighttime))
hist(log(full_TreatSeek$accessibility))
hist(log(full_TreatSeek$VIIRS_nighttime))

dev.off()

# Transform prior normalisation:
full_TreatSeek_n$ANC1_coverage_prop <- full_TreatSeek_n$ANC1_coverage_prop^2
full_TreatSeek_n$hospital_beds_per1000 <- log(full_TreatSeek_n$hospital_beds_per1000)
full_TreatSeek_n$LDI_pc <- log(full_TreatSeek_n$LDI_pc)
full_TreatSeek_n$GDPpc_id_b2010 <- log(full_TreatSeek_n$GDPpc_id_b2010)
full_TreatSeek_n$oop_hexp_cap <- log(full_TreatSeek_n$oop_hexp_cap)
full_TreatSeek_n$ind_health <- log(full_TreatSeek_n$ind_health)
full_TreatSeek_n$DMSP_nighttime <- log(full_TreatSeek_n$DMSP_nighttime)
full_TreatSeek_n$accessibility <- log(full_TreatSeek_n$accessibility)
full_TreatSeek_n$VIIRS_nighttime <- log(full_TreatSeek_n$VIIRS_nighttime)

full_TreatSeek_n[, 25:45] <- apply(full_TreatSeek_n[, 25:45], MARGIN = 2, FUN = function(x){(x - mean(x))/sd(x)}) 

summary(full_TreatSeek_n)

########## ---------------------- 2. CREATE CLEAN SUBSETS WITH TS DATA FOR MODELLING -------------------- ############

# ------------- Check number of IHME  regions in TS data -------- #

clean_TreatSeek_Any <- full_TreatSeek_n[!is.na(full_TreatSeek_n$Any_treat), ]
clean_TreatSeek_HMISfrac <- clean_TreatSeek_Any[!is.na(clean_TreatSeek_Any$HMIS_treat/clean_TreatSeek_Any$Any_treat)  & (clean_TreatSeek_Any$HMIS_treat != clean_TreatSeek_Any$Any_treat), ] # To be able to do logit transform later. logit has a nice interpretation and leads to values between 0 and 1. Non-normality could be due to underlying covariates.
clean_TreatSeek_HMISfrac$HMIS_frac <- clean_TreatSeek_HMISfrac$HMIS_treat/clean_TreatSeek_HMISfrac$Any_treat

clean_TreatSeek_Any$logit_Any <- logit(clean_TreatSeek_Any$Any_treat)
clean_TreatSeek_HMISfrac$logit_HMIS <- logit(clean_TreatSeek_HMISfrac$HMIS_frac)

clean_TreatSeek_Any <- clean_TreatSeek_Any[is.finite(clean_TreatSeek_Any$logit_Any), ]
clean_TreatSeek_HMISfrac <- clean_TreatSeek_HMISfrac[is.finite(clean_TreatSeek_HMISfrac$logit_HMIS), ]

summary(clean_TreatSeek_Any$IHME_Region_Name)
summary(clean_TreatSeek_HMISfrac$IHME_Region_Name)

# Set the IHME Region of GUF to be that of Suriname, set the IHME Region of MYT to be that of COM. 

full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$ISO3 == 'GUF'] <- unique(full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$ISO3 == 'SUR'])
full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$ISO3 == 'MYT'] <- unique(full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$ISO3 == 'COM'])

# Compare income of IHME regions so as to match High-income superregion to the closest region:

# IHME_Regions <- na.omit(unique(full_TreatSeek_n$IHME_Region_Name))
# 
# proxy_data <- data.frame('Region' = IHME_Regions, 'Proxy_mean_1' = NA, 'Proxy_mean_2' = NA, 'Any_mean' = NA, 'HMIS_frac_mean' = NA)
# 
# for (i in 1:length(IHME_Regions)){
#   proxy_data$Proxy_mean_1[i] <- mean(full_TreatSeek_n$GDPpc_id_b2010[full_TreatSeek_n$IHME_Region_Name == IHME_Regions[i]])
#   proxy_data$Proxy_mean_2[i] <- mean(full_TreatSeek_n$LDI_pc[full_TreatSeek_n$IHME_Region_Name == IHME_Regions[i]])
#   proxy_data$Any_mean[i] <- mean(full_TreatSeek_n$Any_treat[full_TreatSeek_n$IHME_Region_Name == IHME_Regions[i]], na.rm= TRUE)
#   proxy_data$HMIS_frac_mean[i] <- mean(full_TreatSeek_n$HMIS_treat[full_TreatSeek_n$IHME_Region_Name == IHME_Regions[i]]/full_TreatSeek_n$Any_treat[full_TreatSeek_n$IHME_Region_Name == IHME_Regions[i]], na.rm = TRUE)
# }
# 
# par(mfrow = c(1, 3))
# plot(proxy_data[, c(2, 3)], main = 'Similarity of IHME Regions')
# text(proxy_data[, c(2, 3)], labels = proxy_data[, 1])
# plot(proxy_data[, c(2, 4)], col = 'red', main = 'Proxies VS Any_mean')
# points(proxy_data[, c(3, 4)], col = 'blue')
# legend('topright', col = c('red','blue'), legend  = c('Proxy_mean_1', 'Proxy_mean_2'), pch = c(1,1))
# plot(proxy_data[, c(2, 5)], col = 'red', main = 'Proxies VS HMIS_frac_mean')
# points(proxy_data[, c(3, 5)], col = 'blue')
# legend('topright', col = c('red','blue'), legend  = c('Proxy_mean_1', 'Proxy_mean_2'), pch = c(1,1))

# Set East Asia and Oceania to South East Asia since they are in the same IHME superregion:

full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$IHME_Region_Name == 'East Asia'] <- 'Southeast Asia'
full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$IHME_Region_Name == 'Oceania'] <- 'Southeast Asia'

# Set High-income Asia to South East Asia since it is the closest geographically; set Southern Latin America to Tropical Latin America because it has higher income per capita compared to Andean Latin America. 

full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$IHME_Region_Name == 'High-income Asia Pacific'] <- 'Southeast Asia'
full_TreatSeek_n$IHME_Region_Name[full_TreatSeek_n$IHME_Region_Name == 'Southern Latin America'] <- 'Tropical Latin America'

summary(full_TreatSeek_n$IHME_Region_Name)

full_TreatSeek_n$IHME_Region_Name <- droplevels(full_TreatSeek_n$IHME_Region_Name)

write.csv(full_TreatSeek_n, paste(data.path, "full_TreatSeek_n.csv", sep = ""))


######### ---------------------- 3. CREATE TRAINING AND TEST SETS FOR CHOOSING MODELS -------------------- ############

# Read in cleaned datasets to remove pre-1990 data and outliers before splitting into training and test sets:
# clean_TreatSeek_Any <- read.csv(paste(data.path, "clean_TreatSeek_Any.csv", sep = ""))
# clean_TreatSeek_HMISfrac <- read.csv(paste(data.path, "clean_TreatSeek_HMISfrac.csv", sep = ""))


# Stratified sampling according to IHME Region Name and include all SMT to ensure that we can estimate the factor levels well. 

clean_TreatSeek_Any$logit_Any <- logit(clean_TreatSeek_Any$Any_treat)
clean_TreatSeek_HMISfrac$logit_HMIS <- logit(clean_TreatSeek_HMISfrac$HMIS_frac)

clean_TreatSeek_Any <- clean_TreatSeek_Any[is.finite(clean_TreatSeek_Any$logit_Any), ]
clean_TreatSeek_HMISfrac <- clean_TreatSeek_HMISfrac[is.finite(clean_TreatSeek_HMISfrac$logit_HMIS), ]

write.csv(clean_TreatSeek_Any, paste(data.path, 'clean_TreatSeek_Any.csv', sep = ''))
write.csv(clean_TreatSeek_HMISfrac, paste(data.path, 'clean_TreatSeek_HMISfrac.csv', sep = ''))

# ------------------- Remove pre-1990 data (and outliers) and create training/test sets -----------------

clean_TreatSeek_Any <- read.csv(paste(data.path, 'clean_TreatSeek_Any.csv', sep = ''))
clean_TreatSeek_HMISfrac <- read.csv(paste(data.path, 'clean_TreatSeek_HMISfrac.csv', sep = ''))

clean_TreatSeek_Any <- clean_TreatSeek_Any[clean_TreatSeek_Any$Year > 1989, ]
clean_TreatSeek_HMISfrac <- clean_TreatSeek_HMISfrac[clean_TreatSeek_HMISfrac$Year > 1989, ]

Any_outliers <- which((clean_TreatSeek_Any$ISO3 == "IND" & clean_TreatSeek_Any$Year == 1993) | (clean_TreatSeek_Any$ISO3 == "PAK" & clean_TreatSeek_Any$Year == 1991) | (clean_TreatSeek_Any$ISO3 == "COL" & clean_TreatSeek_Any$Year == 1990))
HMIS_outliers <- which((clean_TreatSeek_HMISfrac$ISO3 == "IND" & clean_TreatSeek_HMISfrac$Year == 1993) | (clean_TreatSeek_HMISfrac$ISO3 == "PAK" & clean_TreatSeek_HMISfrac$Year == 1991) | (clean_TreatSeek_HMISfrac$ISO3 == "COL" & clean_TreatSeek_HMISfrac$Year == 1990))

clean_TreatSeek_Any <- clean_TreatSeek_Any[-Any_outliers, ]
clean_TreatSeek_HMISfrac <- clean_TreatSeek_HMISfrac[-HMIS_outliers, ]

summary(clean_TreatSeek_Any$IHME_Region_Name)
summary(clean_TreatSeek_HMISfrac$IHME_Region_Name)

master.region.list <- unique(clean_TreatSeek_Any$IHME_Region_Name)

set.seed(1)
train.id <- numeric()
for (i in 1:length(master.region.list)){
  region.id <- which(clean_TreatSeek_Any$IHME_Region_Name == master.region.list[i])
  if (length(region.id) > 50){
    no.sample <- ceiling(length(region.id)*0.7)
    temp.id <- sample(region.id, no.sample)
  }else{temp.id <- region.id}
  train.id <- c(train.id, temp.id)
}

length(train.id)/nrow(clean_TreatSeek_Any) # 0.7477876

train_data_Any <- clean_TreatSeek_Any[train.id, ]
  
summary(train_data_Any$IHME_Region_Name)
summary(train_data_Any$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_data_Any$Admin_Unit_Name)){
    train_data_Any <- rbind(train_data_Any, clean_TreatSeek_Any[clean_TreatSeek_Any$Admin_Unit_Name == six_minor_terr[i], ]) # Only have one data_Any point per SMT at the moment.
  }
}

summary(train_data_Any$SMT_Factor)
nrow(train_data_Any) # 336
train_data_Any <- as.data.frame(train_data_Any)

test_data_Any <- clean_TreatSeek_Any[!(clean_TreatSeek_Any$X %in% train_data_Any$X), ]
nrow(test_data_Any) # 111

summary(test_data_Any$IHME_Region_Name)
summary(train_data_Any$IHME_Region_Name)

set.seed(1)
train.id <- numeric()
for (i in 1:length(master.region.list)){
  region.id <- which(clean_TreatSeek_HMISfrac$IHME_Region_Name == master.region.list[i])
  if (length(region.id) > 50){
    no.sample <- ceiling(length(region.id)*0.7)
    temp.id <- sample(region.id, no.sample)
  }else{temp.id <- region.id}
  train.id <- c(train.id, temp.id)
}

length(train.id)/nrow(clean_TreatSeek_HMISfrac) # 0.7450549

train_data_HMISfrac <- clean_TreatSeek_HMISfrac[train.id, ]

summary(train_data_HMISfrac$IHME_Region_Name)
summary(train_data_HMISfrac$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_data_HMISfrac$Admin_Unit_Name)){
    train_data_HMISfrac <- rbind(train_data_HMISfrac, clean_TreatSeek_HMISfrac[(clean_TreatSeek_HMISfrac$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_HMISfrac point per SMT at the moment.
  }
}

summary(train_data_HMISfrac$SMT_Factor)
nrow(train_data_HMISfrac) # 343
train_data_HMISfrac <- as.data.frame(train_data_HMISfrac)

test_data_HMISfrac <- clean_TreatSeek_HMISfrac[!(clean_TreatSeek_HMISfrac$X %in% train_data_HMISfrac$X), ]
nrow(test_data_HMISfrac) # 113

summary(test_data_HMISfrac$IHME_Region_Name)
summary(train_data_HMISfrac$IHME_Region_Name)


#  Set SMT_Factor base level to Non_SMT:
train_data_Any$SMT_Factor <- relevel(train_data_Any$SMT_Factor, ref = "Non_SMT")
train_data_HMISfrac$SMT_Factor <- relevel(train_data_HMISfrac$SMT_Factor, ref = "Non_SMT")

# Save the training and test sets:
write.csv(train_data_Any, paste(data.path, 'train_data_Any.csv', sep = ''))
write.csv(train_data_HMISfrac, paste(data.path, 'train_data_HMISfrac.csv', sep = ''))
write.csv(test_data_Any, paste(data.path, 'test_data_Any.csv', sep = ''))
write.csv(test_data_HMISfrac, paste(data.path, 'test_data_HMISfrac.csv', sep = ''))



########## ---------------------- 4. CREATE 100 FULL DATASETS TO ACCOUNT FOR TS AND IHME COVARIATE VARIABILITY -------------------- ############

# These are alternative full_TreatSeek_n datasets for fitting and predicting.

N_TS <- 100

TS_datasets <- rep(list(NA), N_TS)

names(TS_datasets) <- paste("Dataset", 1:N_TS, sep = "_")

temptime3 <- proc.time()[3]

for (TS_i in 1:N_TS){
  set.seed(TS_i)
  
  full_TreatSeek_i <- data.frame("ISO3" = NA, "GAUL_Code" = NA, "Country_Name" = NA, "Admin_Unit_Level" = NA, "Admin_Unit_Name" = NA, 
                                 "IHME_location_id" = NA, "IHME_Region_Name" = NA, 
                                 "Year" = NA, "SurveyName" = NA, "HMIS_treat" = NA, "Any_treat" = NA, 
                                 "ANC1_coverage_prop" = NA, "ANC4_coverage_prop" = NA, "DTP3_coverage_prop" = NA, "hospital_beds_per1000" = NA, "IFD_coverage_prop" = NA, 
                                 "LDI_pc" = NA, "measles_vacc_cov_prop" = NA, "SBA_coverage_prop" = NA, "educ_yrs_age_std_pc_1" = NA, "educ_yrs_age_std_pc_2" = NA,"GDPpc_id_b2010" = NA, "prop_urban" = NA, "oop_hexp_cap" = NA, 
                                 "frac_oop_hexp" = NA, "universal_health_coverage" = NA, "haqi" = NA, "measles_vacc_cov_prop_2" = NA, "ind_health" = NA, "education_all_ages_and_sexes_pc" = NA,
                                 "log_the_pc" = NA, "DMSP_nighttime" = NA, "accessibility" = NA, "VIIRS_nighttime" = NA)
  
  # Fill in ADMIN0 data first:
  
  for (i in 1:length(country_list)){
    country_iso <- country_list[i]
    country_id <- config_file$IHME_location_id[config_file$ISO3 == country_iso]
    if (is.na(country_id)){
      if (country_iso == "GUF"){
        country_id <- config_file$IHME_location_id[config_file$ISO3 == "SUR"] # GUF uses Suriname IHME covariates for now.
        end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == 338], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == 338], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == 338])
      }
      if (country_iso == "MYT"){
        country_id <- config_file$IHME_location_id[config_file$ISO3 == "COM"] # MYT uses Comoros IHME covariates for now.
        end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == 364], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == 364], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == 338])
      }
    }else{end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == country_id], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == country_id], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == country_id])}
    front_matter_1 <- config_file[config_file$ISO3 == country_iso, ][, c("ISO3", "GAUL_Code", "MAP_Country_Name")]
    front_matter_2 <- config_file[config_file$ISO3 == country_iso, ][, c("IHME_location_id", "IHME_Region_Name")]
    front_matter <- cbind(front_matter_1, "ADMIN0", front_matter_1$MAP_Country_Name, front_matter_2)
    names(front_matter) <- names(full_TreatSeek_i)[1:ncol(front_matter)] 
    middle_matter <- data.frame("Year" = NA, "SurveyName" = NA, "HMIS_treat" = NA, "Any_treat" = NA)
    cov_matter <- rep(NA, 20)
    names(cov_matter) <- names(full_TreatSeek_i)[12:31]
    for (j in 1:length(years)){
      temp_middle_matter <- data.frame("Year" = years[j], "SurveyName" = NA, "HMIS_treat" = NA, "Any_treat" = NA)
      if (country_iso == "TLS"){
        if (sum(subnational_data$Admin_Unit_Name == "East Timor" & subnational_data$SurveyYear == years[j], na.rm = TRUE)>0){
          country_TS <- na.omit(subnational_data[subnational_data$Admin_Unit_Name == "East Timor" & subnational_data$SurveyYear == years[j], ])
          temp_middle_matter[2] <- as.character(country_TS[1, c("SurveyName")])
          if (country_TS[1, c("fever_public_treat_wt.regionprop_UCI_SVY")] !=  country_TS[1, c("fever_public_treat_wt.regionprop_LCI_SVY")] & !is.na(country_TS[1, c("fever_public_treat_wt.regionprop_LCI_SVY")])){
            temp_middle_matter[3] <- rnorm(1, mean = country_TS[1, c("fever_public_treat_wt.regionprop")], sd = (country_TS[1, c("fever_public_treat_wt.regionprop_UCI_SVY")] - country_TS[1, c("fever_public_treat_wt.regionprop_LCI_SVY")])/(2*qnorm(0.975)))
          }else{temp_middle_matter[3] <- country_TS[1, c("fever_public_treat_wt.regionprop")]}
          if (country_TS[1, c("fever_any_treat_wt.regionprop_UCI_SVY")] !=  country_TS[1, c("fever_any_treat_wt.regionprop_LCI_SVY")] & !is.na(country_TS[1, c("fever_any_treat_wt.regionprop_LCI_SVY")])){
            temp_middle_matter[4] <- rnorm(1, mean = country_TS[1, c("fever_any_treat_wt.regionprop")], sd = (country_TS[1, c("fever_any_treat_wt.regionprop_UCI_SVY")] - country_TS[1, c("fever_any_treat_wt.regionprop_LCI_SVY")])/(2*qnorm(0.975)))
          }else{temp_middle_matter[4] <- country_TS[1, c("fever_any_treat_wt.regionprop")]}
        }
      }else{
        if (sum(national_data$ISO3 == as.character(country_iso) & national_data$Year == years[j])>0){
          country_TS <- national_data[national_data$ISO3 == as.character(country_iso) & national_data$Year == years[j], ]
          # E.g. DOM has two surveys in 2013. This will be reflected in the dataframe used for fitting but not in this dataframe.
          temp_middle_matter[2] <- as.character(country_TS[1, c("SurveyName")])
          if (country_TS[1, c("HMIS_treat_high_SVY")] !=  country_TS[1, c("HMIS_treat_low_SVY")] & !is.na(country_TS[1, c("HMIS_treat_low_SVY")])){
            temp_middle_matter[3] <- rnorm(1, mean = country_TS[1, c("HMIS_treat")], sd = (country_TS[1, c("HMIS_treat_high_SVY")] - country_TS[1, c("HMIS_treat_low_SVY")])/(2*qnorm(0.975)))
          }else{temp_middle_matter[3] <- country_TS[1, c("HMIS_treat")]}
          if (country_TS[1, c("Any_treat_high_SVY")] !=  country_TS[1, c("Any_treat_low_SVY")] & !is.na(country_TS[1, c("Any_treat_low_SVY")])){
            temp_middle_matter[4] <- rnorm(1, mean = country_TS[1, c("Any_treat")], sd = (country_TS[1, c("Any_treat_high_SVY")] - country_TS[1, c("Any_treat_low_SVY")])/(2*qnorm(0.975)))
          }else{temp_middle_matter[4] <- country_TS[1, c("Any_treat")]}
        }
      }
      middle_matter <- rbind(middle_matter, temp_middle_matter)
      year_entry <- NA # Dynamic covariate year entry.
      for (k in 1:length(cov_names)){
        cov_values <- cov_list[[k]]
        if (length(unique(cov_values$sex_id)) > 1){
          if (cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"] != cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"] & !is.na(cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"])){
            first.realisation <- rnorm(1, mean = cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"], sd = (cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"] - cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j]] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages")/(2*qnorm(0.975)))
          }else{first.realisation <- cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"]}
          if (cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"] != cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"] & !is.na(cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"])){
            second.realisation <- rnorm(1, mean = cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"], sd = (cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"] - cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j]] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages")/(2*qnorm(0.975)))
          }else{second.realisation <- cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"]}
          year_entry <- append(year_entry, c(first.realisation, second.realisation))
        }else{
          if (cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j]] != cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j]] & !is.na(cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j]])){
            year_entry <- append(year_entry, rnorm(1, mean = cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j]], sd = (cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j]] - cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j]])/(2*qnorm(0.975))))
          }else{year_entry <- append(year_entry, cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j]])}
        }
      }
      cov_matter <- rbind(cov_matter, year_entry[-1])
    }
    middle_matter <- middle_matter[-1, ]
    cov_matter <- cov_matter[-1, ]
    new_country_TS <- cbind(front_matter, middle_matter, cov_matter, end_matter)
    full_TreatSeek_i <- rbind(full_TreatSeek_i, new_country_TS)
  }
  
  full_TreatSeek_i <- full_TreatSeek_i[-1, ]
  i # 106 if no problem.
  
   # Add subnational data:
  
  for (i in 1:nrow(subnational_list)){
    unit_name <- subnational_list$Admin_Unit_Name[i]
    unit_id <- subnational_list$IHME_location_id[i]
    if (unit_name %in% six_minor_terr){
      unit_id_2 <- 44538 # For IHME covariates.
    }else{unit_id_2 <- unit_id}
    unit_iso <- as.character(subnational_list$ISO3[i])
    end_matter <- data.frame("DMSP_nighttime" = DMSP_nighttime$weighted_rate[DMSP_nighttime$loc_id == unit_id], "accessibility" =  accessibility$weighted_rate[accessibility$loc_id == unit_id], "VIIRS_nighttime" = VIIRS_nighttime$weighted_rate[VIIRS_nighttime$loc_id == unit_id])
    front_matter_1 <- config_file[config_file$ISO3 == unit_iso, ][, c("ISO3", "GAUL_Code", "MAP_Country_Name")]
    front_matter_2 <- config_file[config_file$ISO3 == unit_iso, ][, c("IHME_Region_Name")]
    front_matter <- cbind(front_matter_1, "ADMIN1", unit_name, unit_id, front_matter_2)
    names(front_matter) <- names(full_TreatSeek_i)[1:ncol(front_matter)]
    middle_matter <- data.frame("Year" = NA, "SurveyName" = NA, "HMIS_treat" = NA, "Any_treat" = NA)
    cov_matter <- rep(NA, 20)
    names(cov_matter) <- names(full_TreatSeek_i)[12:31]
    for (j in 1:length(years)){
      temp_middle_matter <- data.frame("Year" = years[j], "SurveyName" = NA, "HMIS_treat" = NA, "Any_treat" = NA)
      year_entry <- NA
      if (sum(subnational_data$Admin_Unit_Name == as.character(unit_name) & subnational_data$SurveyYear == years[j], na.rm = TRUE)>0){
        unit_TS <- na.omit(subnational_data[subnational_data$Admin_Unit_Name == as.character(unit_name) & subnational_data$SurveyYear == years[j], ])
        temp_middle_matter[2] <- unit_TS[ , c("SurveyName")]
        if (unit_TS[1, c("fever_public_treat_wt.regionprop_UCI_SVY")] !=  unit_TS[1, c("fever_public_treat_wt.regionprop_LCI_SVY")] & !is.na(unit_TS[1, c("fever_public_treat_wt.regionprop_LCI_SVY")])){
          temp_middle_matter[3] <- rnorm(1, mean = unit_TS[1, c("fever_public_treat_wt.regionprop")], sd = (unit_TS[1, c("fever_public_treat_wt.regionprop_UCI_SVY")] - unit_TS[1, c("fever_public_treat_wt.regionprop_LCI_SVY")])/(2*qnorm(0.975)))
        }else{temp_middle_matter[3] <- unit_TS[1, c("fever_public_treat_wt.regionprop")]}
        if (unit_TS[1, c("fever_any_treat_wt.regionprop_UCI_SVY")] !=  unit_TS[1, c("fever_any_treat_wt.regionprop_LCI_SVY")] & !is.na(unit_TS[1, c("fever_any_treat_wt.regionprop_LCI_SVY")])){
          temp_middle_matter[4] <- rnorm(1, mean = unit_TS[1, c("fever_any_treat_wt.regionprop")], sd = (unit_TS[1, c("fever_any_treat_wt.regionprop_UCI_SVY")] - unit_TS[1, c("fever_any_treat_wt.regionprop_LCI_SVY")])/(2*qnorm(0.975)))
        }else{temp_middle_matter[4] <- unit_TS[1, c("fever_any_treat_wt.regionprop")]}
      }
      middle_matter <- rbind(middle_matter, temp_middle_matter)
      for (k in 1:length(cov_names)){
        cov_values <- cov_list[[k]]
        if (length(unique(cov_values$sex_id)) > 1){
          if (cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"] != cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"] & !is.na(cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"])){
            first.realisation <- rnorm(1, mean = cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"], sd = (cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"] - cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages")/(2*qnorm(0.975)))
          }else{first.realisation <- cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 1 & cov_values$age_group_name == "All Ages"]}
          if (cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"] != cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"] & !is.na(cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"])){
            second.realisation <- rnorm(1, mean = cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"], sd = (cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"] - cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages")/(2*qnorm(0.975)))
          }else{second.realisation <- cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 2 & cov_values$age_group_name == "All Ages"]}
          year_entry <- append(year_entry, c(first.realisation, second.realisation))
        }else{
          if (cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]] != cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]] & !is.na(cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]])){
            year_entry <- append(year_entry, rnorm(1, mean = cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]], sd = (cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]] - cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]])/(2*qnorm(0.975))))
          }else{year_entry <- append(year_entry, cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j]])}
        }
      }
      cov_matter <- rbind(cov_matter, year_entry[-1])
    }
    cov_matter <- cov_matter[-1, ]
    middle_matter <- middle_matter[-1, ]
    new_unit_TS <- cbind(front_matter, middle_matter, cov_matter, end_matter)
    full_TreatSeek_i <- rbind(full_TreatSeek_i, new_unit_TS)
  }
  
  i # Should be 211 if OK.
  
  # Add column for Six Minor Territories factor:
  
  full_SixMinorTerr_Factor <- rep("Non_SMT", nrow(full_TreatSeek_i))
  full_SixMinorTerr_Factor[full_TreatSeek_i$Admin_Unit_Name %in% six_minor_terr] <- full_TreatSeek_i$Admin_Unit_Name[full_TreatSeek_i$Admin_Unit_Name %in% six_minor_terr]
  unique(full_SixMinorTerr_Factor)
  full_TreatSeek_i$SMT_Factor <- full_SixMinorTerr_Factor

  # Can check values before taking log transforms etc. 
  full_TreatSeek_i$ANC1_coverage_prop <-  sapply(full_TreatSeek_i$ANC1_coverage_prop, function(x){min(max(x, 0.0001), 1)})
  full_TreatSeek_i$ANC4_coverage_prop <-  sapply(full_TreatSeek_i$ANC4_coverage_prop, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$DTP3_coverage_prop <-  sapply(full_TreatSeek_i$DTP3, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$IFD_coverage_prop <- sapply(full_TreatSeek_i$IFD_coverage_prop, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$measles_vacc_cov_prop <-  sapply(full_TreatSeek_i$measles_vacc_cov_prop, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$measles_vacc_cov_prop_2 <-  sapply(full_TreatSeek_i$measles_vacc_cov_prop_2, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$SBA_coverage_prop <-  sapply(full_TreatSeek_i$SBA_coverage_prop, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$prop_urban <-  sapply(full_TreatSeek_i$prop_urban, function(x){min(max(x, 0), 1)})
  full_TreatSeek_i$education_all_ages_and_sexes_pc <-  sapply(full_TreatSeek_i$education_all_ages_and_sexes_pc, function(x){min(max(x, 0), 99)})
  full_TreatSeek_i$log_the_pc <- sapply(full_TreatSeek_i$log_the_pc, function(x){max(x, 0)})
  full_TreatSeek_i$hospital_beds_per1000 <-  sapply(full_TreatSeek_i$education_all_ages_and_sexes_pc, function(x){min(max(x, 0.0001), 1000)})
  full_TreatSeek_i$LDI_pc <- sapply(full_TreatSeek_i$LDI_pc, function(x){max(x, 0.0001)})
  full_TreatSeek_i$GDPpc_id_b2010 <- sapply(full_TreatSeek_i$GDPpc_id_b2010, function(x){max(x, 0.0001)})
  full_TreatSeek_i$oop_hexp_cap <- sapply(full_TreatSeek_i$oop_hexp_cap, function(x){max(x, 0.0001)})
  full_TreatSeek_i$ind_health <- sapply(full_TreatSeek_i$ind_health, function(x){min(max(x, 0.0001), 1)})

  # Transform:
  full_TreatSeek_i$ANC1_coverage_prop <- full_TreatSeek_i$ANC1_coverage_prop^2
  full_TreatSeek_i$hospital_beds_per1000 <- log(full_TreatSeek_i$hospital_beds_per1000)
  full_TreatSeek_i$LDI_pc <- log(full_TreatSeek_i$LDI_pc)
  full_TreatSeek_i$GDPpc_id_b2010 <- log(full_TreatSeek_i$GDPpc_id_b2010)
  full_TreatSeek_i$oop_hexp_cap <- log(full_TreatSeek_i$oop_hexp_cap)
  full_TreatSeek_i$ind_health <- log(full_TreatSeek_i$ind_health)
  full_TreatSeek_i$DMSP_nighttime <- log(full_TreatSeek_i$DMSP_nighttime)
  full_TreatSeek_i$accessibility <- log(full_TreatSeek_i$accessibility)
  full_TreatSeek_i$VIIRS_nighttime <- log(full_TreatSeek_i$VIIRS_nighttime)
  
  full_TreatSeek_i[, 12:34] <- apply(full_TreatSeek_i[, 12:34], MARGIN = 2, FUN = function(x){(x - mean(x))/sd(x)}) 
  
  full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$ISO3 == 'GUF'] <- unique(full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$ISO3 == 'SUR'])
  full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$ISO3 == 'MYT'] <- unique(full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$ISO3 == 'COM'])
  
  # Set East Asia and Oceania to South East Asia since they are in the same IHME superregion:
  
  full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$IHME_Region_Name == 'East Asia'] <- 'Southeast Asia'
  full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$IHME_Region_Name == 'Oceania'] <- 'Southeast Asia'
  
  # Set High-income Asia to South East Asia since it is the closest geographically; set Southern Latin America to Tropical Latin America because it has higher income per capita compared to Andean Latin America. 
  
  full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$IHME_Region_Name == 'High-income Asia Pacific'] <- 'Southeast Asia'
  full_TreatSeek_i$IHME_Region_Name[full_TreatSeek_i$IHME_Region_Name == 'Southern Latin America'] <- 'Tropical Latin America'
  
  str(full_TreatSeek_i)
  
  TS_datasets[[TS_i]] <- full_TreatSeek_i 
  
  print(TS_i)
}

timetaken3 <- proc.time()[3] - temptime3

# About 20.5 hours.
save(TS_datasets, file = paste('J:/Treatment_Seeking/Data/', 'TS_datasets.RData', sep = ''))

# Check:

load(paste('J:/Treatment_Seeking/Data/', 'TS_datasets.RData', sep = ''))
full_TreatSeek_i <- TS_datasets[[3]]
summary(full_TreatSeek_i)

summary(as.factor(full_TreatSeek_i$IHME_Region_Name))
summary(as.factor(full_TreatSeek_n$IHME_Region_Name))

########## ---------------------- 5. CHECK IHME COVARIATE VARIABILITY -------------------- ############

N_cov <- length(cov_list)

for (k in c(1:8, 10:N_cov)){
  
  covariate_i <- data.frame("ISO3" = NA, "GAUL_Code" = NA, "Country_Name" = NA, "Admin_Unit_Level" = NA, "Admin_Unit_Name" = NA, 
                            "IHME_location_id" = NA, "IHME_Region_Name" = NA, "Year" = NA, "cov_mean" = NA,
                            "cov_high" = NA, "cov_low" = NA)
  cov_values <- cov_list[[k]]
  
  # Fill in ADMIN0 data first:
  for (i in 1:length(country_list)){
    country_iso <- country_list[i]
    country_id <- config_file$IHME_location_id[config_file$ISO3 == country_iso]
    front_matter_1 <- config_file[config_file$ISO3 == country_iso, ][, c("ISO3", "GAUL_Code", "MAP_Country_Name")]
    front_matter_2 <- config_file[config_file$ISO3 == country_iso, ][, c("IHME_location_id", "IHME_Region_Name")]
    front_matter <- cbind(front_matter_1, "ADMIN0", front_matter_1$MAP_Country_Name, front_matter_2)
    names(front_matter) <- names(covariate_i)[1:ncol(front_matter)] 
    end_matter <- data.frame("Year" = years, "cov_mean" = NA, "cov_high" = NA, "cov_low" = NA)
    for (j in 1:length(years)){
      end_matter$cov_mean[j] <- cov_values$mean_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 3]
      end_matter$cov_high[j] <- cov_values$upper_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 3]
      end_matter$cov_low[j] <- cov_values$lower_value[cov_values$location_id == country_id & cov_values$year_id == years[j] & cov_values$sex_id == 3]
    }
    new_country_TS <- cbind(front_matter, end_matter)
    covariate_i <- rbind(covariate_i, new_country_TS)
  }
  
  covariate_i <- covariate_i[-1, ]
  i # 106 if no problem.
  
  # Add subnational data:
  
  for (i in 1:nrow(subnational_list)){
    unit_name <- subnational_list$Admin_Unit_Name[i]
    unit_id <- subnational_list$IHME_location_id[i]
    if (unit_name %in% six_minor_terr){
      unit_id_2 <- 44538 # For IHME covariates.
    }else{unit_id_2 <- unit_id}
    unit_iso <- as.character(subnational_list$ISO3[i])
    front_matter_1 <- config_file[config_file$ISO3 == unit_iso, ][, c("ISO3", "GAUL_Code", "MAP_Country_Name")]
    front_matter_2 <- config_file[config_file$ISO3 == unit_iso, ][, c("IHME_Region_Name")]
    front_matter <- cbind(front_matter_1, "ADMIN1", unit_name, unit_id, front_matter_2)
    names(front_matter) <- names(covariate_i)[1:ncol(front_matter)]
    end_matter <- data.frame("Year" = years, "cov_mean" = NA, "cov_high" = NA, "cov_low" = NA)
    for (j in 1:length(years)){
      end_matter$cov_mean[j] <- cov_values$mean_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 3]
      end_matter$cov_high[j] <- cov_values$upper_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 3]
      end_matter$cov_low[j] <- cov_values$lower_value[cov_values$location_id == unit_id_2 & cov_values$year_id == years[j] & cov_values$sex_id == 3]
    }
    new_country_TS <- cbind(front_matter, end_matter)
    covariate_i <- rbind(covariate_i, new_country_TS)
  }
  
  i # Should be 211 if OK.
  
  covariate_i$IHME_Region_Name[covariate_i$ISO3 == 'GUF'] <- unique(covariate_i$IHME_Region_Name[covariate_i$ISO3 == 'SUR'])
  covariate_i$IHME_Region_Name[covariate_i$ISO3 == 'MYT'] <- unique(covariate_i$IHME_Region_Name[covariate_i$ISO3 == 'COM'])
  
  # Set East Asia and Oceania to South East Asia since they are in the same IHME superregion:
  
  covariate_i$IHME_Region_Name[covariate_i$IHME_Region_Name == 'East Asia'] <- 'Southeast Asia'
  covariate_i$IHME_Region_Name[covariate_i$IHME_Region_Name == 'Oceania'] <- 'Southeast Asia'
  
  # Set High-income Asia to South East Asia since it is the closest geographically; set Southern Latin America to Tropical Latin America because it has higher income per capita compared to Andean Latin America. 
  
  covariate_i$IHME_Region_Name[covariate_i$IHME_Region_Name == 'High-income Asia Pacific'] <- 'Southeast Asia'
  covariate_i$IHME_Region_Name[covariate_i$IHME_Region_Name == 'Southern Latin America'] <- 'Tropical Latin America'
  
  pdf(paste(graphics.path, names(cov_list)[k], '.pdf', sep = ''),width=8.7,height = 11.2)
  
  par(mfrow=c(3,2))
  
  for (l in 1:length(master.region.list)){
    region.data <- covariate_i[covariate_i$IHME_Region_Name == master.region.list[l], ]
    countries <- unique(region.data$ISO3)
    for (m in 1:length(countries)){
      unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[m]])
      for (n in 1:length(unit.list)){
        unit.row <- region.data[region.data$ISO3 == countries[m] & region.data$Admin_Unit_Name == unit.list[n], ]
        plot(0,0,type='n',c(min(covariate_i$cov_low, na.rm = TRUE),max(covariate_i$cov_high, na.rm = TRUE)),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[l], ': ', unit.list[n] , ", ", countries[m], sep = ''), ylab='IHME covariate',xlab='Year')
        
        plotCI(years,unit.row$cov_mean,ui=unit.row$cov_high,li=unit.row$cov_low,ylim=c(min(covariate_i$cov_low, na.rm = TRUE),max(covariate_i$cov_high, na.rm = TRUE)),add=T)
      }
    }
  }
  
  dev.off()
  
  
  print(names(cov_list)[k])
}
