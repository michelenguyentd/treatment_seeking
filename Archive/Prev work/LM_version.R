library(mgcv) # For GAM.
library(splitstackshape) # For stratified sampling on IHME regions.
library(MuMIn) # for AIC (model selection)
library(gtools) # for logit transform
library(VGAM) # for probit transform
library(survey) # for taking into account sampling weights
library(nortest) # Normality tests for residuals
library(xtable) # For exporting latex tables.
library(plotrix) # For plotCI.

rm(list = ls())
setwd('J:/Treatment_Seeking/')

graphics.path <- 'J:/Treatment_Seeking/graphics/'
data.path <- 'J:/Treatment_Seeking/Data/'

# Years to model:
years <- 1980:2017

# Read in full datasets, training and test sets:

full_TreatSeek <- read.csv(paste(data.path, "full_TreatSeek.csv", sep = ""))
full_TreatSeek_n <- read.csv(paste(data.path, "full_TreatSeek_n.csv", sep = ""))

clean_TreatSeek_Any <- read.csv(paste(data.path, "clean_TreatSeek_Any.csv", sep = ""))
clean_TreatSeek_HMISfrac <- read.csv(paste(data.path, "clean_TreatSeek_HMISfrac.csv", sep = ""))

clean_TreatSeek_Any$Admin_Unit_Name <- as.character(clean_TreatSeek_Any$Admin_Unit_Name)
clean_TreatSeek_HMISfrac$Admin_Unit_Name <- as.character(clean_TreatSeek_HMISfrac$Admin_Unit_Name)

train_data_Any <- read.csv(paste(data.path, 'train_data_Any.csv', sep = ''))
train_data_HMISfrac <- read.csv(paste(data.path, 'train_data_HMISfrac.csv', sep = ''))
test_data_Any <- read.csv(paste(data.path, 'test_data_Any.csv', sep = ''))
test_data_HMISfrac <- read.csv(paste(data.path, 'test_data_HMISfrac.csv', sep = ''))

#  Add Time_Factor to full datasets:

full_TreatSeek$Time_Factor <- 'Other'
full_TreatSeek$Time_Factor[full_TreatSeek$IHME_Region_Name %in% c('Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa')] <- 'East, West Africa'
# full_TreatSeek$Time_Factor[full_TreatSeek$IHME_Region_Name == 'South Asia'] <- 'South Asia'
full_TreatSeek$Time_Factor <- as.factor(full_TreatSeek$Time_Factor)
unique(full_TreatSeek$Time_Factor)
full_TreatSeek$Time_Factor <- relevel(full_TreatSeek$Time_Factor, ref = 'Other')

full_TreatSeek_n$Time_Factor <- 'Other'
full_TreatSeek_n$Time_Factor[full_TreatSeek_n$IHME_Region_Name %in% c('Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa')] <- 'East, West Africa'
# full_TreatSeek_n$Time_Factor[full_TreatSeek_n$IHME_Region_Name == 'South Asia'] <- 'South Asia'
full_TreatSeek_n$Time_Factor <- as.factor(full_TreatSeek_n$Time_Factor)
unique(full_TreatSeek_n$Time_Factor)
full_TreatSeek_n$Time_Factor <- relevel(full_TreatSeek_n$Time_Factor, ref = 'Other')

#  Add Time_Factor to clean datasets:

clean_TreatSeek_Any$Time_Factor <- 'Other'
clean_TreatSeek_Any$Time_Factor[clean_TreatSeek_Any$IHME_Region_Name %in% c('Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa')] <- 'East, West Africa'
# clean_TreatSeek_Any$Time_Factor[clean_TreatSeek_Any$IHME_Region_Name == 'South Asia'] <- 'South Asia'
clean_TreatSeek_Any$Time_Factor <- as.factor(clean_TreatSeek_Any$Time_Factor)
unique(clean_TreatSeek_Any$Time_Factor)
clean_TreatSeek_Any$Time_Factor <- relevel(clean_TreatSeek_Any$Time_Factor, ref = 'Other')

clean_TreatSeek_HMISfrac$Time_Factor <- 'Other'
clean_TreatSeek_HMISfrac$Time_Factor[clean_TreatSeek_HMISfrac$IHME_Region_Name %in% c('Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa')] <- 'East, West Africa'
# clean_TreatSeek_HMISfrac$Time_Factor[clean_TreatSeek_HMISfrac$IHME_Region_Name == 'South Asia'] <- 'South Asia'
clean_TreatSeek_HMISfrac$Time_Factor <- as.factor(clean_TreatSeek_HMISfrac$Time_Factor)
unique(clean_TreatSeek_HMISfrac$Time_Factor)
clean_TreatSeek_HMISfrac$Time_Factor <- relevel(clean_TreatSeek_HMISfrac$Time_Factor, ref = 'Other')

clean_TreatSeek_Any$SMT_Factor <- relevel(clean_TreatSeek_Any$SMT_Factor, ref = 'Non_SMT')
clean_TreatSeek_Any$IHME_Region_Name <- relevel(clean_TreatSeek_Any$IHME_Region_Name, ref = 'Eastern Sub-Saharan Africa')


# Needs updating if chosen models have changed:
formula_any_1_LM <-  logit_Any ~ -1 + IHME_Region_Name + SMT_Factor + Year + ANC4_coverage_prop + DMSP_nighttime

# formula_hmis_1_LM <-  logit_HMIS ~ -1 + IHME_Region_Name + SMT_Factor + s(Year, by = Time_Factor) + s(frac_oop_hexp, k = 3) + s(education_all_ages_and_sexes_pc, k = 3)+ s(accessibility, k = 3) +  + s(IFD_coverage_prop , k = 3) + s(VIIRS_nighttime, k = 3) + s(measles_vacc_cov_prop, k = 3)

Any_model_LM <-  gam(formula_any_1_LM, data = clean_TreatSeek_Any)
# HMIS_model <- gam(formula_hmis_1, data = clean_TreatSeek_HMISfrac)


summary(Any_model_LM)
# summary(HMIS_model)
