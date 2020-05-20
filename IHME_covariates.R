library(mgcv) # For GAM.
library(splitstackshape) # For stratified sampling on IHME regions.
library(MuMIn) # for AIC (model selection)
library(gtools) # for logit transform
library(VGAM) # for probit transform
library(survey) # for taking into account sampling weights
library(nortest) # Normality tests for residuals
library(xtable) # For exporting latex tables.
library(plotrix) # For plotCI.
library(itsadug) # For latex table of gam model summary.

rm(list = ls())
setwd('J:/Treatment_Seeking/')

graphics.path <- 'J:/Treatment_Seeking/graphics/'
data.path <- 'J:/Treatment_Seeking/Data/'
table.path <- 'J:/Treatment_Seeking/ModelTables/'

# Years to model:
years <- 1980:2017

# Lists of units:
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


# Read in full datasets, training and test sets:

full_TreatSeek <- read.csv(paste(data.path, "full_TreatSeek.csv", sep = ""))
full_TreatSeek_n <- read.csv(paste(data.path, "full_TreatSeek_n.csv", sep = ""))

clean_TreatSeek_Any <- read.csv(paste(data.path, "clean_TreatSeek_Any.csv", sep = ""))
clean_TreatSeek_HMISfrac <- read.csv(paste(data.path, "clean_TreatSeek_HMISfrac.csv", sep = ""))

train_data_Any <- read.csv(paste(data.path, 'train_data_Any.csv', sep = ''))
train_data_HMISfrac <- read.csv(paste(data.path, 'train_data_HMISfrac.csv', sep = ''))
test_data_Any <- read.csv(paste(data.path, 'test_data_Any.csv', sep = ''))
test_data_HMISfrac <- read.csv(paste(data.path, 'test_data_HMISfrac.csv', sep = ''))

summary(train_data_Any$IHME_Region_Name) + summary(test_data_Any$IHME_Region_Name)

# Models to be tested:

model_no <- "Cov_" # 1. RTTIR: IHME region temporal trend and factors; 2. PTTIR: Pruned regions temporal trend and IHME regional factors; 3. PTTPF: Pruned regions temporal trend and factors; 4. OC: Other considerations; 5. Covariates only; no time trend.

########## ---------------------- 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

## Correlation between covariates:

# Get columns with covariates. 
# Don't consider hospital_beds_per1000 because there is only 2 data points in the 0.5-2.5 range.
cov_col <- clean_TreatSeek_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop", "educ_yrs_age_std_pc_1", "educ_yrs_age_std_pc_2",
                                "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "universal_health_coverage", "haqi", "measles_vacc_cov_prop_2", "ind_health", 
                                "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:30, 34:37, 40:46)
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TreatSeek_HMISfrac)[cov_ind]

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TreatSeek_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  var_values_Any <- train_data_Any[, cov_ind[i]] 
  gam_Any <- gam(logit_Any ~ -1 + IHME_Region_Name + s(X), data = data.frame("logit_Any" = train_data_Any$logit_Any, "X" = var_values_Any, 'Year' = train_data_Any$Year, 'IHME_Region_Name' = train_data_Any$IHME_Region_Name))
  var_values_HMIS <- train_data_HMISfrac[, cov_ind[i]]
  gam_HMIS <- gam(logit_HMIS ~ -1 + IHME_Region_Name + s(X), data =  data.frame("logit_HMIS" = train_data_HMISfrac$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_data_HMISfrac$Year, 'IHME_Region_Name' = train_data_HMISfrac$IHME_Region_Name))
  var_values_HMIS <- train_data_HMISfrac[, cov_ind[i]]
  AIC_Any[i] <- gam_Any$aic
  AIC_HMIS[i] <- gam_HMIS$aic
}

AIC_Any_order <- names(sort(AIC_Any[!(names(AIC_Any) %in% c("frac_oop_hexp", "ind_health", 'VIIRS_nighttime'))])) # Don't use frac_oop_hexp for any TS. Exclude ind_health - strange relation with public fraction.

test_cov_mat <- cor(cov_col[, AIC_Any_order])
abs_mat <- abs(test_cov_mat) >0.6 # For checking which covariates are strongly correlated to those of increasing AIC.

test_cov_Any <- AIC_Any_order[1]

for (i in AIC_Any_order[-1]){
  # If candidate covariate has < 0.6 correlation with existing covariates.
  if (sum(abs_mat[i, test_cov_Any])< 1){
    test_cov_Any <- c(test_cov_Any, i)
  }
}

AIC_HMIS_order <- names(sort(AIC_HMIS[!names(AIC_HMIS) %in% c("ind_health", 'VIIRS_nighttime')])) 

test_cov_mat <- cor(cov_col[, AIC_HMIS_order])
abs_mat <- abs(test_cov_mat) >0.6 # For checking which covariates are strongly correlated to those of increasing AIC.

test_cov_HMIS <- AIC_HMIS_order[1]

for (i in AIC_HMIS_order[-1]){
  # If candidate covariate has < 0.6 correlation with existing covariates.
  if (sum(abs_mat[i, test_cov_HMIS])< 1){
    test_cov_HMIS <- c(test_cov_HMIS, i)
  }
}

test_cov_Any
test_cov_HMIS

########## ---------------------- 2. FIT THE PRELIMINARY MODELS ON TS MEANS OF TRAINING DATA -------------------- ############

#  Use covariates identified in test_cov_Any:

formula_any <-  logit_Any ~ -1 + IHME_Region_Name + s(ANC4_coverage_prop, k = 3) + s(IFD_coverage_prop, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(oop_hexp_cap, k = 3) + s(hospital_beds_per1000, k = 3) + s(measles_vacc_cov_prop_2, k = 3)

summary(test.model)

model_any <- uGamm(logit_Any ~ -1 + IHME_Region_Name + s(ANC4_coverage_prop, k = 3), data = train_data_Any)

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed = "IHME_Region_Name", m.min=3)
timetaken <- proc.time()[3] - temptime  # 81.92 s.
model.select.any[1:10]

formula_HMIS <-  logit_HMIS ~ -1 + IHME_Region_Name + s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(frac_oop_hexp, k = 3) + s(prop_urban, k = 3) + s(IFD_coverage_prop, k = 3) + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(measles_vacc_cov_prop_2, k =3 ) 

test.model <- gam(formula_HMIS, data = train_data_HMISfrac)

summary(test.model)

model_HMIS <- uGamm(formula_HMIS, data = train_data_HMISfrac)

model_HMIS$gam$formula <- formula_HMIS

temptime2 <- proc.time()[3]
model.select.HMIS <- dredge(model_HMIS, fixed = 'IHME_Region_Name', m.min=3)
timetaken2 <- proc.time()[3] - temptime2
model.select.HMIS[1:10]
# About 8 minutes.

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 
best.HMIS<-subset(model.select.HMIS, delta < 2) #takes the best model with delta AICc less than 2 - only 1. 

# print(xtable(best.any, type = "latex"), file = paste(table.path, model_no, "best.any.tex", sep = ''))
# print(xtable(best.HMIS, type = "latex"), file = paste(table.path, model_no, "best.HMIS.tex", sep = ''))

########## ---------------------- 3. EXAMINE THE FINAL MODELS -------------------- ############

# Examine components of chosen models:

formula_any_1 <-  logit_Any ~ -1 + IHME_Region_Name + s(ANC4_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3)

model_any_1 <- gam(formula_any_1, data = train_data_Any)
summary(model_any_1) # 71%. - Identified the same covariates as with temporal trend.

# To be changed for different models:

# gamtabs(model_any_1, caption = "Summary of best any TS model with pruned region temporal trends, country and region factors")

pdf(paste(graphics.path, model_no, "Any_model_smooths_1.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_any_1, select = 1, ylim = c(-5, 5)) 
plot(model_any_1, select = 2, ylim = c(-5, 5))  
plot(model_any_1, select = 3, ylim = c(-5, 5)) 
plot(model_any_1, select = 4, ylim = c(-5, 5)) 
plot(model_any_1, select = 5, ylim = c(-5, 5)) 
# plot(model_any_1, select = 6, ylim = c(-5, 5)) 
# plot(model_any_1, select = 7, ylim = c(-5, 5)) 
# plot(model_any_1, select = 8, ylim = c(-5, 5)) 
# plot(model_any_1, select = 9, ylim = c(-5, 5)) 
# plot(model_any_1, select = 10, ylim = c(-5, 5))  
# plot(model_any_1, select = 12, ylim = c(-5, 5)) 
# plot(model_any_1, select = 13, ylim = c(-5, 5))  
# plot(model_any_1, select = 14, ylim = c(-5, 5)) 
dev.off()

# Identified larger subset of covariates than with temporal trend; prop_urban, IFD_coverage_prop, accessibility, hospital_beds_per1000 remains useful.
formula_hmis_1 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(prop_urban, k = 3) + s(IFD_coverage_prop, k = 3) + s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(frac_oop_hexp, k = 3)

model_hmis_1 <- gam(formula_hmis_1, data = train_data_HMISfrac)
summary(model_hmis_1) # 60.2%

gamtabs(model_hmis_1, caption = "Summary of best public fraction model with pruned region temporal trends, country and region factors")


pdf(paste(graphics.path, model_no, "HMIS_model1_smooths.pdf", sep = ""), width = 12, height = 12)
par(mfrow = c(3, 3))
plot(model_hmis_1, select = 1) 
plot(model_hmis_1, select = 2)
plot(model_hmis_1, select = 3)
plot(model_hmis_1, select = 4) 
plot(model_hmis_1, select = 5) 
plot(model_hmis_1, select = 6) 
plot(model_hmis_1, select = 7) 
plot(model_hmis_1, select = 8) 
plot(model_hmis_1, select = 9) 
dev.off()

########## ---------------------- 4. Covariate time series -------------------- ############

master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name) # This has grouped Oceania with SE Asia etc. Use full_TreatSeek if want true regions.
master.unit.list <- unique(full_TreatSeek_n$Admin_Unit_Name)


for (l in 1:length(cov_ind)){
  
  curr_cov <- names(full_TreatSeek_n)[cov_ind[l]] 
  
  pdf(paste(graphics.path, curr_cov, '.pdf', sep = ''),width=8.7,height = 11.2)
  
  par(mfrow=c(3,2))
  dataset <- TS_datasets[[1]]
  for (j in 1:length(master.region.list)){
    region.data <- dataset[dataset$IHME_Region_Name == master.region.list[j], ]
    countries <- unique(region.data$ISO3)
    for (i in 1:length(countries)){
      unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
      for (k in 1:length(unit.list)){
        unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
        plot(0,0,type='n',ylim=c(-4,4),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab= curr_cov,xlab='Year')
        lines(years,unit.row[, cov_ind[l]-12], col = rgb(0, 0, 1, 0.2))
        for (m in 2:100){
          dataset_m <- TS_datasets[[m]]
          lines(years, dataset_m[dataset_m$ISO3 == countries[i] & dataset_m$Admin_Unit_Name == unit.list[k], cov_ind[l]-12], col = rgb(0, 0, 1, 0.2))
        }
      }
    }
  }
  
  dev.off()
}