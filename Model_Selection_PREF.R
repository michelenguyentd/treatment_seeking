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
years <- 1980:2019

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

model_no <- "PRF_" # 1. RF (random effect for region and units)

# For PTTIR onwards (regional temporal trends):

train_data_Any$Time_Factor <- as.character(train_data_Any$IHME_Region_Name)
train_data_Any$Time_Factor[!(train_data_Any$IHME_Region_Name %in% c("Central Asia", "North Africa and Middle East", 'South Asia', 'Southeast Asia', 'Western Sub-Saharan Africa'))] <- 'Other'
train_data_Any$Time_Factor[!(train_data_Any$ISO3 %in% c('ETH', 'MYT', 'COM', 'MDG')) & train_data_Any$IHME_Region_Name == 'Eastern Sub-Saharan Africa'] <- 'Western Sub-Saharan Africa'
train_data_Any$Time_Factor <- as.factor(train_data_Any$Time_Factor)
unique(train_data_Any$Time_Factor)
train_data_Any$Time_Factor <- relevel(train_data_Any$Time_Factor, ref = 'Other')

test_data_Any$Time_Factor <- as.character(test_data_Any$IHME_Region_Name)
test_data_Any$Time_Factor[!(test_data_Any$IHME_Region_Name %in% c("Central Asia", "North Africa and Middle East", 'South Asia', 'Southeast Asia', 'Western Sub-Saharan Africa'))] <- 'Other'
test_data_Any$Time_Factor[!(test_data_Any$ISO3 %in% c('ETH', 'MYT', 'COM', 'MDG')) & test_data_Any$IHME_Region_Name == 'Eastern Sub-Saharan Africa'] <- 'Western Sub-Saharan Africa'
test_data_Any$Time_Factor <- as.factor(test_data_Any$Time_Factor)
unique(test_data_Any$Time_Factor)
test_data_Any$Time_Factor <- relevel(test_data_Any$Time_Factor, ref = 'Other')

train_data_HMISfrac$Time_Factor_2 <- as.character(train_data_HMISfrac$IHME_Region_Name)
train_data_HMISfrac$Time_Factor_2[!(train_data_HMISfrac$IHME_Region_Name %in% c("Central Latin America", 'Southeast Asia', "Western Sub-Saharan Africa",  "Eastern Sub-Saharan Africa"))] <- 'Other'
train_data_HMISfrac$Time_Factor_2 <- as.factor(train_data_HMISfrac$Time_Factor_2)
unique(train_data_HMISfrac$Time_Factor_2)
train_data_HMISfrac$Time_Factor_2 <- relevel(train_data_HMISfrac$Time_Factor_2, ref = 'Other')

test_data_HMISfrac$Time_Factor_2 <- as.character(test_data_HMISfrac$IHME_Region_Name)
test_data_HMISfrac$Time_Factor_2[!(test_data_HMISfrac$IHME_Region_Name %in% c("Central Latin America", 'Southeast Asia', "Western Sub-Saharan Africa",  "Eastern Sub-Saharan Africa"))] <- 'Other'
test_data_HMISfrac$Time_Factor_2 <- as.factor(test_data_HMISfrac$Time_Factor_2)
unique(test_data_HMISfrac$Time_Factor_2)
test_data_HMISfrac$Time_Factor_2 <- relevel(test_data_HMISfrac$Time_Factor_2, ref = 'Other')


# For PTTFR onwards (coarser regional factors):

train_data_Any$Reg_Factor <- as.character(train_data_Any$IHME_Region_Name)
train_data_Any$Reg_Factor[!(train_data_Any$IHME_Region_Name %in% c('Southeast Asia', 'South Asia', "Central Asia", 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa'))] <- 'Other'
train_data_Any$Reg_Factor <- as.factor(train_data_Any$Reg_Factor)
unique(train_data_Any$Reg_Factor)
train_data_Any$Reg_Factor <- relevel(train_data_Any$Reg_Factor, ref = 'Other')

test_data_Any$Reg_Factor <- as.character(test_data_Any$IHME_Region_Name)
test_data_Any$Reg_Factor[!(test_data_Any$IHME_Region_Name %in% c('Southeast Asia','South Asia', "Central Asia", 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa'))] <- 'Other'
test_data_Any$Reg_Factor <- as.factor(test_data_Any$Reg_Factor)
unique(test_data_Any$Reg_Factor)
test_data_Any$Reg_Factor <- relevel(test_data_Any$Reg_Factor, ref = 'Other')

train_data_HMISfrac$Reg_Factor_2 <- as.character(train_data_HMISfrac$IHME_Region_Name)
train_data_HMISfrac$Reg_Factor_2[!(train_data_HMISfrac$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Southeast Asia"))] <- 'Other'
train_data_HMISfrac$Reg_Factor_2 <- as.factor(train_data_HMISfrac$Reg_Factor_2)
unique(train_data_HMISfrac$Reg_Factor_2)
train_data_HMISfrac$Reg_Factor_2 <- relevel(train_data_HMISfrac$Reg_Factor_2, ref = 'Other')

test_data_HMISfrac$Reg_Factor_2 <- as.character(test_data_HMISfrac$IHME_Region_Name)
test_data_HMISfrac$Reg_Factor_2[!(test_data_HMISfrac$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Southeast Asia"))] <- 'Other'
test_data_HMISfrac$Reg_Factor_2 <- as.factor(test_data_HMISfrac$Reg_Factor_2)
unique(test_data_HMISfrac$Reg_Factor_2)
test_data_HMISfrac$Reg_Factor_2 <- relevel(test_data_HMISfrac$Reg_Factor_2, ref = 'Other')

# Use location id as factor instead of admin unit name so as to account for 2 Punjabs and 2 Mexicos:
train_data_Any$IHME_location_id <- as.factor(train_data_Any$IHME_location_id)
train_data_HMISfrac$IHME_location_id <- as.factor(train_data_HMISfrac$IHME_location_id)


########## ---------------------- 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

cov_col <- clean_TreatSeek_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                        'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop",
                                        "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "measles_vacc_cov_prop_2", "ind_health", 
                                        "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:42) # check covariate names.
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TreatSeek_HMISfrac)[cov_ind]

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TreatSeek_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  # if (!(i %in% c(9, 12))){
  var_values_Any <- train_data_Any[, cov_ind[i]]
  gam_Any <- gamm(logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor) + s(X), data = data.frame("logit_Any" = train_data_Any$logit_Any, "X" = var_values_Any, 'Year' = train_data_Any$Year, 'Time_Factor' = train_data_Any$Time_Factor, 'Reg_Factor' = train_data_Any$Reg_Factor, 'IHME_location_id' = train_data_Any$IHME_location_id), random = list(IHME_location_id = ~ 1))
  AIC_Any[i] <- AIC(gam_Any$lme)
  # }
  # if (!(i %in% c(7))){
    var_values_HMIS <- train_data_HMISfrac[, cov_ind[i]]
    gam_HMIS <- gamm(logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2) + s(X), data =  data.frame("logit_HMIS" = train_data_HMISfrac$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_data_HMISfrac$Year, 'Time_Factor_2' = train_data_HMISfrac$Time_Factor_2, 'Reg_Factor_2' = train_data_HMISfrac$Reg_Factor_2, 'IHME_location_id' = train_data_HMISfrac$IHME_location_id), random = list(IHME_location_id = ~ 1))
    AIC_HMIS[i] <- AIC(gam_HMIS$lme)
  # }
}

AIC_Any_order <- names(sort(AIC_Any[!(names(AIC_Any) %in% c("frac_oop_hexp",  "measles_vacc_cov_prop_2", "oop_hexp_cap", "prop_urban", "measles_vacc_cov_prop", "ind_health", "accessibility"))]))# Don't use frac_oop_hexp, oop_hexp_cap for any TS. measles_vacc_cov_prop_2 has extreme outliers. accessibility (travel time) etc. has strange relations.

test_cov_mat <- cor(cov_col[, AIC_Any_order])
abs_mat <- abs(test_cov_mat) >0.6 # For checking which covariates are strongly correlated to those of increasing AIC.

test_cov_Any <- AIC_Any_order[1]

for (i in AIC_Any_order[-1]){
  # If candidate covariate has < 0.6 correlation with existing covariates.
  if (sum(abs_mat[i, test_cov_Any])< 1){
    test_cov_Any <- c(test_cov_Any, i)
  }
}

AIC_HMIS_order <- names(sort(AIC_HMIS[!(names(AIC_HMIS) %in% c("measles_vacc_cov_prop_2", "education_all_ages_and_sexes_pc", "accessibility"))])) # measles_vacc_cov_prop_2 has extreme outliers while rest has strange relations.

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

formula_any <-  logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(GDPpc_id_b2010, k = 3) + s(hospital_beds_per1000, k = 3)
test.model <- gamm(formula_any, data = train_data_Any, random = list(IHME_location_id = ~ 1))

summary(test.model$gam)

model_any <- uGamm(logit_Any ~ s(Year), data = train_data_Any, random = list(IHME_location_id = ~ 1))

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed=c('Reg_Factor', "s(Year, by = Time_Factor, k = 5)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 81.92 s.
model.select.any[1:10]

formula_HMIS <-  logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2, k = 5)  + s(ANC1_coverage_prop, k = 3) + s(VIIRS_nighttime, k = 3) + s(frac_oop_hexp, k = 3) + s(prop_urban, k = 3) + s(log_the_pc, k = 3) + s(hospital_beds_per1000, k = 3)  + s(measles_vacc_cov_prop, k = 3)
test.model <- gamm(formula_HMIS, data = train_data_HMISfrac, random = list(IHME_location_id = ~ 1))

summary(test.model$gam)

model_HMIS <- uGamm(logit_HMIS ~ s(Year), data = train_data_HMISfrac, random = list(IHME_location_id = ~ 1))

model_HMIS$gam$formula <- formula_HMIS

temptime2 <- proc.time()[3]
model.select.HMIS <- dredge(model_HMIS, fixed=c('Reg_Factor_2', 's(Year, by = Time_Factor_2, k = 5)'), m.min=4)
timetaken2 <- proc.time()[3] - temptime2
model.select.HMIS[1:10]
# About 8 minutes.

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 
best.HMIS<-subset(model.select.HMIS, delta < 2) #takes the best model with delta AICc less than 2 - only 1. 

print(xtable(best.any, type = "latex"), file = paste(table.path, model_no, "best.any.tex", sep = ''))
print(xtable(best.HMIS, type = "latex"), file = paste(table.path, model_no, "best.HMIS.tex", sep = ''))

########## ---------------------- 3. EXAMINE THE FINAL MODELS -------------------- ############

formula_any_1 <-  logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(GDPpc_id_b2010, k = 3) 

model_any_1 <- gamm(formula_any_1, data = train_data_Any, random = list(IHME_location_id = ~ 1))
refRegion <- ranef(model_any_1$lme)

# To be changed for different models:

gamtabs(model_any_1$gam, caption = "Summary of best any TS model with random effects for the units.")

pdf(paste(graphics.path, model_no, "Any_model_smooths_1.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_any_1$gam, select = 1, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1$gam, select = 2, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1$gam, select = 3, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1$gam, select = 4, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1$gam, select = 5, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1$gam, select = 6, ylim = c(-5, 5), xlim = c(1990, 2019))
plot(model_any_1$gam, select = 7, ylim = c(-5, 5))
plot(model_any_1$gam, select = 8, ylim = c(-5, 5))
plot(model_any_1$gam, select = 9, ylim = c(-5, 5))
plot(model_any_1$gam, select = 10, ylim = c(-5, 5))
dev.off()

pdf(paste(graphics.path, model_no, "Any_model_smooths_from2000.pdf", sep = ""), width = 9, height = 9)
par(mfrow = c(3, 3))
plot(model_any_1$gam, select = 1, ylim = c(-2, 2), xlim = c(2000, 2019)) 
plot(model_any_1$gam, select = 2, ylim = c(-2, 2), xlim = c(2000, 2019))  
plot(model_any_1$gam, select = 3, ylim = c(-2, 2), xlim = c(2000, 2019))  
plot(model_any_1$gam, select = 4, ylim = c(-2, 2), xlim = c(2000, 2019)) 
plot(model_any_1$gam, select = 5, ylim = c(-2, 2), xlim = c(2000, 2019)) 
plot(model_any_1$gam, select = 6, ylim = c(-2, 2), xlim = c(2000, 2019))
plot(model_any_1$gam, select = 7, ylim = c(-2, 2))
plot(model_any_1$gam, select = 8, ylim = c(-2, 2))
plot(model_any_1$gam, select = 9, ylim = c(-2, 2))
plot(model_any_1$gam, select = 10, ylim = c(-2, 2))
dev.off()

formula_hmis_1 <-  logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2, k = 5) + s(ANC1_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(frac_oop_hexp, k = 3) + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(prop_urban, k = 3) + s(VIIRS_nighttime, k = 3)

model_hmis_1 <- gamm(formula_hmis_1, data = train_data_HMISfrac, random = list(IHME_location_id = ~ 1))
refRegion_HMIS <- ranef(model_hmis_1$lme)

pdf(paste(graphics.path, model_no, "Unit_QQ.pdf", sep = ""), width = 9, height = 4.5)
par(mfrow = c(1, 2))
qqnorm(refRegion$IHME_location_id[, ], main = 'Normal Q-Q plot (Any)')
qqline(refRegion$IHME_location_id[, ])
qqnorm(refRegion_HMIS$IHME_location_id[, ], main = 'Normal Q-Q plot (Pub. fraction)')
qqline(refRegion_HMIS$IHME_location_id[, ])
dev.off()

pdf(paste(graphics.path, model_no, "Any_model_REF_QQ.pdf", sep = ""), width = 4, height = 4)
qqnorm(refRegion$IHME_location_id[, ], main = 'Normal Q-Q plot (Any)')
qqline(refRegion$IHME_location_id[, ])
dev.off()

gamtabs(model_hmis_1$gam, caption = "Summary of best public fraction model with random effects for the units.")

pdf(paste(graphics.path, model_no, "HMIS_model1_smooths.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_hmis_1$gam, select = 1, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1$gam, select = 2, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_hmis_1$gam, select = 3, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_hmis_1$gam, select = 4, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1$gam, select = 5, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1$gam, select = 6, ylim = c(-5, 5))
plot(model_hmis_1$gam, select = 7, ylim = c(-5, 5)) 
plot(model_hmis_1$gam, select = 8, ylim = c(-5, 5)) 
plot(model_hmis_1$gam, select = 9, ylim = c(-5, 5))
plot(model_hmis_1$gam, select = 10, ylim = c(-5, 5))
plot(model_hmis_1$gam, select = 11, ylim = c(-5, 5)) 
plot(model_hmis_1$gam, select = 12, ylim = c(-5, 5)) 
dev.off()

pdf(paste(graphics.path, model_no, "HMIS_model_smooths_from2000.pdf", sep = ""), width = 9, height = 12)
par(mfrow = c(4, 3))
plot(model_hmis_1$gam, select = 1, ylim = c(-2, 2), xlim = c(2000, 2019))  
plot(model_hmis_1$gam, select = 2, ylim = c(-2, 2), xlim = c(2000, 2019)) 
plot(model_hmis_1$gam, select = 3, ylim = c(-2, 2), xlim = c(2000, 2019)) 
plot(model_hmis_1$gam, select = 4, ylim = c(-2, 2), xlim = c(2000, 2019))  
plot(model_hmis_1$gam, select = 5, ylim = c(-2, 2), xlim = c(2000, 2019))  
plot(model_hmis_1$gam, select = 6, ylim = c(-2, 2))
plot(model_hmis_1$gam, select = 7, ylim = c(-2, 2)) 
plot(model_hmis_1$gam, select = 8, ylim = c(-2, 2)) 
plot(model_hmis_1$gam, select = 9, ylim = c(-2, 2))
plot(model_hmis_1$gam, select = 10, ylim = c(-2, 2))
plot(model_hmis_1$gam, select = 11, ylim = c(-2, 2)) 
plot(model_hmis_1$gam, select = 12, ylim = c(-2, 2)) 
dev.off()

# Check results...