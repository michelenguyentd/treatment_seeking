library(mgcv) # For GAM.
library(splitstackshape) # For stratified sampling on IHME regions.
library(MuMIn) # for AIC (model selection)
library(car) # for logit transform
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

clean_TreatSeek_Any <- read.csv(paste(data.path, "clean_TreatSeek_Any.csv", sep = ""))
clean_TreatSeek_HMISfrac <- read.csv(paste(data.path, "clean_TreatSeek_HMISfrac.csv", sep = ""))

clean_TreatSeek_Any$Admin_Unit_Name <- as.character(clean_TreatSeek_Any$Admin_Unit_Name)
clean_TreatSeek_HMISfrac$Admin_Unit_Name <- as.character(clean_TreatSeek_HMISfrac$Admin_Unit_Name)

#  Add Time_Factor to clean datasets:

clean_TreatSeek_Any$Time_Factor <- 'Other'
clean_TreatSeek_Any$Time_Factor[clean_TreatSeek_Any$IHME_Region_Name %in% c('Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa')] <- 'East, West Africa'
clean_TreatSeek_Any$Time_Factor[clean_TreatSeek_Any$IHME_Region_Name == 'South Asia'] <- 'South Asia'
clean_TreatSeek_Any$Time_Factor <- as.factor(clean_TreatSeek_Any$Time_Factor)
unique(clean_TreatSeek_Any$Time_Factor)
clean_TreatSeek_Any$Time_Factor <- relevel(clean_TreatSeek_Any$Time_Factor, ref = 'Other')

clean_TreatSeek_HMISfrac$Time_Factor <- 'Other'
clean_TreatSeek_HMISfrac$Time_Factor[clean_TreatSeek_HMISfrac$IHME_Region_Name %in% c('Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa')] <- 'East, West Africa'
clean_TreatSeek_HMISfrac$Time_Factor[clean_TreatSeek_HMISfrac$IHME_Region_Name == 'South Asia'] <- 'South Asia'
clean_TreatSeek_HMISfrac$Time_Factor <- as.factor(clean_TreatSeek_HMISfrac$Time_Factor)
unique(clean_TreatSeek_HMISfrac$Time_Factor)
clean_TreatSeek_HMISfrac$Time_Factor <- relevel(clean_TreatSeek_HMISfrac$Time_Factor, ref = 'Other')

six_minor_terr <- c("Andaman & Nicobar Islands", "Lakshadweep", "Chandigarh", "Dadra & Nagar Haveli", "Daman & Diu", "Puducherry")


# ------------ 1. Separate data into regions - Other, EW Africa, South Asia ----------- 

# Split from clean TS data -> region specific training and test data. 

clean_TS_EWAfrica_Any <- clean_TreatSeek_Any[clean_TreatSeek_Any$Time_Factor == 'East, West Africa', ]
nrow(clean_TS_EWAfrica_Any) # 116

set.seed(1)
train_Any_EWAfrica <- stratified(indt = clean_TS_EWAfrica_Any, group = "IHME_Region_Name", size = 0.7)
summary(train_Any_EWAfrica$IHME_Region_Name)
summary(train_Any_EWAfrica$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_Any_EWAfrica$Admin_Unit_Name)){
    train_Any_EWAfrica <- rbind(train_Any_EWAfrica, clean_TS_EWAfrica_Any[(clean_TS_EWAfrica_Any$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_Any point per SMT at the moment.
  }
}

summary(train_Any_EWAfrica$SMT_Factor)
nrow(train_Any_EWAfrica) # 81
train_Any_EWAfrica <- as.data.frame(train_Any_EWAfrica)

test_Any_EWAfrica <- clean_TS_EWAfrica_Any[!(clean_TS_EWAfrica_Any$X %in% train_Any_EWAfrica$X), ]
nrow(test_Any_EWAfrica) # 35

clean_TS_EWAfrica_HMISfrac <- clean_TreatSeek_HMISfrac[clean_TreatSeek_HMISfrac$Time_Factor == 'East, West Africa', ]
nrow(clean_TS_EWAfrica_HMISfrac) # 112

set.seed(1)
train_HMISfrac_EWAfrica <- stratified(indt = clean_TS_EWAfrica_HMISfrac, group = "IHME_Region_Name", size = 0.7)
summary(train_HMISfrac_EWAfrica$IHME_Region_Name)
summary(train_HMISfrac_EWAfrica$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_HMISfrac_EWAfrica$Admin_Unit_Name)){
    train_HMISfrac_EWAfrica <- rbind(train_HMISfrac_EWAfrica, clean_TS_EWAfrica_HMISfrac[(clean_TS_EWAfrica_HMISfrac$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_HMISfrac point per SMT at the moment.
  }
}

summary(train_HMISfrac_EWAfrica$SMT_Factor)
nrow(train_HMISfrac_EWAfrica) # 79
train_HMISfrac_EWAfrica <- as.data.frame(train_HMISfrac_EWAfrica)

test_HMISfrac_EWAfrica <- clean_TS_EWAfrica_HMISfrac[!(clean_TS_EWAfrica_HMISfrac$X %in% train_HMISfrac_EWAfrica$X), ]
nrow(test_HMISfrac_EWAfrica) # 33

clean_TS_SAsia_Any <- clean_TreatSeek_Any[clean_TreatSeek_Any$Time_Factor == 'South Asia', ]
nrow(clean_TS_SAsia_Any) # 106

set.seed(1)
train_Any_SAsia <- stratified(indt = clean_TS_SAsia_Any, group = "IHME_Region_Name", size = 0.7)
summary(train_Any_SAsia$IHME_Region_Name)
summary(train_Any_SAsia$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_Any_SAsia$Admin_Unit_Name)){
    train_Any_SAsia <- rbind(train_Any_SAsia, clean_TS_SAsia_Any[(clean_TS_SAsia_Any$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_Any point per SMT at the moment.
  }
}

summary(train_Any_SAsia$SMT_Factor)
nrow(train_Any_SAsia) # 76
train_Any_SAsia <- as.data.frame(train_Any_SAsia)

test_Any_SAsia <- clean_TS_SAsia_Any[!(clean_TS_SAsia_Any$X %in% train_Any_SAsia$X), ]
nrow(test_Any_SAsia) # 30

clean_TS_SAsia_HMISfrac <- clean_TreatSeek_HMISfrac[clean_TreatSeek_HMISfrac$Time_Factor == 'South Asia', ]
nrow(clean_TS_SAsia_HMISfrac) # 106

set.seed(1)
train_HMISfrac_SAsia <- stratified(indt = clean_TS_SAsia_HMISfrac, group = "IHME_Region_Name", size = 0.7)
summary(train_HMISfrac_SAsia$IHME_Region_Name)
summary(train_HMISfrac_SAsia$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_HMISfrac_SAsia$Admin_Unit_Name)){
    train_HMISfrac_SAsia <- rbind(train_HMISfrac_SAsia, clean_TS_SAsia_HMISfrac[(clean_TS_SAsia_HMISfrac$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_HMISfrac point per SMT at the moment.
  }
}

summary(train_HMISfrac_SAsia$SMT_Factor)
nrow(train_HMISfrac_SAsia) # 76
train_HMISfrac_SAsia <- as.data.frame(train_HMISfrac_SAsia)

test_HMISfrac_SAsia <- clean_TS_SAsia_HMISfrac[!(clean_TS_SAsia_HMISfrac$X %in% train_HMISfrac_SAsia$X), ]
nrow(test_HMISfrac_SAsia) # 30


# Split from clean TS data -> region specific training and test data. 

clean_TS_Other_Any <- clean_TreatSeek_Any[clean_TreatSeek_Any$Time_Factor == 'Other', ]
nrow(clean_TS_Other_Any) # 83

set.seed(1)
train_Any_Other <- stratified(indt = clean_TS_Other_Any, group = "IHME_Region_Name", size = 0.7)
summary(train_Any_Other$IHME_Region_Name)
summary(train_Any_Other$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_Any_Other$Admin_Unit_Name)){
    train_Any_Other <- rbind(train_Any_Other, clean_TS_Other_Any[(clean_TS_Other_Any$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_Any point per SMT at the moment.
  }
}

summary(train_Any_Other$SMT_Factor)
nrow(train_Any_Other) # 58
train_Any_Other <- as.data.frame(train_Any_Other)

test_Any_Other <- clean_TS_Other_Any[!(clean_TS_Other_Any$X %in% train_Any_Other$X), ]
nrow(test_Any_Other) # 25

clean_TS_Other_HMISfrac <- clean_TreatSeek_HMISfrac[clean_TreatSeek_HMISfrac$Time_Factor == 'Other', ]
nrow(clean_TS_Other_HMISfrac) # 82

set.seed(1)
train_HMISfrac_Other <- stratified(indt = clean_TS_Other_HMISfrac, group = "IHME_Region_Name", size = 0.7)
summary(train_HMISfrac_Other$IHME_Region_Name)
summary(train_HMISfrac_Other$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_HMISfrac_Other$Admin_Unit_Name)){
    train_HMISfrac_Other <- rbind(train_HMISfrac_Other, clean_TS_Other_HMISfrac[(clean_TS_Other_HMISfrac$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_HMISfrac point per SMT at the moment.
  }
}

summary(train_HMISfrac_Other$SMT_Factor)
nrow(train_HMISfrac_Other) # 57
train_HMISfrac_Other <- as.data.frame(train_HMISfrac_Other)

test_HMISfrac_Other <- clean_TS_Other_HMISfrac[!(clean_TS_Other_HMISfrac$X %in% train_HMISfrac_Other$X), ]
nrow(test_HMISfrac_Other) # 25

clean_TS_Other_Any <- clean_TreatSeek_Any[clean_TreatSeek_Any$Time_Factor == 'Other', ]
nrow(clean_TS_Other_Any) # 83

set.seed(1)
train_Any_Other <- stratified(indt = clean_TS_Other_Any, group = "IHME_Region_Name", size = 0.7)
summary(train_Any_Other$IHME_Region_Name)
summary(train_Any_Other$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_Any_Other$Admin_Unit_Name)){
    train_Any_Other <- rbind(train_Any_Other, clean_TS_Other_Any[(clean_TS_Other_Any$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_Any point per SMT at the moment.
  }
}

summary(train_Any_Other$SMT_Factor)
nrow(train_Any_Other) # 58
train_Any_Other <- as.data.frame(train_Any_Other)

test_Any_Other <- clean_TS_Other_Any[!(clean_TS_Other_Any$X %in% train_Any_Other$X), ]
nrow(test_Any_Other) # 25

clean_TS_Other_HMISfrac <- clean_TreatSeek_HMISfrac[clean_TreatSeek_HMISfrac$Time_Factor == 'Other', ]
nrow(clean_TS_Other_HMISfrac) # 82

set.seed(1)
train_HMISfrac_Other <- stratified(indt = clean_TS_Other_HMISfrac, group = "IHME_Region_Name", size = 0.7)
summary(train_HMISfrac_Other$IHME_Region_Name)
summary(train_HMISfrac_Other$SMT_Factor)

for (i in 1:length(six_minor_terr)){
  if (!(six_minor_terr[i] %in% train_HMISfrac_Other$Admin_Unit_Name)){
    train_HMISfrac_Other <- rbind(train_HMISfrac_Other, clean_TS_Other_HMISfrac[(clean_TS_Other_HMISfrac$Admin_Unit_Name == six_minor_terr[i]), ]) # Only have one data_HMISfrac point per SMT at the moment.
  }
}

summary(train_HMISfrac_Other$SMT_Factor)
nrow(train_HMISfrac_Other) # 57
train_HMISfrac_Other <- as.data.frame(train_HMISfrac_Other)

test_HMISfrac_Other <- clean_TS_Other_HMISfrac[!(clean_TS_Other_HMISfrac$X %in% train_HMISfrac_Other$X), ]
nrow(test_HMISfrac_Other) # 25


########## ---------------------- EW Africa 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

## Correlation between covariates:

# Get columns with covariates. 
# Don't consider hospital_beds_per1000 because there is only 2 data points in the 0.5-2.5 range.
cov_col <- clean_TS_EWAfrica_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                          'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop", "educ_yrs_age_std_pc_1", "educ_yrs_age_std_pc_2",
                                          "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "universal_health_coverage", "haqi", "measles_vacc_cov_prop_2", "ind_health", 
                                          "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:30, 34:37, 41:46) # Don't use measles_vacc_cov_prop_2 since many same values.
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TS_EWAfrica_HMISfrac)[cov_ind]

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TS_EWAfrica_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  var_values_Any <- train_Any_EWAfrica[, cov_ind[i]] 
  gam_Any <- gam(logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(X), data = data.frame("logit_Any" = train_Any_EWAfrica$logit_Any, "X" = var_values_Any, 'Year' = train_Any_EWAfrica$Year, 'IHME_Region_Name' = train_Any_EWAfrica$IHME_Region_Name, 'SMT_Factor' = train_Any_EWAfrica$SMT_Factor, 'Time_Factor' = train_Any_EWAfrica$Time_Factor))
  var_values_HMIS <- train_HMISfrac_EWAfrica[, cov_ind[i]]
  gam_HMIS <- gam(logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(X), data =  data.frame("logit_HMIS" = train_HMISfrac_EWAfrica$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_HMISfrac_EWAfrica$Year, 'IHME_Region_Name' = train_HMISfrac_EWAfrica$IHME_Region_Name, 'SMT_Factor' = train_HMISfrac_EWAfrica$SMT_Factor, 'Time_Factor' = train_HMISfrac_EWAfrica$Time_Factor))
  AIC_Any[i] <- gam_Any$aic
  AIC_HMIS[i] <- gam_HMIS$aic
}

AIC_Any_order <- names(sort(AIC_Any[names(AIC_Any) != "frac_oop_hexp"]))# Don't use frac_oop_hexp for any TS.

test_cov_mat <- cor(cov_col[, AIC_Any_order])
abs_mat <- abs(test_cov_mat) >0.6 # For checking which covariates are strongly correlated to those of increasing AIC.

test_cov_Any <- AIC_Any_order[1]

for (i in AIC_Any_order[-1]){
  # If candidate covariate has < 0.6 correlation with existing covariates.
  if (sum(abs_mat[i, test_cov_Any])< 1){
    test_cov_Any <- c(test_cov_Any, i)
  }
}

AIC_HMIS_order <- names(sort(AIC_HMIS))

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

########## ---------------------- EW Africa 2. FIT THE PRELIMINARY MODELS ON TS MEANS OF TRAINING DATA -------------------- ############

#  Use covariates identified in test_cov_Any:

formula_any <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(DMSP_nighttime) + s(ANC4_coverage_prop, k = 3) + s(oop_hexp_cap, k = 3) + s(IFD_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(accessibility, k = 3) + s(log_the_pc, k = 3)
test.model <- gam(formula_any, data = train_Any_EWAfrica)

summary(test.model)

model_any <- uGamm(logit_Any ~ -1 + IHME_Region_Name + s(Year), data = train_Any_EWAfrica)

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed=c("IHME_Region_Name", "s(Year)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 26.25 s.
model.select.any[1:10]

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 

formula_any_1 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(ANC4_coverage_prop, k = 3) + s(oop_hexp_cap, k = 3)

model_any_1 <- gam(formula_any_1, data = train_Any_EWAfrica)
# plot(model_any_1)

pdf(paste(graphics.path, "EWAfrica_Any_smooths_1.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_1, select = 1) # Degree 6.89. - can restrict to 7. 
plot(model_any_1, select = 2) 
plot(model_any_1, select = 3) 
# plot(model_any_1, select = 4)
# plot(model_any_1, select = 5)
# plot(model_any_1, select = 6)
dev.off()

formula_any_2 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(ANC4_coverage_prop, k = 3) + s(log_the_pc, k = 3)

model_any_2 <- gam(formula_any_2, data = train_Any_EWAfrica)
# plot(model_any_2)

pdf(paste(graphics.path, "EWAfrica_Any_smooths_2.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_2, select = 1) 
plot(model_any_2, select = 2) 
plot(model_any_2, select = 3) 
# plot(model_any_2, select = 4)
# plot(model_any_2, select = 5)
# plot(model_any_2, select = 6)
dev.off()

# Relation with hospital beds per 1000 in formulas 3 and 4 don't look very sensible.

formula_any_3 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(hospital_beds_per1000, k = 3) + s(log_the_pc, k = 3)

model_any_3 <- gam(formula_any_3, data = train_Any_EWAfrica)
# plot(model_any_3)

pdf(paste(graphics.path, "EWAfrica_Any_smooths_3.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_3, select = 1) 
plot(model_any_3, select = 2) 
plot(model_any_3, select = 3) # Relation with hospital beds not very clear - tapering off?
# plot(model_any_3, select = 4)
# plot(model_any_3, select = 5)
# plot(model_any_3, select = 6)
dev.off()

formula_any_4 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(ANC4_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(log_the_pc, k = 3)

model_any_4 <- gam(formula_any_4, data = train_Any_EWAfrica)
# plot(model_any_4)

pdf(paste(graphics.path, "EWAfrica_Any_smooths_4.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_4, select = 1) 
plot(model_any_4, select = 2) 
plot(model_any_4, select = 3) 
plot(model_any_4, select = 4)
# plot(model_any_4, select = 5)
# plot(model_any_4, select = 6)
dev.off()

formula_hmis <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year)+ s(oop_hexp_cap, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(ind_health, k = 3) + s(accessibility, k =3) + s(DMSP_nighttime, k = 3) 
test.model <- gam(formula_hmis, data = train_HMISfrac_EWAfrica)

summary(test.model)

model_hmis <- uGamm(logit_HMIS ~ -1 + IHME_Region_Name + s(Year), data = train_HMISfrac_EWAfrica)

model_hmis$gam$formula <- formula_hmis

temptime <- proc.time()[3]
model.select.hmis <- dredge(model_hmis, fixed=c("IHME_Region_Name", "s(Year)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 23.85 s.
model.select.hmis[1:10]

best.hmis<-subset(model.select.hmis, delta < 2) # Best models with delta AICc less than 2 . 

formula_hmis_1 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(DMSP_nighttime, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(oop_hexp_cap, k = 3) 
formula_hmis_2 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(DMSP_nighttime, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(oop_hexp_cap, k = 3) 

model_hmis_1 <- gam(formula_hmis_1, data = train_HMISfrac_EWAfrica)
summary(model_hmis_1) # 72.9% deviance explained.
# plot(model_hmis_1)

pdf(paste(graphics.path, "EWAfrica_HMISfrac_smooths_1.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_hmis_1, select = 1) 
plot(model_hmis_1, select = 2) 
plot(model_hmis_1, select = 3) 
plot(model_hmis_1, select = 4) 
plot(model_hmis_1, select = 5) 
plot(model_hmis_1, select = 6) 
dev.off()

model_hmis_2 <- gam(formula_hmis_2, data = train_HMISfrac_EWAfrica)
summary(model_hmis_2) # 72.9% deviance explained.
# plot(model_hmis_2)

pdf(paste(graphics.path, "EWAfrica_HMISfrac_smooths_2.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_hmis_2, select = 1) 
plot(model_hmis_2, select = 2) 
plot(model_hmis_2, select = 3) 
plot(model_hmis_2, select = 4) 
plot(model_hmis_2, select = 5) 
dev.off()


########## ---------------------- S Asia 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

## Correlation between covariates:

# Get columns with covariates. 
# Don't consider hospital_beds_per1000 because there is only 2 data points in the 0.5-2.5 range.
cov_col <- clean_TS_SAsia_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                       'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop", "educ_yrs_age_std_pc_1", "educ_yrs_age_std_pc_2",
                                       "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "universal_health_coverage", "haqi", "measles_vacc_cov_prop_2", "ind_health", 
                                       "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:30, 34:35, 40:42, 44:46) # Removed oop_hexp_cap, frac_hexp and log_the_pc, and restricted the basis dimension of time to 8 because not enough distinct values.
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TS_SAsia_HMISfrac)[cov_ind]

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TS_SAsia_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  var_values_Any <- train_Any_SAsia[, cov_ind[i]] 
  gam_Any <- gam(logit_Any ~ -1 + SMT_Factor + s(Year, k = 8) + s(X), data = data.frame("logit_Any" = train_Any_SAsia$logit_Any, "X" = var_values_Any, 'Year' = train_Any_SAsia$Year, 'IHME_Region_Name' = train_Any_SAsia$IHME_Region_Name, 'SMT_Factor' = train_Any_SAsia$SMT_Factor, 'Time_Factor' = train_Any_SAsia$Time_Factor))
  var_values_HMIS <- train_HMISfrac_SAsia[, cov_ind[i]]
  gam_HMIS <- gam(logit_HMIS ~ -1 + SMT_Factor + s(Year, k = 8) + s(X), data =  data.frame("logit_HMIS" = train_HMISfrac_SAsia$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_HMISfrac_SAsia$Year, 'IHME_Region_Name' = train_HMISfrac_SAsia$IHME_Region_Name, 'SMT_Factor' = train_HMISfrac_SAsia$SMT_Factor, 'Time_Factor' = train_HMISfrac_SAsia$Time_Factor))
  AIC_Any[i] <- gam_Any$aic
  AIC_HMIS[i] <- gam_HMIS$aic
}

AIC_Any_order <- names(sort(AIC_Any[names(AIC_Any) != "frac_oop_hexp"]))# Don't use frac_oop_hexp for any TS.

test_cov_mat <- cor(cov_col[, AIC_Any_order])
abs_mat <- abs(test_cov_mat) >0.6 # For checking which covariates are strongly correlated to those of increasing AIC.

test_cov_Any <- AIC_Any_order[1]

for (i in AIC_Any_order[-1]){
  # If candidate covariate has < 0.6 correlation with existing covariates.
  if (sum(abs_mat[i, test_cov_Any])< 1){
    test_cov_Any <- c(test_cov_Any, i)
  }
}

AIC_HMIS_order <- names(sort(AIC_HMIS))

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

########## ---------------------- S Asia 2. FIT THE PRELIMINARY MODELS ON TS MEANS OF TRAINING DATA -------------------- ############

#  Use covariates identified in test_cov_Any:

formula_any <-  logit_Any ~ -1 + s(Year, k = 8) +  SMT_Factor + s(DMSP_nighttime, k = 3) + s(LDI_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) 
test.model <- gam(formula_any, data = train_Any_SAsia)

summary(test.model)

model_any <- uGamm(logit_Any ~ -1 + SMT_Factor + s(Year, k = 8), data = train_Any_SAsia)

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed=c("SMT_Factor", "s(Year, k = 8)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 0.63 s.
model.select.any[1:10]

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 

formula_any_1 <-  logit_Any ~ -1 + SMT_Factor + s(Year, k = 8) + s(DMSP_nighttime, k = 3) + s(LDI_pc, k = 3) 

model_any_1 <- gam(formula_any_1, data = train_Any_SAsia)
# plot(model_any_1)

pdf(paste(graphics.path, "SAsia_Any_smooths_1.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_1, select = 1) 
plot(model_any_1, select = 2) 
plot(model_any_1, select = 3) 
# plot(model_any_1, select = 4)
# plot(model_any_1, select = 5)
# plot(model_any_1, select = 6)
dev.off()

formula_any_2 <-  logit_Any ~ -1 + SMT_Factor + s(Year, k = 8) + s(DMSP_nighttime, k = 3) + s(measles_vacc_cov_prop, k = 3) 

model_any_2 <- gam(formula_any_2, data = train_Any_SAsia)
# plot(model_any_2)

pdf(paste(graphics.path, "SAsia_Any_smooths_2.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_2, select = 1) 
plot(model_any_2, select = 2) 
plot(model_any_2, select = 3) 
# plot(model_any_2, select = 4)
# plot(model_any_2, select = 5)
# plot(model_any_2, select = 6)
dev.off()


formula_hmis <-  logit_HMIS ~ -1 + s(Year, k = 8) + SMT_Factor + s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(IFD_coverage_prop, k = 3) + s(measles_vacc_cov_prop, k = 3)
test.model <- gam(formula_hmis, data = train_HMISfrac_SAsia)

summary(test.model)

model_hmis <- uGamm(logit_HMIS ~ -1 + SMT_Factor + s(Year, k = 8), data = train_HMISfrac_SAsia)

model_hmis$gam$formula <- formula_hmis

temptime <- proc.time()[3]
model.select.hmis <- dredge(model_hmis, fixed=c("SMT_Factor", "s(Year, k = 8)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 1.57 s.
model.select.hmis[1:10]

best.hmis<-subset(model.select.hmis, delta < 2) # Best models with delta AICc less than 2 . 

formula_hmis_1 <-  logit_HMIS ~ -1 + SMT_Factor + s(Year, k = 8) + s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3)

model_hmis_1 <- gam(formula_hmis_1, data = train_HMISfrac_SAsia)
summary(model_hmis_1)
# plot(model_hmis_1)

pdf(paste(graphics.path, "SAsia_HMISfrac_smooths_1.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_hmis_1, select = 1) 
plot(model_hmis_1, select = 2) 
plot(model_hmis_1, select = 3) 
dev.off()


########## ---------------------- Other 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

## Correlation between covariates:

# Get columns with covariates. 
# Don't consider hospital_beds_per1000 because there is only 2 data points in the 0.5-2.5 range.
cov_col <- clean_TS_Other_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                       'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop", "educ_yrs_age_std_pc_1", "educ_yrs_age_std_pc_2",
                                       "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "universal_health_coverage", "haqi", "measles_vacc_cov_prop_2", "ind_health", 
                                       "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:30, 34:37, 40:46)
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TS_Other_HMISfrac)[cov_ind]

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TS_Other_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  var_values_Any <- train_Any_Other[, cov_ind[i]] 
  gam_Any <- gam(logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(X), data = data.frame("logit_Any" = train_Any_Other$logit_Any, "X" = var_values_Any, 'Year' = train_Any_Other$Year, 'IHME_Region_Name' = train_Any_Other$IHME_Region_Name, 'SMT_Factor' = train_Any_Other$SMT_Factor, 'Time_Factor' = train_Any_Other$Time_Factor))
  var_values_HMIS <- train_HMISfrac_Other[, cov_ind[i]]
  gam_HMIS <- gam(logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(X), data =  data.frame("logit_HMIS" = train_HMISfrac_Other$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_HMISfrac_Other$Year, 'IHME_Region_Name' = train_HMISfrac_Other$IHME_Region_Name, 'SMT_Factor' = train_HMISfrac_Other$SMT_Factor, 'Time_Factor' = train_HMISfrac_Other$Time_Factor))
  AIC_Any[i] <- gam_Any$aic
  AIC_HMIS[i] <- gam_HMIS$aic
}

AIC_Any_order <- names(sort(AIC_Any[names(AIC_Any) != "frac_oop_hexp"]))# Don't use frac_oop_hexp for any TS.

test_cov_mat <- cor(cov_col[, AIC_Any_order])
abs_mat <- abs(test_cov_mat) >0.6 # For checking which covariates are strongly correlated to those of increasing AIC.

test_cov_Any <- AIC_Any_order[1]

for (i in AIC_Any_order[-1]){
  # If candidate covariate has < 0.6 correlation with existing covariates.
  if (sum(abs_mat[i, test_cov_Any])< 1){
    test_cov_Any <- c(test_cov_Any, i)
  }
}

AIC_HMIS_order <- names(sort(AIC_HMIS))

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

########## ---------------------- Other 2. FIT THE PRELIMINARY MODELS ON TS MEANS OF TRAINING DATA -------------------- ############

#  Use covariates identified in test_cov_Any:

formula_any <-  logit_Any ~ -1 + s(Year) + IHME_Region_Name + s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(ANC4_coverage_prop, k = 3) + s(prop_urban, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(IFD_coverage_prop, k = 3) + s(measles_vacc_cov_prop_2, k = 3) + s(GDPpc_id_b2010, k = 3)
test.model <- gam(formula_any, data = train_Any_Other)

summary(test.model)

model_any <- uGamm(logit_Any ~ -1 + IHME_Region_Name + s(Year), data = train_Any_Other)

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed=c("IHME_Region_Name", "s(Year)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 23.85 s.
model.select.any[1:10]

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 

formula_any_1 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(DMSP_nighttime, k = 3) + s(LDI_pc, k = 3) 

model_any_1 <- gam(formula_any_1, data = train_Any_Other)
# plot(model_any_1)

pdf(paste(graphics.path, "Other_Any_smooths_1.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_1, select = 1) 
plot(model_any_1, select = 2) 
plot(model_any_1, select = 3) 
# plot(model_any_1, select = 4)
# plot(model_any_1, select = 5)
# plot(model_any_1, select = 6)
dev.off()

formula_any_2 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(DMSP_nighttime, k = 3) + s(measles_vacc_cov_prop, k = 3) 

model_any_2 <- gam(formula_any_2, data = train_Any_Other)
# plot(model_any_2)

pdf(paste(graphics.path, "Other_Any_smooths_2.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_any_2, select = 1) 
plot(model_any_2, select = 2) 
plot(model_any_2, select = 3) 
# plot(model_any_2, select = 4)
# plot(model_any_2, select = 5)
# plot(model_any_2, select = 6)
dev.off()

formula_hmis <-  logit_HMIS ~ -1 + s(Year) +  IHME_Region_Name +  s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(VIIRS_nighttime, k = 3) + s(frac_oop_hexp, k = 3)  + s(measles_vacc_cov_prop, k = 3) + s(ANC4_coverage_prop, k = 3) + s(prop_urban, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(IFD_coverage_prop, k = 3) + s(measles_vacc_cov_prop_2, k = 3) + s(log_the_pc, k = 3)
test.model <- gam(formula_hmis, data = train_HMISfrac_Other)

summary(test.model)

model_hmis <- uGamm(logit_HMIS ~ -1 + IHME_Region_Name + s(Year), data = train_HMISfrac_Other)

model_hmis$gam$formula <- formula_hmis

temptime <- proc.time()[3]
model.select.hmis <- dredge(model_hmis, fixed=c("IHME_Region_Name", "s(Year)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 23.85 s.
model.select.hmis[1:10]

best.hmis<-subset(model.select.hmis, delta < 2) # Best models with delta AICc less than 2 . 

formula_hmis_1 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(log_the_pc, k = 3) + s(prop_urban, k = 3) 
formula_hmis_2 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(log_the_pc, k = 3) + s(VIIRS_nighttime, k = 3) 

model_hmis_1 <- gam(formula_hmis_1, data = train_HMISfrac_Other)
summary(model_hmis_1) # 81.4%
# plot(model_hmis_1)

pdf(paste(graphics.path, "Other_HMISfrac_smooths_1.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_hmis_1, select = 1) 
plot(model_hmis_1, select = 2) 
plot(model_hmis_1, select = 3) 
# plot(model_hmis_1, select = 6)
dev.off()

model_hmis_2 <- gam(formula_hmis_2, data = train_HMISfrac_Other)
summary(model_hmis_2) # 80.2%
# plot(model_hmis_2)

pdf(paste(graphics.path, "Other_HMISfrac_smooths_2.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_hmis_2, select = 1) 
plot(model_hmis_2, select = 2) 
plot(model_hmis_2, select = 3) 
# plot(model_hmis_2, select = 6)
dev.off()

# Best model makes more sense - first one has strange trend with vacc.