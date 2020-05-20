# ------- Model validation

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

model_no <- "Final_" 

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
test_data_Any$IHME_location_id <- as.factor(test_data_Any$IHME_location_id)
test_data_HMISfrac$IHME_location_id <- as.factor(test_data_HMISfrac$IHME_location_id)

formula_any_1 <-  logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(GDPpc_id_b2010, k = 3) 

formula_hmis_1 <-  logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2, k = 5) + s(ANC1_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(frac_oop_hexp, k = 3) + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(prop_urban, k = 3) + s(VIIRS_nighttime, k = 3)

# Use best models:
Any_model <-  gamm(formula_any_1, data = train_data_Any, random = list(IHME_location_id = ~ 1))
HMIS_model <- gamm(formula_hmis_1, data = train_data_HMISfrac, random = list(IHME_location_id = ~ 1), control = list(opt='optim'))

train.predict.any <- predict(Any_model)
train.predict.HMIS <- predict(HMIS_model)

master.region.list <- unique(train_data_Any$IHME_Region_Name)
master.unit.list <- unique(train_data_Any$IHME_location_id)

Any_rf <- rep(NA, nrow(train_data_Any))

# for (j in 1:length(master.region.list)){
#   units.in.region <- unique(train_data_Any$IHME_location_id[train_data_Any$IHME_Region_Name == master.region.list[j]])
for (i in 1:length(master.unit.list)){
  # for (i in 1:length(units.in.region)){
  # 1. REF:
  # Any_rf[train_data_Any$IHME_location_id == as.character(units.in.region[i])] <- ranef(Any_model$lme,level= 11)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(Any_model$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(Any_model$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
  Any_rf[train_data_Any$IHME_location_id == as.character(master.unit.list[i])] <- ifelse(is.na(ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ]), 0, ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ])
}
# }

Any_rf[is.na(Any_rf)] <- 0

HMIS_rf <- rep(NA, nrow(train_data_HMISfrac))

# for (j in 1:length(master.region.list)){
# units.in.region <- unique(train_data_HMISfrac$IHME_location_id[train_data_HMISfrac$IHME_Region_Name == master.region.list[j]])
for (i in 1:length(master.unit.list)){
  # for (i in 1:length(units.in.region)){
  # 1. REF:
  # HMIS_rf[train_data_HMISfrac$IHME_location_id == as.character(units.in.region[i])] <- ranef(HMIS_model$lme,level= 14)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(HMIS_model$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
  # 2. REF:
  HMIS_rf[train_data_HMISfrac$IHME_location_id == as.character(master.unit.list[i])] <- ifelse(is.na(ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ])
}
# }

HMIS_rf[is.na(HMIS_rf)] <- 0

(sum(Any_rf == 0) + sum(HMIS_rf == 0))==0

train.predict.any <- predict(Any_model, newdata = train_data_Any) + Any_rf
train.predict.HMIS <- predict(HMIS_model, newdata = train_data_HMISfrac) + HMIS_rf

master.region.list <- unique(test_data_Any$IHME_Region_Name)
master.unit.list <- unique(test_data_Any$IHME_location_id)

Any_rf_2 <- rep(NA, nrow(test_data_Any))

# for (j in 1:length(master.region.list)){
#   units.in.region <- unique(test_data_Any$IHME_location_id[test_data_Any$IHME_Region_Name == master.region.list[j]])
for (i in 1:length(master.unit.list)){
  # for (i in 1:length(units.in.region)){
  # 1. REF:
  # Any_rf_2[test_data_Any$IHME_location_id == as.character(units.in.region[i])] <- ranef(Any_model$lme,level= 11)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(Any_model$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(Any_model$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
  Any_rf_2[test_data_Any$IHME_location_id == as.character(master.unit.list[i])] <- ifelse(is.na(ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ]), 0, ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ])
}
# }

Any_rf_2[is.na(Any_rf_2)] <- 0

HMIS_rf_2 <- rep(NA, nrow(test_data_HMISfrac))

# for (j in 1:length(master.region.list)){
# units.in.region <- unique(test_data_HMISfrac$IHME_location_id[test_data_HMISfrac$IHME_Region_Name == master.region.list[j]])
for (i in 1:length(master.unit.list)){
  # for (i in 1:length(units.in.region)){
  # 1. REF:
  # HMIS_rf_2[test_data_HMISfrac$IHME_location_id == as.character(units.in.region[i])] <- ranef(HMIS_model$lme,level= 14)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(HMIS_model$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
  # 2. REF:
  HMIS_rf_2[test_data_HMISfrac$IHME_location_id == as.character(master.unit.list[i])] <- ifelse(is.na(ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", master.unit.list[i], sep = ""), ])
}
# }

HMIS_rf_2[is.na(HMIS_rf_2)] <- 0

(sum(Any_rf_2 == 0) + sum(HMIS_rf_2 == 0))==0 # There are test units which have no data in training set.

test.predict.any <- predict(Any_model, newdata = test_data_Any) + Any_rf_2
test.predict.HMIS <- predict(HMIS_model, newdata = test_data_HMISfrac) + HMIS_rf_2


# Model validation:

pdf(paste(graphics.path, "Model_validation_plots.pdf", sep = ""), width = 9, height = 9)
par(mfrow = c(2, 2), mai = rep(0.4, 4))
plot(train.predict.any, train.predict.any - train_data_Any$logit_Any, main = "Residuals versus fitted values (logit any)", ylim = c(-2.5, 2.5))
abline(h = 0)
plot(train.predict.HMIS, train.predict.HMIS - train_data_HMISfrac$logit_HMIS, main = "Residuals versus fitted values (logit public fraction)", ylim = c(-2.5, 2.5))
abline(h = 0)
hist(train.predict.any - train_data_Any$logit_Any, main = 'Histogram of residuals (logit any)')
hist(train.predict.HMIS - train_data_HMISfrac$logit_HMIS, main = 'Histogram of residuals (logit public fraction)')
dev.off()

pdf(paste(graphics.path, "Fitted_vs_observed.pdf", sep = ""), width = 9, height = 4.5)
par(mfrow = c(1, 2), mai = rep(0.4, 4))
plot(train_data_Any$logit_Any, train.predict.any, main = "Fitted versus observed values (logit any)", ylim = c(min(train_data_Any$logit_Any), max(train_data_Any$logit_Any)), xlim = c(min(train_data_Any$logit_Any), max(train_data_Any$logit_Any)))
lines(train_data_Any$logit_Any, train_data_Any$logit_Any)
plot(train_data_HMISfrac$logit_HMIS, train.predict.HMIS, main = "Fitted versus observed values (logit public fraction)", ylim = c(min(train_data_Any$logit_Any), max(train_data_Any$logit_Any)), xlim = c(min(train_data_HMISfrac$logit_HMIS), max(train_data_HMISfrac$logit_HMIS)))
lines(train_data_HMISfrac$logit_HMIS, train_data_HMISfrac$logit_HMIS)
dev.off()

pdf(paste(graphics.path, "Fitted_vs_observed_rawscale.pdf", sep = ""), width = 9, height = 4.5)
par(mfrow = c(1, 2), mai = rep(0.4, 4))
plot(train_data_Any$Any_treat, inv.logit(train.predict.any), main = "Fitted versus observed values (any)", ylim = c(0, 1), xlim = c(0, 1))
lines(c(0, 1), c(0, 1))
plot(train_data_HMISfrac$HMIS_frac, inv.logit(train.predict.HMIS), main = "Fitted versus observed values (public fraction)", ylim = c(0, 1), xlim = c(0, 1))
lines(c(0, 1), c(0, 1))
dev.off()

# qqnorm(Any_model$residuals) - thinner tails than normal but good coherence in the middle of the data which is more important?
# qqline(Any_model$residuals)
# qqnorm(train.predict.HMIS - train_data_HMISfrac$logit_HMIS) # Slightly thinner tails than normal but better than Any model.
# qqline(train.predict.HMIS - train_data_HMISfrac$logit_HMIS)

# Residuals uncorrelated with fitted values:
# cor.test(train.predict.any, train.predict.any - train_data_Any$logit_Any)
# cor.test(train.predict.HMIS, train.predict.HMIS - train_data_HMISfrac$logit_HMIS)

# Compare training and test RMSE - amount of overfitting.

train.error.any <- sqrt(mean((train.predict.any - train_data_Any$logit_Any)^2))  # 0.4283441. (logit scale)
train.error.hmis <- sqrt(mean((train.predict.HMIS - train_data_HMISfrac$logit_HMIS)^2)) # 0.484026. (logit scale) 

test.error.any <- sqrt(mean((test.predict.any - test_data_Any$logit_Any)^2)) # 0.6005164. (logit scale)
test.error.hmis <- sqrt(mean((test.predict.HMIS - test_data_HMISfrac$logit_HMIS)^2)) # 0.5607164. (logit scale)

train.error.any.raw <- sqrt(mean((train_data_Any$Any_treat - inv.logit(train.predict.any))^2)) #  0.08914954.
train.error.hmis.raw <- sqrt(mean((train_data_HMISfrac$HMIS_frac - inv.logit(train.predict.HMIS))^2)) # 0.09750537.

test.error.any.raw <- sqrt(mean((test_data_Any$Any_treat - inv.logit(test.predict.any))^2)) # 0.1213506.
test.error.hmis.raw <- sqrt(mean((test_data_HMISfrac$HMIS_frac - inv.logit(test.predict.HMIS))^2)) # 0.1231298.

# Training and test error are comparable.

