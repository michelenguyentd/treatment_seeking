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

model_no <- "PTTIR_" # 1. RTTIR: IHME region temporal trend and factors; 2. PTTIR: Pruned regions temporal trend and IHME regional factors; 3. PTTPF: Pruned regions temporal trend and factors; 4. OC: Other considerations.

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

# Colombia and Dominican Republic are in training set because data in regions < 50.

########## ---------------------- 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

## Correlation between covariates:

# Get columns with covariates. 
cov_col <- clean_TreatSeek_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop", 
                                "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "measles_vacc_cov_prop_2", "ind_health", 
                                "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:42)
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TreatSeek_HMISfrac)[cov_ind] # Check names

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TreatSeek_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  var_values_Any <- train_data_Any[, cov_ind[i]]
  gam_Any <- gam(logit_Any ~ -1 + IHME_Region_Name + s(Year, by = Time_Factor) + s(X), data = data.frame("logit_Any" = train_data_Any$logit_Any, "X" = var_values_Any, 'Year' = train_data_Any$Year, 'IHME_Region_Name' = train_data_Any$IHME_Region_Name, 'Time_Factor' = train_data_Any$Time_Factor))
  var_values_HMIS <- train_data_HMISfrac[, cov_ind[i]]
  gam_HMIS <- gam(logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = Time_Factor_2) + s(X), data =  data.frame("logit_HMIS" = train_data_HMISfrac$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_data_HMISfrac$Year, 'IHME_Region_Name' = train_data_HMISfrac$IHME_Region_Name, 'Time_Factor_2' = train_data_HMISfrac$Time_Factor_2))
  AIC_Any[i] <- gam_Any$aic
  AIC_HMIS[i] <- gam_HMIS$aic
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

formula_any <-  logit_Any ~ -1 + IHME_Region_Name + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(log_the_pc, k = 3) + s(DMSP_nighttime, k = 3)  + s(hospital_beds_per1000, k = 3) 
test.model <- gam(formula_any, data = train_data_Any)

summary(test.model)
# plot(test.model, ylim = c(-5, 5)) # Check for strange relations.

model_any <- uGamm(logit_Any ~-1 + IHME_Region_Name + s(Year), data = train_data_Any)

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed=c("IHME_Region_Name", "s(Year, by = Time_Factor, k = 5)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # About 1 minute.
model.select.any[1:10]

formula_HMIS <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = Time_Factor_2, k = 5) + s(ANC1_coverage_prop, k = 3) + s(VIIRS_nighttime, k = 3) + s(frac_oop_hexp, k = 3) + s(prop_urban, k = 3) + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(hospital_beds_per1000, k = 3) 

test.model <- gam(formula_HMIS, data = train_data_HMISfrac)

summary(test.model)
# plot(test.model, ylim = c(-5, 5)) # Check for strange relations.

model_HMIS <- uGamm(logit_HMIS ~-1 + IHME_Region_Name + s(Year), data = train_data_HMISfrac)

model_HMIS$gam$formula <- formula_HMIS

temptime2 <- proc.time()[3]
model.select.HMIS <- dredge(model_HMIS, fixed=c("IHME_Region_Name", 's(Year, by = Time_Factor_2, k = 5)'), m.min=4)
timetaken2 <- proc.time()[3] - temptime2
model.select.HMIS[1:10]
# About 1 minute.

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 
best.HMIS<-subset(model.select.HMIS, delta < 2) #takes the best model with delta AICc less than 2 - only 1. 

print(xtable(best.any, type = "latex"), file = paste(table.path, model_no, "best.any.tex"))
print(xtable(best.HMIS, type = "latex"), file = paste(table.path, model_no, "best.HMIS.tex"))


########## ---------------------- 3. EXAMINE THE FINAL MODELS -------------------- ############

# Examine components of chosen models:

formula_any_1 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(log_the_pc, k = 3) + s(DMSP_nighttime, k = 3)  + s(hospital_beds_per1000, k = 3) 

model_any_1 <- gam(formula_any_1, data = train_data_Any)
sum_any_1 <- summary(model_any_1) # PTTIR: 49.9%

sum_any_1

reg_coeff <- sum_any_1$p.coeff
col.id <- rep(1, length(reg_coeff))
# col.id[!(names(reg_coeff) %in% c('IHME_Region_NameEastern Sub-Saharan Africa', 'IHME_Region_NameCentral Latin America', "IHME_Region_NameNorth Africa and Middle East", "IHME_Region_NameSouth Asia", "IHME_Region_NameTropical Latin America", "IHME_Region_NameWestern Sub-Saharan Africa"))] <- 2
col.id[(names(reg_coeff) %in% c('IHME_Region_NameEastern Sub-Saharan Africa',"IHME_Region_NameSoutheast Asia", "IHME_Region_NameWestern Sub-Saharan Africa", "IHME_Region_NameSouth Asia", 'IHME_Region_NameCentral Asia'))] <- 2


pdf(paste(graphics.path, model_no, "Any_intercepts_1.pdf", sep = ""), width = 12, height = 4)
plotCI(1:length(reg_coeff), reg_coeff, ui = reg_coeff + qnorm(0.975)*sum_any_1$se[grep("IHME_Region_Name", names(sum_any_1$se))], li = reg_coeff - qnorm(0.975)*sum_any_1$se[grep("IHME_Region_Name", names(sum_any_1$se))], xlab = 'IHME Regions', ylab = 'Intercept estimates', xaxt = 'n', col = col.id)
abline(h = 0, lty = 2)
abline(h = mean(reg_coeff[!(names(reg_coeff) %in% c('IHME_Region_NameEastern Sub-Saharan Africa', "IHME_Region_NameSoutheast Asia", "IHME_Region_NameWestern Sub-Saharan Africa", "IHME_Region_NameSouth Asia", 'IHME_Region_NameCentral Asia'))]), col = 2)
axis(side=1, at=1:length(reg_coeff), labels = c("ALA", 'Car.', 'C.Asia', 'CLA', 'CSA', 'ESA', 'NAME', 'S.Asia', 'SE.Asia', 'SSA', 'TLA', 'WSA'))
legend('topright', lty = 1, col = 2, legend = c('Mean of base regions'))
dev.off()

# To be changed for different models:

gamtabs(model_any_1, caption = "Summary of best any TS model with pruned region temporal trends and IHME region factors")

pdf(paste(graphics.path, model_no, "Any_model_smooths_1.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_any_1, select = 1, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1, select = 2, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 3, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 4, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1, select = 5, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1, select = 6, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1, select = 7, ylim = c(-5, 5))
plot(model_any_1, select = 8, ylim = c(-5, 5))
plot(model_any_1, select = 9, ylim = c(-5, 5))
plot(model_any_1, select = 10, ylim = c(-5, 5))
plot(model_any_1, select = 11, ylim = c(-5, 5))
# plot(model_any_1, select = 12, ylim = c(-5, 5))
# plot(model_any_1, select = 13, ylim = c(-5, 5))  
# plot(model_any_1, select = 14, ylim = c(-5, 5)) 
dev.off()

formula_hmis_1 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = Time_Factor_2, k = 5) + s(frac_oop_hexp, k = 3) + s(hospital_beds_per1000, k = 3) + s(log_the_pc, k =3) + s(measles_vacc_cov_prop, k = 3) + s(prop_urban, k = 3) + s(VIIRS_nighttime, k = 3)

model_hmis_1 <- gam(formula_hmis_1, data = train_data_HMISfrac)
sum_hmis_1 <- summary(model_hmis_1) # 51.3%.

gamtabs(model_hmis_1, caption = "Summary of best public fraction model with pruned region temporal trends and IHME region factors")

reg_coeff_2 <- sum_hmis_1$p.coeff

col.id_2 <- rep(1, length(reg_coeff_2))
col.id_2[names(reg_coeff_2) %in% c('IHME_Region_NameCentral Asia', 'IHME_Region_NameSouth Asia', 'IHME_Region_NameSoutheast Asia', "IHME_Region_NameEastern Sub-Saharan Africa", "IHME_Region_NameWestern Sub-Saharan Africa", "IHME_Region_NameSouthern Sub-Saharan Africa")] <- 2


pdf(paste(graphics.path, model_no, "HMIS_intercepts_1.pdf", sep = ""), width = 12, height = 4)
plotCI(1:length(reg_coeff_2), reg_coeff_2, ui = reg_coeff_2 + qnorm(0.975)*sum_hmis_1$se[grep("IHME_Region_Name", names(sum_hmis_1$se))], li = reg_coeff_2 - qnorm(0.975)*sum_hmis_1$se[grep("IHME_Region_Name", names(sum_hmis_1$se))], xlab = 'IHME Regions', ylab = 'Intercept estimates', xaxt = 'n', col = col.id_2)
abline(h = 0, lty = 2)
abline(h = mean(reg_coeff_2[!(names(reg_coeff_2) %in%  c('IHME_Region_NameCentral Asia', 'IHME_Region_NameSouth Asia', 'IHME_Region_NameSoutheast Asia', "IHME_Region_NameEastern Sub-Saharan Africa", "IHME_Region_NameWestern Sub-Saharan Africa", "IHME_Region_NameSouthern Sub-Saharan Africa"))]), col = 2)
axis(side=1, at=1:length(reg_coeff_2), labels = c("ALA", 'Car.', 'C.Asia', 'CLA', 'CSA', 'ESA', 'NAME', 'S.Asia', 'SE.Asia', 'SSA', 'TLA', 'WSA'))
legend('topright', lty = 1, col = 2, legend = c('Mean of base regions'))
dev.off()

pdf(paste(graphics.path, model_no, "HMIS_model1_smooths.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_hmis_1, select = 1, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_hmis_1, select = 2, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 3, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_hmis_1, select = 4, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 5, ylim = c(-5, 5), xlim = c(1990, 2019))   
plot(model_hmis_1, select = 6, ylim = c(-5, 5))  
plot(model_hmis_1, select = 7, ylim = c(-5, 5))  
plot(model_hmis_1, select = 8, ylim = c(-5, 5))  
plot(model_hmis_1, select = 9, ylim = c(-5, 5))  
plot(model_hmis_1, select = 10, ylim = c(-5, 5))  
plot(model_hmis_1, select = 11, ylim = c(-5, 5))
plot(model_hmis_1, select = 12, ylim = c(-5, 5))  
plot(model_hmis_1, select = 13, ylim = c(-5, 5))
dev.off()

# # Use best models:
# Any_model <-  gam(formula_any_1, data = train_data_Any)
# HMIS_model <- gam(formula_hmis_1, data = train_data_HMISfrac)
# 
# train.predict.any <- predict(Any_model)
# train.predict.HMIS <- predict(HMIS_model)

# train.predict.HMIS <- best.HMIS$weight[1]*predict(model_hmis_1) + best.HMIS$weight[2]*predict(model_hmis_2) 
# Use averaged predictions instead of predictions from averaged coefficients - makes more sense in the light of non-linearity.
# Weight = Akaike weight.

# test.predict.any <- predict(Any_model, newdata = test_data_Any)
# test.predict.HMIS <- predict(HMIS_model, newdata = test_data_HMISfrac)

# test.predict.HMIS <- best.HMIS$weight[1]*predict(model_hmis_1, newdata = test_data_HMISfrac) + best.HMIS$weight[2]*predict(model_hmis_2, newdata = test_data_HMISfrac) 

# Model validation:

# pdf(paste(graphics.path, "Model_validation_plots.pdf", sep = ""), width = 12, height = 8)
# par(mfrow = c(2, 2), mai = rep(0.4, 4))
# plot(Any_model$fitted.values, Any_model$residuals, main = "Residuals versus fitted values (logit any)", ylim = c(-2.5, 2.5))
# abline(h = 0)
# plot(train.predict.HMIS, train.predict.HMIS - train_data_HMISfrac$logit_HMIS, main = "Residuals versus fitted values (logit public fraction)", ylim = c(-2.5, 2.5))
# abline(h = 0)
# hist(Any_model$residuals, main = 'Histogram of residuals (logit any)')
# hist(train.predict.HMIS - train_data_HMISfrac$logit_HMIS, main = 'Histogram of residuals (logit public fraction)')
# dev.off()
# 
# pdf(paste(graphics.path, "Fitted_vs_observed.pdf", sep = ""), width = 12, height = 4)
# par(mfrow = c(1, 2), mai = rep(0.4, 4))
# plot(train_data_Any$logit_Any, Any_model$fitted.values, main = "Fitted versus observed values (logit any)", ylim = c(min(train_data_Any$logit_Any), max(train_data_Any$logit_Any)), xlim = c(min(train_data_Any$logit_Any), max(train_data_Any$logit_Any)))
# lines(train_data_Any$logit_Any, train_data_Any$logit_Any)
# plot(train_data_HMISfrac$logit_HMIS, train.predict.HMIS, main = "Fitted versus observed values (logit public fraction)", ylim = c(min(train_data_Any$logit_Any), max(train_data_Any$logit_Any)), xlim = c(min(train_data_HMISfrac$logit_HMIS), max(train_data_HMISfrac$logit_HMIS)))
# lines(train_data_HMISfrac$logit_HMIS, train_data_HMISfrac$logit_HMIS)
# dev.off()
# 
# pdf(paste(graphics.path, "Fitted_vs_observed_rawscale.pdf", sep = ""), width = 12, height = 4)
# par(mfrow = c(1, 2), mai = rep(0.4, 4))
# plot(train_data_Any$Any_treat, inv.logit(Any_model$fitted.values), main = "Fitted versus observed values (any)", ylim = c(0, 1), xlim = c(0, 1))
# lines(c(0, 1), c(0, 1))
# plot(train_data_HMISfrac$HMIS_frac, inv.logit(train.predict.HMIS), main = "Fitted versus observed values (public fraction)", ylim = c(0, 1), xlim = c(0, 1))
# lines(c(0, 1), c(0, 1))
# dev.off()

# qqnorm(Any_model$residuals) - thinner tails than normal but good coherence in the middle of the data which is more important?
# qqline(Any_model$residuals)
# qqnorm(train.predict.HMIS - train_data_HMISfrac$logit_HMIS) # Slightly thinner tails than normal but better than Any model.
# qqline(train.predict.HMIS - train_data_HMISfrac$logit_HMIS)

# Residuals uncorrelated with fitted values:
# cor.test(Any_model$fitted.values, Any_model$residuals)
# cor.test(train.predict.HMIS, train.predict.HMIS - train_data_HMISfrac$logit_HMIS)

# Compare training and test RMSE - amount of overfitting.

# train.error.any <- sqrt(mean(Any_model$residuals^2)) # 0.6469436. (logit scale)
# train.error.hmis <- sqrt(mean((train.predict.HMIS - train_data_HMISfrac$logit_HMIS)^2)) #0.6244664. (logit scale)
# 
# test.error.any <- sqrt(mean((test.predict.any - test_data_Any$logit_Any)^2)) #0.7111579. (logit scale)
# test.error.hmis <- sqrt(mean((test.predict.HMIS - test_data_HMISfrac$logit_HMIS)^2)) # 0.7809577. (logit scale)
# 
# train.error.any.raw <- sqrt(mean((train_data_Any$Any_treat - inv.logit(Any_model$fitted.values))^2)) #0.1176315.
# train.error.hmis.raw <- sqrt(mean((train_data_HMISfrac$HMIS_frac - inv.logit(train.predict.HMIS))^2)) #0.1250249. 
# 
# test.error.any.raw <- sqrt(mean((test_data_Any$Any_treat - inv.logit(test.predict.any))^2)) #0.1331892. 
# test.error.hmis.raw <- sqrt(mean((test_data_HMISfrac$HMIS_frac - inv.logit(test.predict.HMIS))^2)) #0.1583549. 

# Training and test error are comparable -> reasonably low amount of overfitting. Sensible for prediction. 
