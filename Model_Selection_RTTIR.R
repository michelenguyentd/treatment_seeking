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

# Models to be tested:

model_no <- "RTTIR_" # 1. RTTIR: IHME region temporal trend and factors; 2. PTTIR: Pruned regions temporal trend and IHME regional factors; 3. PTTPF: Pruned regions temporal trend and factors; 4. OC: Other considerations.

# # Check GUF using SUR IHME covariates:
# 
# head(full_TreatSeek_n[full_TreatSeek_n$ISO3 == "GUF", c("ANC1_coverage_prop", "accessibility")])
# head(full_TreatSeek_n[full_TreatSeek_n$ISO3 == "SUR", c("ANC1_coverage_prop", "accessibility")])
# 
# # Check MYT using COM IHME covariates:
# 
# head(full_TreatSeek_n[full_TreatSeek_n$ISO3 == "MYT", c("ANC1_coverage_prop", "accessibility")])
# head(full_TreatSeek_n[full_TreatSeek_n$ISO3 == "COM", c("ANC1_coverage_prop", "accessibility")])
# 
# # Check that Liberia 1986 and Pakistan 1991 is included in full_TreatSeek_n:
# 
# full_TreatSeek_n[full_TreatSeek_n$ISO3 == "LBR" & full_TreatSeek_n$Year == 1986, ]
# full_TreatSeek_n[full_TreatSeek_n$ISO3 == "PAK" & full_TreatSeek_n$Year == 1991, ]


########## ---------------------- 1. CHOOSE SUBSETS OF COVARIATES TO CONSIDER IN MODEL WHICH HAVE LOW COLLINEARITY -------------------- ############

## Correlation between covariates:

# Get columns with covariates. 
cov_col <- clean_TreatSeek_HMISfrac[, c("ANC1_coverage_prop", "ANC4_coverage_prop", "DTP3_coverage_prop", 
                                'hospital_beds_per1000', "IFD_coverage_prop", "LDI_pc", "measles_vacc_cov_prop", "SBA_coverage_prop", 
                                "GDPpc_id_b2010", "prop_urban", "oop_hexp_cap", "frac_oop_hexp", "measles_vacc_cov_prop_2", "ind_health", 
                                "education_all_ages_and_sexes_pc", "log_the_pc", "DMSP_nighttime", "accessibility", "VIIRS_nighttime")]
# Fit univariate GAMs on the covariates and compare AIC, then select covariate to include in final model depending on their relative AIC and correlation with each other:

cov_ind <- c(24:42) # check covariate names.
AIC_Any <- rep(NA, length(cov_ind))
names(AIC_Any) <- names(clean_TreatSeek_Any)[cov_ind]

AIC_HMIS <- rep(NA, length(cov_ind))
names(AIC_HMIS) <- names(clean_TreatSeek_HMISfrac)[cov_ind]

for (i in 1:length(cov_ind)){
  var_values_Any <- train_data_Any[, cov_ind[i]] 
  gam_Any <- gam(logit_Any ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name) + s(X), data = data.frame("logit_Any" = train_data_Any$logit_Any, "X" = var_values_Any, 'Year' = train_data_Any$Year, 'IHME_Region_Name' = train_data_Any$IHME_Region_Name))
  var_values_HMIS <- train_data_HMISfrac[, cov_ind[i]]
  gam_HMIS <- gam(logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name) + s(X), data =  data.frame("logit_HMIS" = train_data_HMISfrac$logit_HMIS, "X" = var_values_HMIS, 'Year' = train_data_HMISfrac$Year, 'IHME_Region_Name' = train_data_HMISfrac$IHME_Region_Name))
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

# Check for regional trends to omit...

#  Use covariates identified in test_cov_Any:

formula_any <-  logit_Any ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name, k = 5) + s(ANC1_coverage_prop, k = 3) + s(GDPpc_id_b2010, k = 3) + s(DMSP_nighttime, k = 3) + s(hospital_beds_per1000, k = 3) 

test.model <- gam(formula_any, data = train_data_Any)

summary(test.model)

# plot(test.model, ylim = c(-5, 5)) # Check for strange relations.

model_any_1 <- test.model

# model_any <- uGamm(logit_Any ~ -1 + IHME_Region_Name + s(Year) , data = train_data_Any)
# 
# model_any$gam$formula <- formula_any
# 
# temptime <- proc.time()[3]
# model.select.any <- dredge(model_any, fixed=c("IHME_Region_Name", "s(Year, by = IHME_Region_Name, k = 5)"), m.min = 4)
# timetaken <- proc.time()[3] - temptime  # About 3.57 min.
# model.select.any[1:10]

formula_HMIS <- logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name, k = 5) + s(ANC1_coverage_prop, k = 3)  + s(VIIRS_nighttime, k =3) + s(frac_oop_hexp, k = 3) + s(measles_vacc_cov_prop, k = 3)  + s(prop_urban, k = 3) + s(log_the_pc, k = 3) + s(hospital_beds_per1000, k = 3) 

test.model <- gam(formula_HMIS, data = train_data_HMISfrac)

summary(test.model)

model_hmis_1 <- test.model

# model_HMIS <- uGamm(logit_HMIS ~-1 + IHME_Region_Name + s(Year), data = train_data_HMISfrac)
# 
# model_HMIS$gam$formula <- formula_HMIS
# 
# temptime2 <- proc.time()[3]
# model.select.HMIS <- dredge(model_HMIS, fixed=c("IHME_Region_Name", 's(Year, by = IHME_Region_Name, k = 5)'), m.lim = c(4, 9))
# timetaken2 <- proc.time()[3] - temptime2
# model.select.HMIS[1:10]
# # About 3.43 min.

# best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 
# best.HMIS<-subset(model.select.HMIS, delta < 2) #takes the best model with delta AICc less than 2 - only 1. 
# print(xtable(best.any, type = "latex"), file = paste(table.path, model_no, "best.any.tex"))
# print(xtable(best.HMIS, type = "latex"), file = paste(table.path, model_no, "best.HMIS.tex"))

########## ---------------------- 3. EXAMINE THE FINAL MODELS -------------------- ############

# Examine components of chosen models:

# formula_any_1 <- logit_Any ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name, k = 5) + s(ANC1_coverage_prop, k = 3) + s(GDPpc_id_b2010, k = 3) + s(DMSP_nighttime, k = 3)
# model_any_1 <- gam(formula_any_1, data = train_data_Any)
# summary(model_any_1) # RTTIR: 50.8%

# To be changed for different models:

gamtabs(model_any_1, caption = "Summary of first any TS model with IHME region temporal trends and factors")

pdf(paste(graphics.path, model_no, "Any_model_smooths_1.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_any_1, select = 1, ylim = c(-5, 5), xlim = c(1990, 2019)) 
plot(model_any_1, select = 2, ylim = c(-5, 5), xlim = c(1990, 2019))   
plot(model_any_1, select = 3, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 4, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 5, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 6, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 7, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 8, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 9, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 10, ylim = c(-5, 5), xlim = c(1990, 2019))   
plot(model_any_1, select = 11, ylim = c(-5, 5), xlim = c(1990, 2019))   
plot(model_any_1, select = 12, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_any_1, select = 13, ylim = c(-5, 5))  
plot(model_any_1, select = 14, ylim = c(-5, 5)) 
plot(model_any_1, select = 15, ylim = c(-5, 5)) 
plot(model_any_1, select = 16, ylim = c(-5, 5)) 
plot(model_any_1, select = 17, ylim = c(-5, 5)) 
plot(model_any_1, select = 18, ylim = c(-5, 5)) 
plot(model_any_1, select = 19, ylim = c(-5, 5)) 
plot(model_any_1, select = 20, ylim = c(-5, 5)) 
dev.off()

# formula_hmis_1<-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name, k = 5)  + s(ANC1_coverage_prop, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(frac_oop_hexp, k = 3) + s(hospital_beds_per1000, k = 3) + s(VIIRS_nighttime, k =3)
# 
# model_hmis_1 <- gam(formula_hmis_1, data = train_data_HMISfrac)
# summary(model_hmis_1) # 52.9%.

gamtabs(model_hmis_1, caption = "Summary of first public fraction model with IHME region temporal trends and factors")

pdf(paste(graphics.path, model_no, "HMIS_model1_smooths.pdf", sep = ""), width = 12, height = 8)
par(mfrow = c(2, 3))
plot(model_hmis_1, select = 1, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 2, ylim = c(-5, 5), xlim = c(1990, 2019))   
plot(model_hmis_1, select = 3, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 4, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 5, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 6, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 7, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 8, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 9, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 10, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 11, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 12, ylim = c(-5, 5), xlim = c(1990, 2019))  
plot(model_hmis_1, select = 13, ylim = c(-5, 5))  
plot(model_hmis_1, select = 14, ylim = c(-5, 5)) 
plot(model_hmis_1, select = 15, ylim = c(-5, 5)) 
plot(model_hmis_1, select = 16, ylim = c(-5, 5))  
plot(model_hmis_1, select = 17, ylim = c(-5, 5)) 
plot(model_hmis_1, select = 18, ylim = c(-5, 5))  
plot(model_hmis_1, select = 19, ylim = c(-5, 5))  
plot(model_hmis_1, select = 20, ylim = c(-5, 5))  
dev.off()


# # Use best models:
# Any_model <-  gam(formula_any_1, data = train_data_Any)
# HMIS_model <- gam(formula_hmis_1, data = train_data_HMISfrac)
# 
# train.predict.any <- predict(Any_model)
# train.predict.HMIS <- predict(HMIS_model)
# 
# # train.predict.HMIS <- best.HMIS$weight[1]*predict(model_hmis_1) + best.HMIS$weight[2]*predict(model_hmis_2) 
# # Use averaged predictions instead of predictions from averaged coefficients - makes more sense in the light of non-linearity.
# # Weight = Akaike weight.
# 
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
