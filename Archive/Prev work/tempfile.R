
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

formula_any <-  logit_Any ~ -1 + s(Year) + IHME_Region_Name + s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(ANC4_coverage_prop, k = 3) + s(VIIRS_nighttime, k = 3) + s(prop_urban, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(IFD_coverage_prop, k = 3) + s(oop_hexp_cap, k = 3) + s(measles_vacc_cov_prop_2, k = 3)
test.model <- gam(formula_any, data = train_Any_Other)

summary(test.model)

model_any <- uGamm(logit_Any ~ -1 + IHME_Region_Name + s(Year), data = train_Any_Other)

model_any$gam$formula <- formula_any

temptime <- proc.time()[3]
model.select.any <- dredge(model_any, fixed=c("IHME_Region_Name", "s(Year)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 23.85 s.
model.select.any[1:10]

# ------ To be continued...

best.any<-subset(model.select.any, delta < 2) # Best models with delta AICc less than 2 . 

formula_any_1 <-  logit_Any ~ -1 + IHME_Region_Name + s(Year) + s(ANC4_coverage_prop, k = 3) + s(oop_hexp_cap, k = 3) # Makes more sense without s(oop_hexp_cap, k = 3)?

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

formula_hmis <-  logit_HMIS ~ -1 + s(Year) +  IHME_Region_Name +  s(accessibility, k = 3) + s(hospital_beds_per1000, k = 3) + s(VIIRS_nighttime, k = 3) + s(frac_oop_hexp, k = 3) + s(oop_hexp_caps) + s(measles_vacc_cov_prop, k = 3) + s(ANC4_coverage_prop, k = 3) + s(prop_urban, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(IFD_coverage_prop, k = 3) + s(measles_vacc_cov_prop_2, k = 3)
test.model <- gam(formula_hmis, data = train_HMISfrac_Other)

summary(test.model)

model_hmis <- uGamm(logit_HMIS ~ -1 + IHME_Region_Name + s(Year), data = train_HMISfrac_Other)

model_hmis$gam$formula <- formula_hmis

temptime <- proc.time()[3]
model.select.hmis <- dredge(model_hmis, fixed=c("SMT_Factor", "s(Year)"), m.min=4)
timetaken <- proc.time()[3] - temptime  # 23.85 s.
model.select.hmis[1:10]

best.hmis<-subset(model.select.hmis, delta < 2) # Best models with delta AICc less than 2 . 

formula_hmis_1 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(IFD_coverage_prop, k = 3) + s(oop_hexp_cap, k = 3) # Makes more sense without s(oop_hexp_cap, k = 3)?
formula_hmis_2 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(measles_vacc_cov_prop, k = 3) + s(oop_hexp_cap, k = 3) # Makes more sense without s(oop_hexp_cap, k = 3)?
formula_hmis_3 <-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year) + s(DTP3_coverage_prop, k = 3) + s(oop_hexp_cap, k = 3) # Makes more sense without s(oop_hexp_cap, k = 3)?

model_hmis_1 <- gam(formula_hmis_1, data = train_HMISfrac_Other)
summary(model_hmis_1)
# plot(model_hmis_1)

pdf(paste(graphics.path, "Other_HMISfrac_smooths_1.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_hmis_1, select = 1) 
plot(model_hmis_1, select = 2) 
plot(model_hmis_1, select = 3) 
# plot(model_hmis_1, select = 6)
dev.off()

model_hmis_2 <- gam(formula_hmis_2, data = train_HMISfrac_Other)
summary(model_hmis_2)
# plot(model_hmis_2)

pdf(paste(graphics.path, "Other_HMISfrac_smooths_2.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_hmis_2, select = 1) 
plot(model_hmis_2, select = 2) 
plot(model_hmis_2, select = 3) 
# plot(model_hmis_2, select = 6)
dev.off()

model_hmis_3 <- gam(formula_hmis_3, data = train_HMISfrac_Other)
summary(model_hmis_3)
# plot(model_hmis_3)

pdf(paste(graphics.path, "Other_HMISfrac_smooths_3.pdf", sep = ""), width = 12, height = 4)
par(mfrow = c(1, 3))
plot(model_hmis_3, select = 1) 
plot(model_hmis_3, select = 2) 
plot(model_hmis_3, select = 3) 
# plot(model_hmis_3, select = 6)
dev.off()