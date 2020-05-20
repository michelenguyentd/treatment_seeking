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
formula_any_1 <-  logit_Any ~ -1 + IHME_Region_Name + SMT_Factor + s(Year, by = Time_Factor, k = 7) + s(ANC4_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3)

formula_hmis_1 <-  logit_HMIS ~ -1 + IHME_Region_Name + SMT_Factor + s(Year, by = Time_Factor) + s(frac_oop_hexp, k = 3) + s(education_all_ages_and_sexes_pc, k = 3)+ s(accessibility, k = 3) +  + s(IFD_coverage_prop , k = 3) + s(VIIRS_nighttime, k = 3) + s(measles_vacc_cov_prop, k = 3)

Any_model <-  gam(formula_any_1, data = clean_TreatSeek_Any)
HMIS_model <- gam(formula_hmis_1, data = clean_TreatSeek_HMISfrac)


summary(Any_model)
summary(HMIS_model)

# Use the full data set and chosen model configurations. 

# ------------ 1. Ignore the variability in Any_Treat, HMIS_Treat and the IHME covariates ----------- 

# Compute mean and 95% confidence intervals:

Any_pred_initial <- predict(Any_model, newdata = full_TreatSeek_n, se.fit = TRUE)
HMIS_pred_initial <- predict(HMIS_model, newdata = full_TreatSeek_n, se.fit = TRUE)

full_TreatSeek_n$Any_pred_initial <- inv.logit(Any_pred_initial$fit)
full_TreatSeek_n$Any_pred_initial_low <- inv.logit(Any_pred_initial$fit + qnorm(0.025)*Any_pred_initial$se.fit)
full_TreatSeek_n$Any_pred_initial_high <- inv.logit(Any_pred_initial$fit + qnorm(0.975)*Any_pred_initial$se.fit)

full_TreatSeek_n$HMIS_pred_initial <- full_TreatSeek_n$Any_pred_initial*inv.logit(HMIS_pred_initial$fit)
full_TreatSeek_n$HMIS_pred_initial_low <- full_TreatSeek_n$Any_pred_initial_low*inv.logit(HMIS_pred_initial$fit + qnorm(0.025)*HMIS_pred_initial$se.fit)
full_TreatSeek_n$HMIS_pred_initial_high <- full_TreatSeek_n$Any_pred_initial_high*inv.logit(HMIS_pred_initial$fit + qnorm(0.975)*HMIS_pred_initial$se.fit)

head(full_TreatSeek_n)

master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name) # This has grouped Oceania with SE Asia etc. Use full_TreatSeek if want true regions.
master.unit.list <- unique(full_TreatSeek_n$Admin_Unit_Name)

pdf(paste(graphics.path, 'Any_Treat_initial.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')
      
      plotCI(years,unit.row$Any_pred_initial,ui=unit.row$Any_pred_initial_high,li=unit.row$Any_pred_initial_low,ylim=c(0,1),add=T)
      
      in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list[k])
      
      if(length(in_out)>0){
        
        country_line<- clean_TreatSeek_Any[in_out,]
        
        plotCI(country_line$Year, country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,ylim=c(0,1),add=T, col = 2)
        
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Predicted'),pch=21,pt.bg=c('red','white'))
      }
    }
  }
}

dev.off()

pdf(paste(graphics.path, 'HMIS_Treat_initial.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')
      
      plotCI(years,unit.row$HMIS_pred_initial,ui=unit.row$HMIS_pred_initial_high,li=unit.row$HMIS_pred_initial_low,ylim=c(0,1),add=T)
      
      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[k])
      
      if(length(in_out)>0){
        
        country_line<- clean_TreatSeek_HMISfrac[in_out,]
        
        plotCI(country_line$Year, country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,ylim=c(0,1),add=T, col = 2)
        
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Predicted'),pch=21,pt.bg=c('red','white'))
      }
    }
  }
}

dev.off()

# ------------ 2. Consider the variability in Any_Treat, HMIS_Treat and the IHME covariates ----------- 

# load('TS_datasets.RData')
# N_TS <- 100
# 
# ## For each of the 100 datasets, fit the chosen models and generate 100 prediction sets.
# 
# Any_pred_list <- rep(list(NA), N_TS)
# HMIS_pred_list <- rep(list(NA), N_TS)
# 
# names(Any_pred_list) <- paste("Dataset", 1:N_TS, sep = "_")
# names(HMIS_pred_list) <- paste("Dataset", 1:N_TS, sep = "_")
# 
# ## Apply lower and upper bounds for the numeric covariates which were simulated.
# for (TS_j in 1:N_TS){
#   dataset <- TS_datasets[[TS_j]]
#   dataset$Any_treat[dataset$Any_treat>1] <- 0.999 # If Any TS > 1 due to random simulations, set upper bound of public frac to be 0.999. 
#   dataset$Any_treat[dataset$Any_treat<0.001] <- 0.001 # If Any TS <0 due to random simulations, set it to 0.001. 
#   dataset$logit_Any <- logit(dataset$Any_treat)
#   temp_HMIS_frac <- dataset$HMIS_treat/dataset$Any_treat
#   temp_HMIS_frac[temp_HMIS_frac>1] <- 0.999 # If public TS > Any TS due to random simulations, set upper bound of public frac to be 0.999. 
#   temp_HMIS_frac[temp_HMIS_frac<0.001] <- 0.001
#   dataset$logit_HMIS <- logit(temp_HMIS_frac) 
#   for (i in c(12:14, 16, 18:19, 23, 25, 28:29)){
#     temp_cov <- dataset[, i]
#     temp_cov[temp_cov<0] <- 0
#     temp_cov[temp_cov>1] <- 1
#     dataset[, i] <- temp_cov
#   }
#   dataset$hospital_beds_per1000[dataset$hospital_beds_per1000>1000] <- 1000
#   dataset$hospital_beds_per1000[dataset$hospital_beds_per1000<0] <- 0
#   dataset$LDI_pc[dataset$LDI_pc<0] <- 0
#   dataset$educ_yrs_age_std_pc_1[dataset$educ_yrs_age_std_pc_1<0] <- 0
#   dataset$educ_yrs_age_std_pc_1[dataset$educ_yrs_age_std_pc_1>99] <- 99
#   dataset$educ_yrs_age_std_pc_2[dataset$educ_yrs_age_std_pc_2<0] <- 0
#   dataset$educ_yrs_age_std_pc_2[dataset$educ_yrs_age_std_pc_2>99] <- 99
#   dataset$education_all_ages_and_sexes_pc[dataset$education_all_ages_and_sexes_pc<0] <- 0
#   dataset$education_all_ages_and_sexes_pc[dataset$education_all_ages_and_sexes_pc>99] <- 99
#   dataset$GDPpc_id_b2010[dataset$GDPpc_id_b2010<0] <- 0
#   dataset$oop_hexp_cap[dataset$oop_hexp_cap<0] <- 0
#   # Don't know about universal health coverage and haqi - not using them anyway.
#   
#   # summary(dataset$logit_Any)
#   # summary(dataset$logit_HMIS)
#   # summary(dataset$oop_hexp_cap)
#   
#   #  Normalise numeric covariates:
#   dataset_n <- dataset
#   dataset_n[, 12:34] <- apply(dataset_n[, 12:34], MARGIN = 2, FUN = function(x){(x - mean(x))/sd(x)}) 
#   
#   #  Remove name attributes and set Year as a numeric covariate:
#   dataset_n$Year <- as.numeric(dataset_n$Year)
#   dataset_n$SurveyName <- as.character(dataset_n$SurveyName)
#   # (in case) Fix for typo in previous for loop:
#   dataset_n$IHME_Region_Name[dataset_n$IHME_Region_Name == 'High-income Asia Pacific'] <- 'Southeast Asia' 
#   
#   # Remove the rows for each logit(Any_treat) and logit(HMIS_frac) are infinite, NaN or NA. 
#   #  i.e. create clean versions for modelling
#   
#   clean_Any <- dataset_n[!is.na(dataset_n$logit_Any) & is.finite(dataset_n$logit_Any), ]
#   
#   # nrow(clean_Any)
#   
#   clean_HMIS <- dataset_n[!is.na(dataset_n$logit_HMIS) & is.finite(dataset_n$logit_HMIS), ]
#   
#   # nrow(clean_HMIS)
#   # summary(clean_HMIS$logit_HMIS)
#   
#   #  Fit models on cleaned data and store 100 predictions for full data set:
#   
#   Any_fit <- gam(formula_any_1, data = clean_Any)
#   
#   HMIS_fit <- gam(formula_hmis_1, data = clean_HMIS)
#   
#   Any_pred <- matrix(NA, nrow = nrow(dataset_n), ncol = 100)
#   HMIS_pred <- matrix(NA, nrow = nrow(dataset_n), ncol = 100)
#   
#   Any_pred_distr <- predict(Any_fit, newdata = dataset_n, se.fit = TRUE)
#   HMIS_pred_distr <- predict(HMIS_fit, newdata = dataset_n, se.fit = TRUE)
#   
#   for (i in 1:100){
#     set.seed(i)
#     Any_pred[, i] <- rnorm(nrow(dataset_n), mean = Any_pred_distr$fit, sd = Any_pred_distr$se.fit)
#     HMIS_pred[, i] <- rnorm(nrow(dataset_n), mean = HMIS_pred_distr$fit, sd = HMIS_pred_distr$se.fit)
#   } 
#   Any_pred_list[[TS_j]] <- Any_pred
#   HMIS_pred_list[[TS_j]] <- HMIS_pred
# }
# 
# full_Any_pred <- do.call(cbind, Any_pred_list)
# full_HMIS_pred <- do.call(cbind, HMIS_pred_list)
# 
# full_Any_pred_raw <- inv.logit(full_Any_pred)
# full_HMIS_pred_raw <- inv.logit(full_HMIS_pred)
# 
# # Compute mean and 95% confidence intervals:
# 
# full_TreatSeek$Any_pred <- rowMeans(full_Any_pred_raw)
# full_TreatSeek$Any_pred_low <- apply(full_Any_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
# full_TreatSeek$Any_pred_high <- apply(full_Any_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.975)})
# 
# full_TreatSeek$HMIS_pred <- rowMeans(full_HMIS_pred_raw)*full_TreatSeek$Any_pred
# full_TreatSeek$HMIS_pred_low <- apply(full_HMIS_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.025)})*full_TreatSeek$Any_pred_low
# full_TreatSeek$HMIS_pred_high <- apply(full_HMIS_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.975)})*full_TreatSeek$Any_pred_high
# 
# head(full_TreatSeek)
# 
# master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name) # This has grouped Oceania with SE Asia etc. Use full_TreatSeek if want true regions.
# master.unit.list <- unique(full_TreatSeek$Admin_Unit_Name)
# 
# pdf(paste(graphics.path, 'Any_Treat.pdf'),width=8.7,height = 11.2)
# 
# par(mfrow=c(3,2))
# 
# for (j in 1:length(master.region.list)){
#   unit.list <- unique(full_TreatSeek$Admin_Unit_Name[full_TreatSeek_n$IHME_Region_Name == master.region.list[j]])
#   for (i in 1:length(unit.list)){
#     unit.row <- full_TreatSeek[full_TreatSeek$Admin_Unit_Name == unit.list[i], ]
#     
#     plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')
#     
#     plotCI(years,unit.row$Any_pred,ui=unit.row$Any_pred_high,li=unit.row$Any_pred_low,ylim=c(0,1),add=T)
#     
#     in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list[i])
#     
#     if(length(in_out)>0){
#       
#       country_line<- clean_TreatSeek_Any[in_out,]
#       
#       plotCI(country_line$Year, country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,ylim=c(0,1),add=T, col = 2)
#       
#     }
#     if (unit.list[i] == 'Afghanistan'){
#       legend('topright',legend=c('DHS','Predicted'),pch=21,pt.bg=c('red','white'))
#     }
#   }
#   
# }
# 
# dev.off()
# 
# pdf(paste(graphics.path, 'HMIS_Treat.pdf'),width=8.7,height = 11.2)
# 
# par(mfrow=c(3,2))
# 
# for (j in 1:length(master.region.list)){
#   unit.list <- unique(full_TreatSeek$Admin_Unit_Name[full_TreatSeek_n$IHME_Region_Name == master.region.list[j]])
#   
#   for (i in 1:length(unit.list)){
#     unit.row <- full_TreatSeek[full_TreatSeek$Admin_Unit_Name == unit.list[i], ]
#     
#     plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]), main=paste(master.region.list[j], ': ', unit.list[i], sep = ''),ylab='% U5 fevers sought treatment at public facilities',xlab='Year')
#     
#     plotCI(years,unit.row$HMIS_pred,ui=unit.row$HMIS_pred_high,li=unit.row$HMIS_pred_low,ylim=c(0,1),add=T)
#     
#     in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[i])
#     
#     if(length(in_out)>0){
#       
#       country_line<- clean_TreatSeek_HMISfrac[in_out,]
#       
#       plotCI(country_line$Year, country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,ylim=c(0,1),add=T, col = 2)
#       
#     }
#     if (unit.list[i] == 'Afghanistan'){
#       legend('topright',legend=c('DHS','Predicted'),pch=21,pt.bg=c('red','white'))
#     }
#   }
# }
# 
# dev.off()
