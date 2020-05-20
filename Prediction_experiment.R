# In this script, we test the following changes:
# 1. Predict Eastern SSA countries with no data with other trend instead of Western SSA trend - check difference.
# 2. Select Eastern SSA trend for units with no data based on covariates.
# 3. Find nearest neighbour within region for units with no data based on significant covariates to select random effect -> This doesn't work well for Shanghai (with low ANC1_coverage_prop).

# ---------------------------------------------------------------------------------------------------------------------- #

library(mgcv) # For GAM.
library(splitstackshape) # For stratified sampling on IHME regions.
library(MuMIn) # for AIC (model selection)
library(gtools) # for logit transform
library(VGAM) # for probit transform
library(survey) # for taking into account sampling weights
library(nortest) # Normality tests for residuals
library(xtable) # For exporting latex tables.
library(plotrix) # For plotCI.
library(ggplot2)
library(grid)
library(gridExtra)
library(class) # For knn.
library(TMB) # For time series modelling later.
library(sparseMVN)
library(Matrix)

rm(list = ls())
setwd('J:/Treatment_Seeking/')
# setwd('C:/Users/jimzl/Downloads/Michele/Treatment Seeking')

graphics.path <- 'J:/Treatment_Seeking/graphics/'
data.path <- 'J:/Treatment_Seeking/Data/'
source("J:/Treatment_Seeking/Code/multiplot.R")

# graphics.path <- 'C:/Users/jimzl/Downloads/Michele/Treatment Seeking/graphics/'
# data.path <- 'C:/Users/jimzl/Downloads/Michele/Treatment Seeking/Data/'
# 
# source("C:/Users/jimzl/Downloads/Michele/Treatment Seeking/Code/multiplot.R")

# Years to model:
years <- 1980:2019

# IHME populations:
#ihme_pop <- read.csv(paste(data.path, "ihme_populations.csv", sep = ""))
ihme_pop <- read.csv("Z:\\GBD2019\\Processing\\Stages\\03b_Population_Figures_Export\\Checkpoint_Outputs\\ihme_populations.csv") # This now includes GUF and MYT, and gets updated with API dump.

# Read in IHME covariates:
# load(paste(data.path, "IHME_covar_decomp2.RData", sep = ""))
load('Z:/GBD2019/Processing/Stages/01a_IHME_covariates/IHME_Subnational_Covariates_GBD2019/covariates_for_treatment_seeking/IHME_covar_decomp2.RData')
str(covar_list)

# Remove the non-aggregated education_yr_pc covariate:

cov_names <- rep(NA, length(covar_list))

for (i in 1:length(cov_names)){
  cov_names[i] <- unique(covar_list[[i]]$covariate_name_short)
}

names(covar_list) <- cov_names

covar_list <- covar_list[-which(cov_names %in% c("education_yrs_pc", "haqi", "universal_health_coverage", "educ_yrs_age_std_pc"))]
# Don't use because of interpretation difficulty. Keep frac_oop_hexp for now just in case.

cov_names <- rep(NA, length(covar_list))

for (i in 1:length(cov_names)){
  cov_names[i] <- unique(as.character(covar_list[[i]]$covariate_name_short))
}

names(covar_list) <- cov_names


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

# Use location id as factor instead of admin unit name so as to account for 2 Punjabs and 2 Mexicos:
train_data_Any$IHME_location_id <- as.factor(train_data_Any$IHME_location_id)
train_data_HMISfrac$IHME_location_id <- as.factor(train_data_HMISfrac$IHME_location_id)
test_data_Any$IHME_location_id <- as.factor(test_data_Any$IHME_location_id)
test_data_HMISfrac$IHME_location_id <- as.factor(test_data_HMISfrac$IHME_location_id)

clean_TreatSeek_Any$IHME_location_id <- as.factor(clean_TreatSeek_Any$IHME_location_id)
clean_TreatSeek_HMISfrac$IHME_location_id <- as.factor(clean_TreatSeek_HMISfrac$IHME_location_id)
full_TreatSeek_n$IHME_location_id <- as.factor(full_TreatSeek_n$IHME_location_id)
full_TreatSeek$IHME_location_id <- as.factor(full_TreatSeek$IHME_location_id)

# Remove Brazil 2016 data point from training set - done beforehand:
# train_data_Any <- train_data_Any[-which(train_data_Any$ISO3 == "BRA" & train_data_Any$Year == 1996), ]
# train_data_HMISfrac <- train_data_HMISfrac[-which(train_data_HMISfrac$ISO3 == "BRA" & train_data_HMISfrac$Year == 1996), ]

# Find units without data:

master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name) # This has grouped Oceania with SE Asia etc. Use full_TreatSeek if want true regions.
master.unit.list <- unique(full_TreatSeek_n$IHME_location_id)
# Use superregion for matching units without data with those with data based on covariates:
full_TreatSeek_n$IHME_Super_Region_Name[full_TreatSeek_n$Admin_Unit_Name %in% c("Monaco", "San Marino", "South Korea")] <- "Southeast Asia, East Asia, and Oceania"
full_TreatSeek_n$IHME_Super_Region_Name[full_TreatSeek_n$Admin_Unit_Name %in% c("Argentina", "French Guiana")] <- "Latin America and Caribbean"
full_TreatSeek_n$IHME_Super_Region_Name[full_TreatSeek_n$Admin_Unit_Name %in% c("Mayotte")] <- "Sub-Saharan Africa"
master.superregion.list <- unique(full_TreatSeek_n$IHME_Super_Region_Name)
master.superregion.list <-droplevels(master.superregion.list)

units_no_Any_data <- c(); units_with_Any_data <- c(); units_1_Any_data <- c()
units_no_HMISfrac_data <- c(); units_with_HMISfrac_data <- c(); units_1_HMISfrac_data <- c()
master.unit.list <- as.character(master.unit.list)

for (i in 1:length(master.unit.list)){
  Any_pts <- sum(train_data_Any$IHME_location_id == as.character(master.unit.list[i])) + sum(test_data_Any$IHME_location_id == as.character(master.unit.list[i]))
  HMISfrac_pts <- sum(train_data_HMISfrac$IHME_location_id == as.character(master.unit.list[i])) + sum(test_data_HMISfrac$IHME_location_id == as.character(master.unit.list[i]))
  if (Any_pts == 0){units_no_Any_data <- c(units_no_Any_data, master.unit.list[i])}
  if (Any_pts == 1){units_1_Any_data <- c(units_1_Any_data, master.unit.list[i])}
  if (Any_pts > 0){units_with_Any_data <- c(units_with_Any_data, master.unit.list[i])}
  if (HMISfrac_pts == 0){units_no_HMISfrac_data <- c(units_no_HMISfrac_data, master.unit.list[i])}
  if (HMISfrac_pts == 1){units_1_HMISfrac_data <- c(units_1_HMISfrac_data, master.unit.list[i])}
  if (HMISfrac_pts > 0){units_with_HMISfrac_data <- c(units_with_HMISfrac_data, master.unit.list[i])}
}



# For PTTIR onwards (regional temporal trends):
clean_TreatSeek_Any$Time_Factor <- as.character(clean_TreatSeek_Any$IHME_Region_Name)
clean_TreatSeek_Any$Time_Factor[!(clean_TreatSeek_Any$IHME_Region_Name %in% c("Central Asia", "North Africa and Middle East", 'South Asia', 'Southeast Asia', 'Western Sub-Saharan Africa'))] <- 'Other'
clean_TreatSeek_Any$Time_Factor[!(clean_TreatSeek_Any$ISO3 %in% c('ETH', 'MYT', 'COM', 'MDG')) & clean_TreatSeek_Any$IHME_Region_Name == 'Eastern Sub-Saharan Africa'] <- 'Western Sub-Saharan Africa'
clean_TreatSeek_Any$Time_Factor <- as.factor(clean_TreatSeek_Any$Time_Factor)
unique(clean_TreatSeek_Any$Time_Factor)
clean_TreatSeek_Any$Time_Factor <- relevel(clean_TreatSeek_Any$Time_Factor, ref = 'Other')

full_TreatSeek_n$Time_Factor <- as.character(full_TreatSeek_n$IHME_Region_Name)
full_TreatSeek_n$Time_Factor[!(full_TreatSeek_n$IHME_Region_Name %in% c("Central Asia", "North Africa and Middle East", 'South Asia', 'Southeast Asia', 'Western Sub-Saharan Africa'))] <- 'Other'
full_TreatSeek_n$Time_Factor[!(full_TreatSeek_n$ISO3 %in% c('ETH', 'MYT', 'COM', 'MDG')) & full_TreatSeek_n$IHME_Region_Name == 'Eastern Sub-Saharan Africa'] <- 'Western Sub-Saharan Africa'
full_TreatSeek_n$Time_Factor <- as.factor(full_TreatSeek_n$Time_Factor)
unique(full_TreatSeek_n$Time_Factor)
full_TreatSeek_n$Time_Factor <- relevel(full_TreatSeek_n$Time_Factor, ref = 'Other')

clean_TreatSeek_HMISfrac$Time_Factor_2 <- as.character(clean_TreatSeek_HMISfrac$IHME_Region_Name)
clean_TreatSeek_HMISfrac$Time_Factor_2[!(clean_TreatSeek_HMISfrac$IHME_Region_Name %in% c("Central Latin America", 'Southeast Asia', "Western Sub-Saharan Africa",  "Eastern Sub-Saharan Africa"))] <- 'Other'
clean_TreatSeek_HMISfrac$Time_Factor_2 <- as.factor(clean_TreatSeek_HMISfrac$Time_Factor_2)
unique(clean_TreatSeek_HMISfrac$Time_Factor_2)
clean_TreatSeek_HMISfrac$Time_Factor_2 <- relevel(clean_TreatSeek_HMISfrac$Time_Factor_2, ref = 'Other')

full_TreatSeek_n$Time_Factor_2 <- as.character(full_TreatSeek_n$IHME_Region_Name)
full_TreatSeek_n$Time_Factor_2[!(full_TreatSeek_n$IHME_Region_Name %in% c("Central Latin America", 'Southeast Asia', "Western Sub-Saharan Africa",  "Eastern Sub-Saharan Africa"))] <- 'Other'
full_TreatSeek_n$Time_Factor_2 <- as.factor(full_TreatSeek_n$Time_Factor_2)
unique(full_TreatSeek_n$Time_Factor_2)
full_TreatSeek_n$Time_Factor_2 <- relevel(full_TreatSeek_n$Time_Factor_2, ref = 'Other')

# For PTTFR onwards (coarser regional factors):

clean_TreatSeek_Any$Reg_Factor <- as.character(clean_TreatSeek_Any$IHME_Region_Name)
clean_TreatSeek_Any$Reg_Factor[!(clean_TreatSeek_Any$IHME_Region_Name %in% c('Southeast Asia', 'South Asia', "Central Asia", 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa'))] <- 'Other'
clean_TreatSeek_Any$Reg_Factor <- as.factor(clean_TreatSeek_Any$Reg_Factor)
unique(clean_TreatSeek_Any$Reg_Factor)
clean_TreatSeek_Any$Reg_Factor <- relevel(clean_TreatSeek_Any$Reg_Factor, ref = 'Other')

full_TreatSeek_n$Reg_Factor <- as.character(full_TreatSeek_n$IHME_Region_Name)
full_TreatSeek_n$Reg_Factor[!(full_TreatSeek_n$IHME_Region_Name %in% c('Southeast Asia','South Asia', "Central Asia", 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa'))] <- 'Other'
full_TreatSeek_n$Reg_Factor <- as.factor(full_TreatSeek_n$Reg_Factor)
unique(full_TreatSeek_n$Reg_Factor)
full_TreatSeek_n$Reg_Factor <- relevel(full_TreatSeek_n$Reg_Factor, ref = 'Other')

clean_TreatSeek_HMISfrac$Reg_Factor_2 <- as.character(clean_TreatSeek_HMISfrac$IHME_Region_Name)
clean_TreatSeek_HMISfrac$Reg_Factor_2[!(clean_TreatSeek_HMISfrac$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Southeast Asia"))] <- 'Other'
clean_TreatSeek_HMISfrac$Reg_Factor_2 <- as.factor(clean_TreatSeek_HMISfrac$Reg_Factor_2)
unique(clean_TreatSeek_HMISfrac$Reg_Factor_2)
clean_TreatSeek_HMISfrac$Reg_Factor_2 <- relevel(clean_TreatSeek_HMISfrac$Reg_Factor_2, ref = 'Other')

full_TreatSeek_n$Reg_Factor_2 <- as.character(full_TreatSeek_n$IHME_Region_Name)
full_TreatSeek_n$Reg_Factor_2[!(full_TreatSeek_n$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Southeast Asia"))] <- 'Other'
full_TreatSeek_n$Reg_Factor_2 <- as.factor(full_TreatSeek_n$Reg_Factor_2)
unique(full_TreatSeek_n$Reg_Factor_2)
full_TreatSeek_n$Reg_Factor_2 <- relevel(full_TreatSeek_n$Reg_Factor_2, ref = 'Other')

formula_any_1 <-  logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(GDPpc_id_b2010, k = 3) 

formula_hmis_1 <-  logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2, k = 5) + s(ANC1_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(frac_oop_hexp, k = 3) + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(prop_urban, k = 3) + s(VIIRS_nighttime, k = 3)

# Insert dummy true Year for plotting:
full_TreatSeek_n$t.Year <- full_TreatSeek_n$Year
# Freeze non-linear temporal trend pre 1995:
full_TreatSeek_n$Year[full_TreatSeek_n$Year < 1995] <- 1995
unique(full_TreatSeek_n$Year)

# ------------ 1a. Explore the similarity of covariates for units without data ----------

# Function to check proportion of variance explained by significant covariates:

prop_var_plot <- function(prin_comp = region.pca1, plot.title = "Any TS Regional Trend Covariates"){
  std_dev <- prin_comp$sdev
  #compute variance
  pr_var <- std_dev^2
  #proportion of variance explained
  prop_varex <- pr_var/sum(pr_var)
  #scree plot
  plot(prop_varex, xlab = "Principal Component",
         ylab = "Proportion of Variance Explained",
         type = "b")
  title(plot.title)
}

full_TreatSeek_n$rf.unit <- full_TreatSeek_n$IHME_location_id
full_TreatSeek_n$rf.unit.pf <- full_TreatSeek_n$IHME_location_id
full_TreatSeek_n$trend.unit <- full_TreatSeek_n$IHME_location_id
full_TreatSeek_n$trend.unit.pf <- full_TreatSeek_n$IHME_location_id

ref_year_1 <- 2000; ref_year_2 <- 2010; ref_year_3 <- 2019
ETimor_id <- as.character(unique(full_TreatSeek_n$IHME_location_id[full_TreatSeek_n$Admin_Unit_Name == "East Timor"]))

for (j in 1:length(master.superregion.list)){
  
  # Use ADMIN0 with data for matching with ADMIN0 and ADMIN1 without data:
  temp_region_cov <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == as.character(master.superregion.list[j]), ]
  
  # a) Trend: Use changes between dynamic covariates for years 2000-2010 and 2010-2019.
  
  t_Any_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% c(units_no_Any_data, units_1_Any_data)), ]
  t_Any_region_cov_1 <- t_Any_region_cov[t_Any_region_cov$t.Year == ref_year_1, ]
  t_Any_region_cov_2 <- t_Any_region_cov[t_Any_region_cov$t.Year == ref_year_2, ]
  t_Any_region_cov_3 <- t_Any_region_cov[t_Any_region_cov$t.Year == ref_year_3, ]
  t_Any_region_no_data <- unique(t_Any_region_cov$IHME_location_id[t_Any_region_cov$IHME_location_id %in% c(units_no_Any_data, units_1_Any_data)])
  
  t_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)), ]
  t_HMISfrac_region_cov_1 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$t.Year == ref_year_1, ]
  t_HMISfrac_region_cov_2 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$t.Year == ref_year_2, ]
  t_HMISfrac_region_cov_3 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$t.Year == ref_year_3, ]
  t_HMISfrac_region_no_data <- unique(t_HMISfrac_region_cov$IHME_location_id[t_HMISfrac_region_cov$IHME_location_id %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)])
  
  # Omit "DMSP_nighttime" because static.
  temp_df1 <- cbind(t_Any_region_cov_2[, c("ANC1_coverage_prop", "GDPpc_id_b2010")] - t_Any_region_cov_1[, c("ANC1_coverage_prop",  "GDPpc_id_b2010")], t_Any_region_cov_3[, c("ANC1_coverage_prop",  "GDPpc_id_b2010")] - t_Any_region_cov_2[, c("ANC1_coverage_prop",   "GDPpc_id_b2010")])
  region.pca1 <- prcomp(temp_df1, center = TRUE, scale. = TRUE)
  
  query_df1 <- region.pca1$x[t_Any_region_cov_1$IHME_location_id %in% c(units_no_Any_data, units_1_Any_data), 1:2]
  ref_df1 <- region.pca1$x[!(t_Any_region_cov_1$IHME_location_id %in% c(units_no_Any_data, units_1_Any_data)), 1:2]
  
  # Omit "VIIRS_nighttime" because static.
  temp_df2 <- cbind(t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")] - t_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], t_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")] - t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")])
  region.pca2 <- prcomp(temp_df2, center = TRUE, scale. = TRUE)
  
  query_df2 <- region.pca2$x[t_HMISfrac_region_cov_1$IHME_location_id %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data), 1:2]
  ref_df2 <- region.pca2$x[!(t_HMISfrac_region_cov_1$IHME_location_id %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)), 1:2]
  
  # b) Random effect: Use significant covariates for years 2000, 2010 and 2019.
  
  rf_Any_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% units_no_Any_data), ]
  # if(j == 6){rf_Any_region_cov <- rf_Any_region_cov[rf_Any_region_cov$IHME_location_id != ETimor_id, ]} # Remove East Timor as possible match.

  rf_Any_region_cov_1 <- rf_Any_region_cov[rf_Any_region_cov$t.Year == ref_year_1, ]
  rf_Any_region_cov_2 <- rf_Any_region_cov[rf_Any_region_cov$t.Year == ref_year_2, ]
  rf_Any_region_cov_3 <- rf_Any_region_cov[rf_Any_region_cov$t.Year == ref_year_3, ]
  rf_Any_region_no_data <- unique(rf_Any_region_cov$IHME_location_id[rf_Any_region_cov$IHME_location_id %in% units_no_Any_data])
  
  rf_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% units_no_HMISfrac_data), ]
  # if(j == 6){rf_HMISfrac_region_cov <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$IHME_location_id != ETimor_id, ]} # Remove East Timor as possible match.
  
  rf_HMISfrac_region_cov_1 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$t.Year == ref_year_1, ]
  rf_HMISfrac_region_cov_2 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$t.Year == ref_year_2, ]
  rf_HMISfrac_region_cov_3 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$t.Year == ref_year_3, ]
  rf_HMISfrac_region_no_data <- unique(rf_HMISfrac_region_cov$IHME_location_id[rf_HMISfrac_region_cov$IHME_location_id %in% units_no_HMISfrac_data])
  
  temp_df3 <- cbind(rf_Any_region_cov_1[, c("ANC1_coverage_prop", "DMSP_nighttime", "GDPpc_id_b2010")], rf_Any_region_cov_2[, c("ANC1_coverage_prop", "DMSP_nighttime", "GDPpc_id_b2010")], rf_Any_region_cov_3[, c("ANC1_coverage_prop", "DMSP_nighttime", "GDPpc_id_b2010")])
  region.pca3 <- prcomp(temp_df3, center = TRUE, scale. = TRUE)
  
  query_df3 <- region.pca3$x[rf_Any_region_cov_1$IHME_location_id %in% units_no_Any_data, 1:2]
  ref_df3 <- region.pca3$x[!(rf_Any_region_cov_1$IHME_location_id %in% units_no_Any_data), 1:2]
  
  temp_df4 <- cbind(rf_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], rf_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], rf_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")])
  region.pca4 <- prcomp(temp_df4, center = TRUE, scale. = TRUE)
  
  query_df4 <- region.pca4$x[rf_HMISfrac_region_cov_1$IHME_location_id %in% units_no_HMISfrac_data, 1:4]
  ref_df4 <- region.pca4$x[!(rf_HMISfrac_region_cov_1$IHME_location_id %in% units_no_HMISfrac_data), 1:4]
  
  ref_class_1 <- t_Any_region_cov_1[!(t_Any_region_cov_1$IHME_location_id %in% t_Any_region_no_data), c("IHME_location_id")]
  ref_class_2 <- t_HMISfrac_region_cov_1[!(t_HMISfrac_region_cov_1$IHME_location_id %in% t_HMISfrac_region_no_data), c("IHME_location_id")]
  ref_class_3 <- rf_Any_region_cov_1[!(rf_Any_region_cov_1$IHME_location_id %in% rf_Any_region_no_data), c("IHME_location_id")]
  ref_class_4 <- rf_HMISfrac_region_cov_1[!(rf_HMISfrac_region_cov_1$IHME_location_id %in% rf_HMISfrac_region_no_data), c("IHME_location_id")]
  
  neighbour_result_t_any <- knn1(test = query_df1, train = ref_df1, cl = ref_class_1)
  neighbour_result_t_pf <- knn1(test = query_df2, train = ref_df2, cl = ref_class_2)
  neighbour_result_rf_any <- knn1(test = query_df3, train = ref_df3, cl = ref_class_3)
  neighbour_result_rf_pf <- knn1(test = query_df4, train = ref_df4, cl = ref_class_4)
  
  for (i in 1:length(t_Any_region_no_data)){
    full_TreatSeek_n$trend.unit[full_TreatSeek_n$IHME_location_id == t_Any_region_no_data[i]] <- neighbour_result_t_any[i]
  }
  for (i in 1:length(t_HMISfrac_region_no_data)){
    full_TreatSeek_n$trend.unit.pf[full_TreatSeek_n$IHME_location_id == t_HMISfrac_region_no_data[i]] <- neighbour_result_t_pf[i]
  }
  for (i in 1:length(rf_Any_region_no_data)){
    full_TreatSeek_n$rf.unit[full_TreatSeek_n$IHME_location_id == rf_Any_region_no_data[i]] <- neighbour_result_rf_any[i]
  }
  for (i in 1:length(rf_HMISfrac_region_no_data)){
    full_TreatSeek_n$rf.unit.pf[full_TreatSeek_n$IHME_location_id == rf_HMISfrac_region_no_data[i]] <- neighbour_result_rf_pf[i]
  }
  
  pdf(paste(graphics.path, master.superregion.list[j], '.trend.rf.covariates.PCA_NN.pdf', sep = ""),width=12, height = 12)

  par(mfrow=c(2,2))

  plot(region.pca1$x[, 1:2], col = "white", main = "(a) Trend: Any Treatment Seeking", xlim = c(min(region.pca1$x[, 1])*1.5, max(region.pca1$x[, 1])*1.5), ylim = c(c(min(region.pca1$x[, 2])*1.5, max(region.pca1$x[, 2])*1.5)))
  text(region.pca1$x[, 1:2], labels = t_Any_region_cov_1$Admin_Unit_Name, col = ifelse(t_Any_region_cov_1$IHME_location_id %in% t_Any_region_no_data, "blue", "black"), cex = 0.7)

  plot(region.pca2$x[, 1:2], col = "white", main = "(b) Trend: Public Fractions", xlim = c(min(region.pca2$x[, 1])*1.5, max(region.pca2$x[, 1])*1.5), ylim = c(c(min(region.pca2$x[, 2])*1.5, max(region.pca2$x[, 2])*1.5)))
  text(region.pca2$x[, 1:2], labels = t_HMISfrac_region_cov_1$Admin_Unit_Name, col = ifelse(t_HMISfrac_region_cov_1$IHME_location_id %in% t_HMISfrac_region_no_data, "blue", "black"), cex = 0.7)

  plot(region.pca3$x[, 1:2], col = "white", main = "(c) Random Effect: Any Treatment Seeking", xlim = c(min(region.pca3$x[, 1])*1.5, max(region.pca3$x[, 1])*1.5), ylim = c(c(min(region.pca3$x[, 2])*1.5, max(region.pca3$x[, 2])*1.5)))
  text(region.pca3$x[, 1:2], labels = rf_Any_region_cov_1$Admin_Unit_Name, col = ifelse(rf_Any_region_cov_1$IHME_location_id %in% rf_Any_region_no_data, "blue", "black"), cex = 0.7)

  plot(region.pca4$x[, 1:2], col = "white", main = "(d) Random Effect: Public Fractions", xlim = c(min(region.pca4$x[, 1])*1.5, max(region.pca4$x[, 1])*1.5), ylim = c(c(min(region.pca4$x[, 2])*1.5, max(region.pca4$x[, 2])*1.5)))
  text(region.pca4$x[, 1:2], labels = rf_HMISfrac_region_cov_1$Admin_Unit_Name, col = ifelse(rf_HMISfrac_region_cov_1$IHME_location_id %in% rf_HMISfrac_region_no_data, "blue", "black"), cex = 0.7)

  dev.off()
  
  pdf(paste(graphics.path, master.superregion.list[j], '.trend.rf.covariates.PCA_propvar.pdf', sep = ""),width=12, height = 12)
  
  par(mfrow=c(2,2))
  
  prop_var_plot(prin_comp = region.pca1, plot.title = "(a) Trend: Any Treatment Seeking")
  prop_var_plot(prin_comp = region.pca2, plot.title = "(b) Trend: Public Fractions")
  prop_var_plot(prin_comp = region.pca3, plot.title = "(c) Random Effect: Any Treatment Seeking")
  prop_var_plot(prin_comp = region.pca4, plot.title = "(d) Random Effect: Public Fractions")
  
  dev.off()
  
  
  
}


# Check:
sum(full_TreatSeek_n$rf.unit %in% units_no_Any_data)
sum(full_TreatSeek_n$rf.unit.pf %in% units_no_HMISfrac_data)

NN_results <- full_TreatSeek_n[full_TreatSeek_n$IHME_location_id %in% c(units_no_Any_data, units_no_HMISfrac_data, units_1_Any_data, units_1_HMISfrac_data) & full_TreatSeek_n$Year == 2019, c("ISO2", "ISO3", "GAUL_Code", "Country_Name", "Admin_Unit_Level", "Admin_Unit_Name", "IHME_location_id", "IHME_Super_Region_Name", "IHME_Region_Name", "trend.unit", "trend.unit.pf", "rf.unit", "rf.unit.pf")]

col.names <- c("trend.unit", "trend.unit.pf", "rf.unit", "rf.unit.pf")

for (j in col.names){
  NN_results[, j] <- as.character(NN_results[, j])
  temp.trend.unit <- as.character(unique(NN_results[, j]))
  for (i in 1:length(temp.trend.unit)){
    temp.name <- as.character(unique(full_TreatSeek_n$Admin_Unit_Name[full_TreatSeek_n$IHME_location_id == temp.trend.unit[i]]))
    NN_results[na.omit(NN_results[, j] == temp.trend.unit[i]), j] <- temp.name
  }  
}

head(NN_results)

write.csv(NN_results, file = paste(data.path, "trend_rf_NN.csv", sep = ""))

# Check Brazil and China random effects:

NN_results[NN_results$ISO3 == "CHN", ]
NN_results[NN_results$ISO3 == "BRA", ]

# Implement matched trends:

switch_units <- which(full_TreatSeek_n$IHME_location_id != full_TreatSeek_n$trend.unit)
for (i in switch_units){
  full_TreatSeek_n$Time_Factor[i] <-  unique(full_TreatSeek_n$Time_Factor[full_TreatSeek_n$IHME_location_id == full_TreatSeek_n$trend.unit[i]])
}

switch_units_2 <- which(full_TreatSeek_n$IHME_location_id != full_TreatSeek_n$trend.unit.pf)
for (i in switch_units_2){
  full_TreatSeek_n$Time_Factor_2[i] <-  unique(full_TreatSeek_n$Time_Factor_2[full_TreatSeek_n$IHME_location_id == full_TreatSeek_n$trend.unit.pf[i]])
}

# ----------------------------------------------- Fit the models...

# Reset if not using matched random effects:
# full_TreatSeek_n$rf.unit <- full_TreatSeek_n$Admin_Unit_Name
# full_TreatSeek_n$rf.unit.pf <- full_TreatSeek_n$Admin_Unit_Name


# Apply data outliering and post-1990 restriction:

Any_outliers <- which((clean_TreatSeek_Any$ISO3 == "BRA" & clean_TreatSeek_Any$Year == 1996) | (clean_TreatSeek_Any$ISO3 == "IND" & clean_TreatSeek_Any$Year == 1993) | (clean_TreatSeek_Any$ISO3 == "PAK" & clean_TreatSeek_Any$Year == 1991) | (clean_TreatSeek_Any$ISO3 == "NGA" & clean_TreatSeek_Any$Year == 2008)) 
HMIS_outliers <- which((clean_TreatSeek_HMISfrac$ISO3 == "BRA" & clean_TreatSeek_HMISfrac$Year == 1996) | (clean_TreatSeek_HMISfrac$ISO3 == "IND" & clean_TreatSeek_HMISfrac$Year == 1993) | (clean_TreatSeek_HMISfrac$ISO3 == "PAK" & clean_TreatSeek_HMISfrac$Year == 1991) | (clean_TreatSeek_HMISfrac$ISO3 == "NGA" & clean_TreatSeek_HMISfrac$Year == 2008)) 

Any_data <- clean_TreatSeek_Any[-Any_outliers, ]
HMIS_data <- clean_TreatSeek_HMISfrac[-HMIS_outliers, ]

Any_data <- Any_data[Any_data$Year > 1989, ]
HMIS_data <- HMIS_data[HMIS_data$Year > 1989, ]

Any_data$IHME_location_id <- as.factor(Any_data$IHME_location_id)
HMIS_data$IHME_location_id <- as.factor(HMIS_data$IHME_location_id)

# 3. PREF:
Any_model <-  gamm(formula_any_1, data = Any_data, random = list(IHME_location_id = ~ 1)) #, control = list(niterEM=1, opt='optim', maxit = 500))
HMIS_model <- gamm(formula_hmis_1, data = HMIS_data, random = list(IHME_location_id = ~ 1)) #, control = list(niterEM=1, opt='optim', maxit = 500))

summary(Any_model$gam)
summary(HMIS_model$gam)

# ------------ 1b. Ignore the variability in Any_Treat, HMIS_Treat and the IHME covariates -----------


# Compute mean and 95% confidence intervals (no random effects):

# Any_pred_initial <- predict(Any_model, newdata = full_TreatSeek_n, se.fit = TRUE)
# HMIS_pred_initial <- predict(HMIS_model, newdata = full_TreatSeek_n, se.fit = TRUE)
#
# full_TreatSeek_n$Any_pred_initial <- inv.logit(Any_pred_initial$fit)
# full_TreatSeek_n$Any_pred_initial_low <- inv.logit(Any_pred_initial$fit + qnorm(0.025)*Any_pred_initial$se.fit)
# full_TreatSeek_n$Any_pred_initial_high <- inv.logit(Any_pred_initial$fit + qnorm(0.975)*Any_pred_initial$se.fit)
#
# full_TreatSeek_n$HMIS_pred_initial <- full_TreatSeek_n$Any_pred_initial*inv.logit(HMIS_pred_initial$fit)
# full_TreatSeek_n$HMIS_pred_initial_low <- full_TreatSeek_n$Any_pred_initial_low*inv.logit(HMIS_pred_initial$fit + qnorm(0.025)*HMIS_pred_initial$se.fit)
# full_TreatSeek_n$HMIS_pred_initial_high <- full_TreatSeek_n$Any_pred_initial_high*inv.logit(HMIS_pred_initial$fit + qnorm(0.975)*HMIS_pred_initial$se.fit)

Any_rf <- rep(NA, nrow(full_TreatSeek_n))

# for (j in 1:length(master.region.list)){
#   units.in.region <- unique(full_TreatSeek_n$rf.unit[full_TreatSeek_n$IHME_Region_Name == master.region.list[j]])
for (i in 1:length(units_with_Any_data)){
  # for (i in 1:length(units.in.region)){
  # 1. REF:
  # Any_rf[full_TreatSeek_n$rf.unit == as.character(units.in.region[i])] <- ranef(Any_model$lme,level= 11)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(Any_model$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(Any_model$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
  Any_rf[full_TreatSeek_n$rf.unit == as.character(units_with_Any_data[i])] <- ifelse(is.na(ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_Any_data[i], sep = ""), ]), 0, ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_Any_data[i], sep = ""), ])
}
# }

Any_rf[is.na(Any_rf)] <- 0

HMIS_rf <- rep(NA, nrow(full_TreatSeek_n))

# for (j in 1:length(master.region.list)){
# units.in.region <- unique(full_TreatSeek_n$rf.unit[full_TreatSeek_n$IHME_Region_Name == master.region.list[j]])
for (i in 1:length(units_with_HMISfrac_data)){
  # for (i in 1:length(units.in.region)){
  # 1. REF:
  # HMIS_rf[full_TreatSeek_n$rf.unit == as.character(units.in.region[i])] <- ranef(HMIS_model$lme,level= 14)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(HMIS_model$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
  # 2. REF:
  HMIS_rf[full_TreatSeek_n$rf.unit.pf == as.character(units_with_HMISfrac_data[i])] <- ifelse(is.na(ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ])
}
# }

HMIS_rf[is.na(HMIS_rf)] <- 0



# check:
sum(Any_rf == 0)
sum(HMIS_rf == 0)

Any_pred_initial <- predict(Any_model, newdata = full_TreatSeek_n, se.fit = TRUE)
HMIS_pred_initial <- predict(HMIS_model, newdata = full_TreatSeek_n, se.fit = TRUE)

full_TreatSeek_n$Any_pred_initial <- inv.logit(Any_pred_initial$fit + Any_rf)
full_TreatSeek_n$Any_pred_initial_low <- inv.logit(Any_pred_initial$fit + Any_rf + qnorm(0.025)*Any_pred_initial$se.fit)
full_TreatSeek_n$Any_pred_initial_high <- inv.logit(Any_pred_initial$fit + Any_rf + qnorm(0.975)*Any_pred_initial$se.fit)

full_TreatSeek_n$HMIS_pred_initial <- full_TreatSeek_n$Any_pred_initial*inv.logit(HMIS_pred_initial$fit + HMIS_rf)
full_TreatSeek_n$HMIS_pred_initial_low <- full_TreatSeek_n$Any_pred_initial_low*inv.logit(HMIS_pred_initial$fit + HMIS_rf + qnorm(0.025)*HMIS_pred_initial$se.fit)
full_TreatSeek_n$HMIS_pred_initial_high <- full_TreatSeek_n$Any_pred_initial_high*inv.logit(HMIS_pred_initial$fit + HMIS_rf + qnorm(0.975)*HMIS_pred_initial$se.fit)

head(full_TreatSeek_n)

tail(full_TreatSeek_n)

# Read in results for plotting only:
# full_TreatSeek_n  <- read.csv(file = paste(data.path, 'TS_predictions.csv', sep = ''))

pdf(paste(graphics.path, 'Any_Treat_initial.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
    for (k in 1:nrow(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list$Admin_Unit_Name[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')

      plotCI(years,unit.row$Any_pred_initial,ui=unit.row$Any_pred_initial_high,li=unit.row$Any_pred_initial_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_Any[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
        # points(country_line$Year, country_line$Report_Treat, col = 'blue')
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
      }
    }
  }
}

dev.off()

pdf(paste(graphics.path, 'Public_fraction_initial.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
    for (k in 1:nrow(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list$Admin_Unit_Name[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers who sought treatment went to public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred_initial/unit.row$Any_pred_initial,ui=unit.row$HMIS_pred_initial_high/unit.row$Any_pred_initial_high,li=unit.row$HMIS_pred_initial_low/unit.row$Any_pred_initial_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_HMISfrac[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$HMIS_treat/country_line$Any_treat,ui=country_line$HMIS_treat_high_SVY/country_line$Any_treat_low_SVY,li=country_line$HMIS_treat_low_SVY/country_line$Any_treat_high_SVY,ylim=c(0,1),add=T, col = points.col)
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
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
    unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
    for (k in 1:nrow(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list$Admin_Unit_Name[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment at public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred_initial,ui=unit.row$HMIS_pred_initial_high,li=unit.row$HMIS_pred_initial_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_HMISfrac[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
        points(country_line$Year, country_line$Report_Treat, col = 'blue')
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
      }
    }
  }
}

dev.off()

# ------------ 2. Consider the variability in Any_Treat, HMIS_Treat and the IHME covariates -----------

load(file = paste(data.path, 'TS_datasets.RData', sep = ''))
N_TS <- 100

## For each of the 100 datasets, fit the chosen models and generate 100 prediction sets.

Any_pred_list <- rep(list(NA), N_TS)
HMIS_pred_list <- rep(list(NA), N_TS)

names(Any_pred_list) <- paste("Dataset", 1:N_TS, sep = "_")
names(HMIS_pred_list) <- paste("Dataset", 1:N_TS, sep = "_")

ref_error <- NA

temp_time <- proc.time()[3]

## Apply lower and upper bounds for the numeric covariates which were simulated.
for (TS_j in 1:N_TS){
  dataset <- TS_datasets[[TS_j]]
  dataset$Any_treat[dataset$Any_treat>1] <- 0.999 # If Any TS > 1 due to random simulations, set upper bound of public frac to be 0.999.
  dataset$Any_treat[dataset$Any_treat<0.001] <- 0.001 # If Any TS <0 due to random simulations, set it to 0.001.
  dataset$logit_Any <- logit(dataset$Any_treat)
  temp_HMIS_frac <- dataset$HMIS_treat/dataset$Any_treat
  temp_HMIS_frac[temp_HMIS_frac>1] <- 0.999 # If public TS > Any TS due to random simulations, set upper bound of public frac to be 0.999.
  temp_HMIS_frac[temp_HMIS_frac<0.001] <- 0.001
  dataset$logit_HMIS <- logit(temp_HMIS_frac)

  #  Remove name attributes and set Year as a numeric covariate:
  dataset$Year <- as.numeric(dataset$Year)
  dataset$SurveyName <- as.character(dataset$SurveyName)

  # For PTTIR onwards (regional temporal trends):

  dataset$Time_Factor <- as.character(dataset$IHME_Region_Name)
  dataset$Time_Factor[!(dataset$IHME_Region_Name %in% c("Central Asia", "North Africa and Middle East", 'South Asia', 'Southeast Asia', 'Western Sub-Saharan Africa'))] <- 'Other'
  dataset$Time_Factor[!(dataset$ISO3 %in% c('ETH', 'MYT', 'COM', 'MDG'))  & dataset$IHME_Region_Name == 'Eastern Sub-Saharan Africa'] <- 'Western Sub-Saharan Africa'
  dataset$Time_Factor <- as.factor(dataset$Time_Factor)
  unique(dataset$Time_Factor)
  dataset$Time_Factor <- relevel(dataset$Time_Factor, ref = 'Other')

  dataset$Time_Factor_2 <- as.character(dataset$IHME_Region_Name)
  dataset$Time_Factor_2[!(dataset$IHME_Region_Name %in% c("Central Latin America", 'Southeast Asia', "Western Sub-Saharan Africa",  "Eastern Sub-Saharan Africa"))] <- 'Other'
  dataset$Time_Factor_2 <- as.factor(dataset$Time_Factor_2)
  unique(dataset$Time_Factor_2)
  dataset$Time_Factor_2 <- relevel(dataset$Time_Factor_2, ref = 'Other')

  for (i in switch_units){
    dataset$Time_Factor[i] <-  unique(full_TreatSeek_n$Time_Factor[full_TreatSeek_n$IHME_location_id == full_TreatSeek_n$trend.unit[i]])
  }

  for (i in switch_units_2){
    dataset$Time_Factor_2[i] <-  unique(full_TreatSeek_n$Time_Factor_2[full_TreatSeek_n$IHME_location_id == full_TreatSeek_n$trend.unit.pf[i]])
  }

  # For PTTFR onwards (coarser regional factors):

  dataset$Reg_Factor <- as.character(dataset$IHME_Region_Name)
  dataset$Reg_Factor[!(dataset$IHME_Region_Name %in% c('Southeast Asia','South Asia', "Central Asia", 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa'))] <- 'Other'
  dataset$Reg_Factor <- as.factor(dataset$Reg_Factor)
  unique(dataset$Reg_Factor)
  dataset$Reg_Factor <- relevel(dataset$Reg_Factor, ref = 'Other')
  
  dataset$Reg_Factor_2 <- as.character(dataset$IHME_Region_Name)
  dataset$Reg_Factor_2[!(dataset$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Southeast Asia"))] <- 'Other'
  dataset$Reg_Factor_2 <- as.factor(dataset$Reg_Factor_2)
  unique(dataset$Reg_Factor_2)
  dataset$Reg_Factor_2 <- relevel(dataset$Reg_Factor_2, ref = 'Other')

  dataset$IHME_Region_Name  <- as.factor(dataset$IHME_Region_Name)
  dataset$IHME_location_id  <- as.factor(dataset$IHME_location_id)
  
  #  Option 1: Fit models on cleaned data and store 100 predictions for full data set:

  clean_Any <- dataset[!is.na(dataset$logit_Any) & is.finite(dataset$logit_Any), ]

  # nrow(clean_Any)

  clean_HMIS <- dataset[!is.na(dataset$logit_HMIS) & is.finite(dataset$logit_HMIS), ]

  # nrow(clean_HMIS)
  # summary(clean_HMIS$logit_HMIS)

  Any_outliers <- which((clean_Any$ISO3 == "BRA" & clean_Any$Year == 1996) | (clean_Any$ISO3 == "IND" & clean_Any$Year == 1993) | (clean_Any$ISO3 == "PAK" & clean_Any$Year == 1991) | (clean_Any$ISO3 == "NGA" & clean_Any$Year == 2008))
  HMIS_outliers <- which((clean_HMIS$ISO3 == "BRA" & clean_HMIS$Year == 1996) | (clean_HMIS$ISO3 == "IND" & clean_HMIS$Year == 1993) | (clean_HMIS$ISO3 == "PAK" & clean_HMIS$Year == 1991)  | (clean_HMIS$ISO3 == "NGA" & clean_HMIS$Year == 2008))

  Any_data <- clean_Any[-Any_outliers, ]
  HMIS_data <- clean_HMIS[-HMIS_outliers, ]

  Any_data <- Any_data[Any_data$Year > 1989, ]
  HMIS_data <- HMIS_data[HMIS_data$Year > 1989, ]

  # Any_fit <-  gam(formula_any_1, data = Any_data)
  # HMIS_fit <- gam(formula_hmis_1, data = HMIS_data)

  Any_data$Admin_Unit_Name <- as.factor(Any_data$Admin_Unit_Name)
  HMIS_data$Admin_Unit_Name <- as.factor(HMIS_data$Admin_Unit_Name)

  # Refit the models on simulated datasets:

  Any_fit <- try(gamm(formula_any_1, data = Any_data, random = list(IHME_location_id = ~ 1)))
  if("try-error" %in% class(Any_fit)){
    Any_fit <-  gamm(formula_any_1, data = Any_data, random = list(IHME_location_id = ~ 1), control = list(opt='optim')) # list(niterEM=1, opt='optim', maxit = 500)
  }

  HMIS_fit <- try(gamm(formula_hmis_1, data = HMIS_data, random = list(IHME_location_id = ~ 1)))
  if("try-error" %in% class(HMIS_fit)){
    HMIS_fit <- try(gamm(formula_hmis_1, data = HMIS_data, random = list(IHME_location_id = ~ 1), control = list(opt='optim')))
  }
  if("try-error" %in% class(HMIS_fit)){
  HMIS_fit <- gamm(formula_hmis_1, data = HMIS_data, random = list(IHME_location_id = ~ 1), control = list(niterEM=1, opt='optim', maxit = 500))
  }

  # # Option 2: Use original fitted model:
  # Any_fit <- Any_model
  # HMIS_fit <- HMIS_model

  Any_pred <- matrix(NA, nrow = nrow(dataset), ncol = 100)
  HMIS_pred <- matrix(NA, nrow = nrow(dataset), ncol = 100)

  # Freeze non-linear temporal trend pre 1995:
  dataset$Year[dataset$Year < 1995] <- 1995

  Any_pred_distr <- predict(Any_fit, newdata = dataset, se.fit = TRUE)
  HMIS_pred_distr <- predict(HMIS_fit, newdata = dataset, se.fit = TRUE)

  Any_rf <- rep(NA, nrow(full_TreatSeek_n))

  # for (j in 1:length(master.region.list)){
  #   units.in.region <- unique(full_TreatSeek_n$rf.unit[full_TreatSeek_n$IHME_Region_Name == master.region.list[j]])
  for (i in 1:length(units_with_Any_data)){
    # for (i in 1:length(units.in.region)){
    # 1. REF:
    # Any_rf[full_TreatSeek_n$rf.unit == as.character(units.in.region[i])] <- ranef(Any_fit$lme,level= 11)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(Any_fit$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(Any_fit$lme,level= 12)[paste("1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
    Any_rf[full_TreatSeek_n$rf.unit == as.character(units_with_Any_data[i])] <- ifelse(is.na(ranef(Any_fit$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_Any_data[i], sep = ""), ]), 0, ranef(Any_fit$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_Any_data[i], sep = ""), ])
  }
  # }

  Any_rf[is.na(Any_rf)] <- 0

  HMIS_rf <- rep(NA, nrow(full_TreatSeek_n))

  # for (j in 1:length(master.region.list)){
  # units.in.region <- unique(full_TreatSeek_n$rf.unit[full_TreatSeek_n$IHME_Region_Name == master.region.list[j]])
  for (i in 1:length(units_with_HMISfrac_data)){
    # for (i in 1:length(units.in.region)){
    # 1. REF:
    # HMIS_rf[full_TreatSeek_n$rf.unit == as.character(units.in.region[i])] <- ranef(HMIS_fit$lme,level= 14)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], sep = ""), ] + ifelse(is.na(ranef(HMIS_fit$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ]), 0, ranef(HMIS_fit$lme,level= 15)[paste("1/1/1/1/1/1/1/1/1/1/1/1/1/", master.region.list[j], "/", units.in.region[i], sep = ""), ])
    # 2. REF:
    HMIS_rf[full_TreatSeek_n$rf.unit.pf == as.character(units_with_HMISfrac_data[i])] <- ifelse(is.na(ranef(HMIS_fit$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ]), 0, ranef(HMIS_fit$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ])
  }
  # }

  HMIS_rf[is.na(HMIS_rf)] <- 0

  for (i in 1:100){
    set.seed(i)
    Any_pred[, i] <- rnorm(nrow(dataset), mean = Any_pred_distr$fit + Any_rf, sd = Any_pred_distr$se.fit)
    HMIS_pred[, i] <- rnorm(nrow(dataset), mean = HMIS_pred_distr$fit + HMIS_rf, sd = HMIS_pred_distr$se.fit)
  }
  Any_pred_list[[TS_j]] <- Any_pred
  HMIS_pred_list[[TS_j]] <- HMIS_pred

  if((sum(Any_rf == 0) + sum(HMIS_rf == 0))==0){
    print(paste("Dataset ", TS_j, " done.", sep = ""))
  }else{
    ref_error <- c(ref_error, TS_j)
    print(paste("Dataset ", TS_j, ": Random effect matching error.", sep = ""))}

}

time.taken <- proc.time()[3] - temp_time

full_Any_pred <- do.call(cbind, Any_pred_list)
full_HMIS_pred <- do.call(cbind, HMIS_pred_list)

# full_Any_pred <-Any_pred_list[[TS_j]] # For testing.
# full_HMIS_pred <-  HMIS_pred_list[[TS_j]] # For testing.

full_Any_pred_raw <- inv.logit(full_Any_pred)
full_HMIS_pred_raw <- inv.logit(full_HMIS_pred)

# Compute mean and 95% confidence intervals:

full_TreatSeek_n$Any_pred <- rowMeans(full_Any_pred_raw)
full_TreatSeek_n$Any_pred_low <- apply(full_Any_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_n$Any_pred_high <- apply(full_Any_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

full_TreatSeek_n$HMIS_pred <- rowMeans(full_HMIS_pred_raw*full_Any_pred_raw)
full_TreatSeek_n$HMIS_pred_low <- apply(full_HMIS_pred_raw*full_Any_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_n$HMIS_pred_high <- apply(full_HMIS_pred_raw*full_Any_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

full_TreatSeek_n$HMISfrac_pred <- rowMeans(full_HMIS_pred_raw)
full_TreatSeek_n$HMISfrac_pred_low <- apply(full_HMIS_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_n$HMISfrac_pred_high <- apply(full_HMIS_pred_raw, MARGIN = 1, function(x){quantile(x, probs = 0.975)})


head(full_TreatSeek_n)

master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name) # This has grouped Oceania with SE Asia etc. Use full_TreatSeek if want true regions.
master.unit.list <- unique(full_TreatSeek_n$Admin_Unit_Name)

# To plot ADMIN0 only:
# full_TreatSeek_original <- full_TreatSeek_n
# full_TreatSeek_n <- full_TreatSeek_n[full_TreatSeek_n$Admin_Unit_Level == "ADMIN0", ]

# Public treatment seeking from Sam, aggregated to admin:
africa_means <- read.csv("J:\\Treatment_Seeking\\Data\\treatment_seeking_africa_weighted_means_adm1.csv") # This now includes GUF and MYT, and gets updated with API dump.
str(africa_means)
summary(africa_means$year)
africa_means$Name_1 <- as.character(africa_means$Name_1)
africa_means$ISO <- as.character(africa_means$ISO)

# Match ihme admin names:
africa_means$Name_1[africa_means$Name_1 == "Addis Ababa" & africa_means$ISO == "ETH"] <- "Addis Abeba"
africa_means$Name_1[africa_means$Name_1 == "Beneshangul Gumu" & africa_means$ISO == "ETH"] <- "Benshangul-Gumaz"
africa_means$Name_1[africa_means$Name_1 == "Gambela" & africa_means$ISO == "ETH"] <- "Gambela Peoples"
africa_means$Name_1[africa_means$Name_1 == "Hareri" & africa_means$ISO == "ETH"] <- "Harari People"
africa_means$Name_1[africa_means$Name_1 == "SNNPR" & africa_means$ISO == "ETH"] <- "Southern Nations, Nationalities and Peoples"
africa_means$Name_1[africa_means$Name_1 == "Abuja" & africa_means$ISO == "NGA"] <- "Federal Capital Territory"

africa_iso <- unique(africa_means$ISO)
africa_units <- unique(africa_means$Name_1)

pdf(paste(graphics.path, 'Any_Treat.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
    for (k in 1:nrow(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list$Admin_Unit_Name[k], ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')

      plotCI(years,unit.row$Any_pred,ui=unit.row$Any_pred_high,li=unit.row$Any_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_Any[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
        points(country_line$Year, country_line$Report_Treat, col = 'blue')
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
      }
    }
  }
}

dev.off()

pdf(paste(graphics.path, 'Public_fraction.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
    for (k in 1:nrow(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list$Admin_Unit_Name[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers who sought treatment went to public facilities',xlab='Year')

      plotCI(years,unit.row$HMISfrac_pred,ui=unit.row$HMISfrac_pred_high,li=unit.row$HMISfrac_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_HMISfrac[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$HMIS_treat/country_line$Any_treat,ui=country_line$HMIS_treat_high_SVY/country_line$Any_treat_low_SVY,li=country_line$HMIS_treat_low_SVY/country_line$Any_treat_high_SVY,ylim=c(0,1),add=T, col = points.col)

      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
      }
    }
  }
}

dev.off()





pdf(paste(graphics.path, 'HMIS_Treat.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
    for (k in 1:nrow(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list$Admin_Unit_Name[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment at public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred,ui=unit.row$HMIS_pred_high,li=unit.row$HMIS_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_HMISfrac[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
        # points(country_line$Year, country_line$Report_Treat, col = 'blue')
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted', 'Aggregated from pixels'),pch=1,col=c('red','blue', 'black', 'orange'))
      }
      if(unique(unit.row$ISO3) %in% as.character(africa_iso) & unique(unit.row$Admin_Unit_Name) %in% as.character(africa_units)){
        africa_points <- africa_means[as.character(africa_means$ISO) == unique(unit.row$ISO3) & as.character(africa_means$Name_1) == unique(unit.row$Admin_Unit_Name), ]
        points(africa_points$year, africa_points$public_treatment, col = 'orange')
      }
    }
  }
}

dev.off()

# full_TreatSeek_n <- full_TreatSeek_original

#  Save the results:

write.csv(full_TreatSeek_n, file = paste(data.path, 'TS_predictions_GAMkNN.csv', sep = '')) # For Option 1: With the uncertainty in the model fit considered.
# write.csv(full_TreatSeek_n, file = paste(data.path, 'TS_predictions_Option2.csv', sep = '')) # For Option 2: Without the uncertainty in the model fit.

# ------------ 2b. Plot regional trends against regional data...

Any_pred_realisations <- cbind(full_TreatSeek[, c("ISO2", "ISO3", "GAUL_Code", "Country_Name", "Admin_Unit_Level", "Admin_Unit_Name", "WHO_Region", "WHO_Subregion", "IHME_location_id", "IHME_Super_Region_ID", "IHME_Super_Region_Name", "IHME_Region_ID", "IHME_Region_Name", "Year")], full_Any_pred_raw)
HMIS_pred_realisations <- cbind(full_TreatSeek[, c("ISO2", "ISO3", "GAUL_Code", "Country_Name", "Admin_Unit_Level", "Admin_Unit_Name", "WHO_Region", "WHO_Subregion", "IHME_location_id", "IHME_Super_Region_ID", "IHME_Super_Region_Name", "IHME_Region_ID", "IHME_Region_Name", "Year")], full_Any_pred_raw*full_HMIS_pred_raw)

# Set Super Regions of GUF and MYT to Latin America and Carribean and Sub-Saharan Africa (i.e. that of Suriname and Comoros respectively):

full_TreatSeek$IHME_Super_Region_Name[full_TreatSeek$ISO3 == "GUF"] <- "Latin America and Caribbean"
full_TreatSeek$IHME_Super_Region_Name[full_TreatSeek$ISO3 == "MYT"] <- "Sub-Saharan Africa"
full_TreatSeek$IHME_Region_Name[full_TreatSeek$ISO3 == "MYT"] <- "Eastern Sub-Saharan Africa"

master.superregion.list <- unique(full_TreatSeek$IHME_Super_Region_Name)

superregion_mean_realisations <- data.frame("IHME_Super_Region_name" = rep(master.superregion.list, each = length(years)), "Year" = rep(years, length(master.superregion.list)))

superregion_aggreg <- function(pred_realisations = Any_pred_realisations, master.list = master.superregion.list, ssa = FALSE, covariates = FALSE, cov.col = "ANC1_coverage_prop"){
  
  temp.time <- proc.time()[3]
  if(covariates){
    realisation_col <- cov.col
  }else{realisation_col <- as.character(1:10000)}
  if(ssa){
    list_col <- pred_realisations$IHME_Region_Name
  }else{list_col <- pred_realisations$IHME_Super_Region_Name}
  
  temp_realisations <- rep(NA, length(realisation_col))
  names(temp_realisations) <- realisation_col
  pop_error <- c()
  
  # No populations (esp for the MAP infants category) for Cook Islands, Monaco, Nauru, Niue, Palau, Saint Kitts and Nevis, San Marino, Tokelau and Tuvalu.
  # Currently their regions' aggregation excludes these.
  
  for (k in 1:length(master.list)){ # Region
    Countries <- pred_realisations[list_col == master.list[k] & pred_realisations$Admin_Unit_Level == "ADMIN0", c("IHME_location_id", "Year", realisation_col)]
    Region_aggreg <- matrix(NA, nrow =length(years), ncol = length(realisation_col))
    for (i in 1:length(years)){ # Region-Year
      year_data <- Countries[Countries$Year == years[i], ]
      year_realisations <- year_data[, realisation_col]
      year_pop <- rep(NA, nrow(year_data))
      for (j in 1:length(year_pop)){ # Country-Year
        if(length(ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]]) > 0){
          year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]][1]
          print(paste("Region ", k, ", ", master.list[k], ": ", years[i], ", Country ", j, "/", length(year_pop), ", ", (proc.time()[3] - temp.time)/60, " minutes taken.", sep = ""))
        }else{
          pop_error <- c(pop_error, year_data$IHME_location_id[j])
          print(paste("Region ", k, ", ", master.list[k], ": ", years[i], ", Country ", j, "/", length(year_pop), ", ", " IHME Population Error.", sep = ""))}
      }
      Region_aggreg[i, ] <- colSums(as.matrix(year_realisations, nrow  = length(year_pop), ncol = length(realisation_col))*matrix(rep(year_pop, length(realisation_col)), nrow = length(year_pop), ncol = length(realisation_col)), na.rm = TRUE)/sum(year_pop, na.rm = TRUE)
    }
    temp_realisations <- rbind(temp_realisations, Region_aggreg)
  }
  temp_realisations <- temp_realisations[-1, ]
  pop_error_countries <- unique(full_TreatSeek$Country_Name[full_TreatSeek$IHME_location_id %in% unique(pop_error)])
  return(list("temp_realisations" = temp_realisations, "pop_error_countries" = pop_error_countries))
}

SR_Any_pred_aggreg_list <- superregion_aggreg(pred_realisations = Any_pred_realisations) # About 8 minutes.
SR_Any_pred_aggreg <- SR_Any_pred_aggreg_list$temp_realisations
SR_Any_pred_aggreg_list$pop_error_countries # Check.

SR_HMIS_pred_aggreg_list <- superregion_aggreg(pred_realisations = HMIS_pred_realisations)
SR_HMIS_pred_aggreg <- SR_HMIS_pred_aggreg_list$temp_realisations
SR_HMIS_pred_aggreg_list$pop_error_countries # Check.

Any_superregion_mean_realisations <- cbind(superregion_mean_realisations, SR_Any_pred_aggreg)
HMIS_superregion_mean_realisations <- cbind(superregion_mean_realisations, SR_HMIS_pred_aggreg)

# Compute mean and 95% confidence intervals:

superregion_mean_realisations$Any_pred <- rowMeans(SR_Any_pred_aggreg)
superregion_mean_realisations$Any_pred_low <- apply(SR_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
superregion_mean_realisations$Any_pred_high <- apply(SR_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

superregion_mean_realisations$HMIS_pred <- rowMeans(SR_HMIS_pred_aggreg)
superregion_mean_realisations$HMIS_pred_low <- apply(SR_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
superregion_mean_realisations$HMIS_pred_high <- apply(SR_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

head(superregion_mean_realisations)

write.csv(superregion_mean_realisations,  file = paste(data.path, 'superregion_mean_realisations.csv', sep = ''))

# For each superregion, plot the regional trend and datapoints for each country. Remove legend for countries if too many. 

Any_outliers_2 <- which((full_TreatSeek$ISO3 == "BRA" & full_TreatSeek$Year == 1996) | (full_TreatSeek$ISO3 == "IND" & full_TreatSeek$Year == 1993) | (full_TreatSeek$ISO3 == "PAK" & full_TreatSeek$Year == 1991) | (full_TreatSeek$ISO3 == "NGA" & full_TreatSeek$Year == 2008)) # | (full_TreatSeek$ISO3 == "PHL" & full_TreatSeek$Year == 1993))
HMIS_outliers_2 <- which((full_TreatSeek$ISO3 == "BRA" & full_TreatSeek$Year == 1996) | (full_TreatSeek$ISO3 == "IND" & full_TreatSeek$Year == 1993) | (full_TreatSeek$ISO3 == "PAK" & full_TreatSeek$Year == 1991) | (full_TreatSeek$ISO3 == "NGA" & full_TreatSeek$Year == 2008)) # | (full_TreatSeek$ISO3 == "PHL" & full_TreatSeek$Year == 1993))

Any_data_2 <- full_TreatSeek[-Any_outliers_2, ]
HMIS_data_2 <- full_TreatSeek[-HMIS_outliers_2, ]

Any_data_2 <- Any_data_2[Any_data_2$Year > 1989, ]
HMIS_data_2 <- HMIS_data_2[HMIS_data_2$Year > 1989, ]

Any_data_2$Short_Super_Region_Name <- NA
HMIS_data_2$Short_Super_Region_Name <- NA
superregion_mean_realisations$Short_Super_Region_Name <- NA
short.superregion.list <- c("N.A.M.E.", "S.S.A.", "High-Income", "C.E.E.C.A.", "S. Asia", "L.A.C.", "S.E. & E. Asia, Oceania")

for (i in 1: length(master.superregion.list)){
  Any_data_2$Short_Super_Region_Name[Any_data_2$IHME_Super_Region_Name == master.superregion.list[i]] <- short.superregion.list[i]
  HMIS_data_2$Short_Super_Region_Name[HMIS_data_2$IHME_Super_Region_Name == master.superregion.list[i]] <- short.superregion.list[i]
  superregion_mean_realisations$Short_Super_Region_Name[superregion_mean_realisations$IHME_Super_Region_name == master.superregion.list[i]] <- short.superregion.list[i]
}


p1 <- ggplot(data = superregion_mean_realisations, aes(x = Year, y = Any_pred, group = Short_Super_Region_Name, colour = Short_Super_Region_Name)) + geom_line() + geom_ribbon(data=superregion_mean_realisations,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = Short_Super_Region_Name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "")  + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

plot_list_Any <- list()

for (k in 1:length(master.superregion.list)){
  
  # Country data:
  Countries_Any <- na.omit(Any_data_2[Any_data_2$IHME_Super_Region_Name == master.superregion.list[k] & Any_data_2$Admin_Unit_Level == "ADMIN0", c("Admin_Unit_Name", "Year", "Any_treat")])
  n_countries <- length(unique(Countries_Any$Admin_Unit_Name))
  Region <- superregion_mean_realisations[superregion_mean_realisations$IHME_Super_Region_name == master.superregion.list[k], ]
  
  plot.title.k <- paste(master.superregion.list[k], " (", short.superregion.list[k], ") ", sep = "")
  if (k == 3){plot.title.k <- master.superregion.list[k]}
  p1a <- ggplot(data = Region, aes(x = Year, y = Any_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=Any_pred_low,ymax=Any_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k) + ylim(0, 1) 
  p1b <- p1a + geom_point(data = Countries_Any, aes(x = Year, y = Any_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p1b <- p1b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10),  axis.title=element_text(size=10), legend.text=element_text(size=7))
  
  if (k == 2){p1b <- p1b + theme(legend.position = "none")}
  plot_list_Any[[k]] <- p1b 
  
}


p2 <- ggplot(data = superregion_mean_realisations, aes(x = Year, y = HMIS_pred, group = Short_Super_Region_Name, colour = Short_Super_Region_Name)) + geom_line() + geom_ribbon(data=superregion_mean_realisations,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = Short_Super_Region_Name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "")  + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

plot_list_HMIS <- list()

for (k in 1:length(master.superregion.list)){
  
  # Country data:
  Countries_HMIS <- na.omit(HMIS_data_2[HMIS_data_2$IHME_Super_Region_Name == master.superregion.list[k] & HMIS_data_2$Admin_Unit_Level == "ADMIN0", c("Admin_Unit_Name", "Year", "HMIS_treat")])
  n_countries <- length(unique(Countries_HMIS$Admin_Unit_Name))
  Region <- superregion_mean_realisations[superregion_mean_realisations$IHME_Super_Region_name == master.superregion.list[k], ]
  
  plot.title.k <- paste(master.superregion.list[k], " (", short.superregion.list[k], ") ", sep = "")
  if (k == 3){plot.title.k <- master.superregion.list[k]}
  
  p2a <- ggplot(data = Region, aes(x = Year, y = HMIS_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k)  + ylim(0, 1) 
  p2b <- p2a + geom_point(data = Countries_HMIS, aes(x = Year, y = HMIS_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p2b <- p2b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))
  
  if (k == 2){p2b <- p2b + theme(legend.position = "none")}
  plot_list_HMIS[[k]] <- p2b 
  
}


pdf(paste(graphics.path, 'Any_Regional_Averages.pdf'),width=10,height = 12)

grid.arrange(p1, plot_list_Any[[1]], plot_list_Any[[2]], plot_list_Any[[3]], plot_list_Any[[4]], plot_list_Any[[5]], plot_list_Any[[6]], plot_list_Any[[7]], nrow = 4)

dev.off()

pdf(paste(graphics.path, 'HMIS_Regional_Averages.pdf'),width=10,height = 12)

grid.arrange(p1, plot_list_HMIS[[1]], plot_list_HMIS[[2]], plot_list_HMIS[[3]], plot_list_HMIS[[4]], plot_list_HMIS[[5]], plot_list_HMIS[[6]], plot_list_HMIS[[7]], nrow = 4)

dev.off()


# ------------ Plots for 2000 onwards:

trun_version <- superregion_mean_realisations[superregion_mean_realisations$Year >= 2000, ]

p1 <- ggplot(data = trun_version, aes(x = Year, y = Any_pred, group = Short_Super_Region_Name, colour = Short_Super_Region_Name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = Short_Super_Region_Name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "")  + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

plot_list_Any <- list()

for (k in 1:length(master.superregion.list)){
  
  # Country data:
  Countries_Any <- na.omit(Any_data_2[Any_data_2$IHME_Super_Region_Name == master.superregion.list[k] & Any_data_2$Admin_Unit_Level == "ADMIN0" & Any_data_2$Year >= 2000, c("Admin_Unit_Name", "Year", "Any_treat")])
  n_countries <- length(unique(Countries_Any$Admin_Unit_Name))
  Region <- trun_version[trun_version$IHME_Super_Region_name == master.superregion.list[k], ]
  
  plot.title.k <- paste(master.superregion.list[k], " (", short.superregion.list[k], ") ", sep = "")
  if (k == 3){plot.title.k <- master.superregion.list[k]}
  
  p1a <- ggplot(data = Region, aes(x = Year, y = Any_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=Any_pred_low,ymax=Any_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k)  + ylim(0, 1) 
  p1b <- p1a + geom_point(data = Countries_Any, aes(x = Year, y = Any_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p1b <- p1b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))
  
  if (k == 2){p1b <- p1b + theme(legend.position = "none")}
  plot_list_Any[[k]] <- p1b 
  
}

p2 <- ggplot(data = trun_version, aes(x = Year, y = HMIS_pred, group = Short_Super_Region_Name, colour = Short_Super_Region_Name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = Short_Super_Region_Name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "")  + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

plot_list_HMIS <- list()

for (k in 1:length(master.superregion.list)){
  
  # Country data:
  Countries_HMIS <- na.omit(HMIS_data_2[HMIS_data_2$IHME_Super_Region_Name == master.superregion.list[k] & HMIS_data_2$Admin_Unit_Level == "ADMIN0" & HMIS_data_2$Year >= 2000, c("Admin_Unit_Name", "Year", "HMIS_treat")])
  n_countries <- length(unique(Countries_HMIS$Admin_Unit_Name))
  Region <- trun_version[trun_version$IHME_Super_Region_name == master.superregion.list[k], ]
  
  plot.title.k <- paste(master.superregion.list[k], " (", short.superregion.list[k], ") ", sep = "")
  if (k == 3){plot.title.k <- master.superregion.list[k]}
  
  
  p2a <- ggplot(data = Region, aes(x = Year, y = HMIS_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k)  + ylim(0, 1) 
  p2b <- p2a + geom_point(data = Countries_HMIS, aes(x = Year, y = HMIS_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p2b <- p2b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))
  
  if (k == 2){p2b <- p2b + theme(legend.position = "none")}
  plot_list_HMIS[[k]] <- p2b 
  
}

pdf(paste(graphics.path, 'Any_Regional_Averages_from_2000.pdf'),width=10,height = 12)

grid.arrange(p1, plot_list_Any[[1]], plot_list_Any[[2]], plot_list_Any[[3]], plot_list_Any[[4]], plot_list_Any[[5]], plot_list_Any[[6]], plot_list_Any[[7]], nrow = 4)

dev.off()

pdf(paste(graphics.path, 'HMIS_Regional_Averages_from_2000.pdf'),width=10,height = 12)

grid.arrange(p2, plot_list_HMIS[[1]], plot_list_HMIS[[2]], plot_list_HMIS[[3]], plot_list_HMIS[[4]], plot_list_HMIS[[5]], plot_list_HMIS[[6]], plot_list_HMIS[[7]], nrow = 4)

dev.off()

# ------------ Regional Any and HMIS trends without data for main paper:

p3 <- ggplot(data = trun_version, aes(x = Year, y = Any_pred, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = IHME_Super_Region_name),alpha=0.2, show.legend = FALSE, linetype = 0)   + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate")  + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1) + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

p4 <- ggplot(data = trun_version, aes(x = Year, y = HMIS_pred, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = IHME_Super_Region_name),alpha=0.2, show.legend = FALSE, linetype = 0)   + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1) + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

pdf(paste(graphics.path, 'Regional_Averages_from2000.pdf'),width=12,height = 5)

grid_arrange_shared_legend(p3, p4)

dev.off()

# ------------ Plots for Sub-Saharan Africa:

ssa.list <- as.character(unique(full_TreatSeek$IHME_Region_Name[full_TreatSeek$IHME_Super_Region_Name == "Sub-Saharan Africa"]))

ssa_mean_realisations <- data.frame("IHME_Region_name" = rep(ssa.list, each = length(years)), "Year" = rep(years, length(ssa.list)))

SSA_Any_pred_aggreg_list <- superregion_aggreg(pred_realisations = Any_pred_realisations, master.list = ssa.list, ssa = TRUE) # About 2 minutes.
SSA_Any_pred_aggreg <- SSA_Any_pred_aggreg_list$temp_realisations
SSA_Any_pred_aggreg_list$pop_error_countries # Check.

SSA_HMIS_pred_aggreg_list <- superregion_aggreg(pred_realisations = HMIS_pred_realisations, master.list = ssa.list, ssa = TRUE)
SSA_HMIS_pred_aggreg <- SSA_HMIS_pred_aggreg_list$temp_realisations
SSA_HMIS_pred_aggreg_list$pop_error_countries # Check.

Any_ssa_mean_realisations <- cbind(ssa_mean_realisations, SSA_Any_pred_aggreg)
HMIS_ssa_mean_realisations <- cbind(ssa_mean_realisations, SSA_HMIS_pred_aggreg)

# Compute mean and 95% confidence intervals:

ssa_mean_realisations$Any_pred <- rowMeans(SSA_Any_pred_aggreg)
ssa_mean_realisations$Any_pred_low <- apply(SSA_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
ssa_mean_realisations$Any_pred_high <- apply(SSA_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

ssa_mean_realisations$HMIS_pred <- rowMeans(SSA_HMIS_pred_aggreg)
ssa_mean_realisations$HMIS_pred_low <- apply(SSA_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
ssa_mean_realisations$HMIS_pred_high <- apply(SSA_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

write.csv(ssa_mean_realisations,  file = paste(data.path, 'ssa_mean_realisations.csv', sep = ''))

p6 <- ggplot(data = ssa_mean_realisations, aes(x = Year, y = Any_pred, group = IHME_Region_name, colour = IHME_Region_name)) + geom_line() + geom_ribbon(data=ssa_mean_realisations,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = IHME_Region_name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "")  + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

plot_list_Any <- list()

for (k in 1:length(ssa.list)){
  
  # Country data:
  Countries_Any <- na.omit(Any_data_2[Any_data_2$IHME_Region_Name == ssa.list[k] & Any_data_2$Admin_Unit_Level == "ADMIN0", c("Admin_Unit_Name", "Year", "Any_treat")])
  n_countries <- length(unique(Countries_Any$Admin_Unit_Name))
  Region <- ssa_mean_realisations[ssa_mean_realisations$IHME_Region_name == ssa.list[k], ]
  
  plot.title.k <- ssa.list[k]
  
  p6a <- ggplot(data = Region, aes(x = Year, y = Any_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=Any_pred_low,ymax=Any_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k) + ylim(0, 1) 
  p6b <- p6a + geom_point(data = Countries_Any, aes(x = Year, y = Any_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p6b <- p6b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10),  axis.title=element_text(size=10), legend.text=element_text(size=7))
  
  plot_list_Any[[k]] <- p6b 
  
}

pdf(paste(graphics.path, 'Any_Regional_Averages_SSA.pdf'),width=10,height = 10)

grid.arrange(p6, plot_list_Any[[1]], plot_list_Any[[2]], plot_list_Any[[3]], plot_list_Any[[4]], layout_matrix = rbind(c(1, 1), c(2, 3), c(4, 5)))

dev.off()

p7 <- ggplot(data = ssa_mean_realisations, aes(x = Year, y = HMIS_pred, group = IHME_Region_name, colour = IHME_Region_name)) + geom_line() + geom_ribbon(data=ssa_mean_realisations,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = IHME_Region_name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "")  + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))

plot_list_HMIS <- list()

for (k in 1:length(master.superregion.list)){
  
  # Country data:
  Countries_HMIS <- na.omit(HMIS_data_2[HMIS_data_2$IHME_Region_Name == ssa.list[k] & HMIS_data_2$Admin_Unit_Level == "ADMIN0", c("Admin_Unit_Name", "Year", "HMIS_treat")])
  n_countries <- length(unique(Countries_HMIS$Admin_Unit_Name))
  Region <- ssa_mean_realisations[ssa_mean_realisations$IHME_Region_name == ssa.list[k], ]
  
  plot.title.k <- ssa.list[k]
  
  p7a <- ggplot(data = Region, aes(x = Year, y = HMIS_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k)  + ylim(0, 1) 
  p7b <- p7a + geom_point(data = Countries_HMIS, aes(x = Year, y = HMIS_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p7b <- p7b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))
  
  plot_list_HMIS[[k]] <- p7b 
  
}

pdf(paste(graphics.path, 'HMIS_Regional_Averages_SSA.pdf'),width=10,height = 12)

grid.arrange(p7, plot_list_HMIS[[1]], plot_list_HMIS[[2]], plot_list_HMIS[[3]], plot_list_HMIS[[4]], layout_matrix = rbind(c(1, 1), c(2, 3), c(4, 5)))

dev.off()


# ------------ SSA Plots for 2000 onwards:

trun_version <- ssa_mean_realisations[ssa_mean_realisations$Year >= 2000, ]

p8 <- ggplot(data = trun_version, aes(x = Year, y = Any_pred, group = IHME_Region_name, colour = IHME_Region_name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = IHME_Region_name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate") + ylim(0, 1) + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8)) + scale_colour_discrete(name = "SSA region") 

plot_list_Any <- list()

for (k in 1:length(ssa.list)){
  
  # Country data:
  Countries_Any <- na.omit(Any_data_2[Any_data_2$IHME_Region_Name == ssa.list[k] & Any_data_2$Admin_Unit_Level == "ADMIN0" & Any_data_2$Year >= 2000, c("Admin_Unit_Name", "Year", "Any_treat")])
  n_countries <- length(unique(Countries_Any$Admin_Unit_Name))
  Region <- trun_version[trun_version$IHME_Region_name == ssa.list[k], ]
  
  plot.title.k <- ssa.list[k]
  
  p8a <- ggplot(data = Region, aes(x = Year, y = Any_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=Any_pred_low,ymax=Any_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k) + ylim(0, 1) 
  p8b <- p8a + geom_point(data = Countries_Any, aes(x = Year, y = Any_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p8b <- p8b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10),  axis.title=element_text(size=10), legend.text=element_text(size=7))
  
  plot_list_Any[[k]] <- p8b 
  
}

pdf(paste(graphics.path, 'Any_Regional_Averages_SSA_from_2000.pdf'),width=10,height = 10)

grid.arrange(p8, plot_list_Any[[1]], plot_list_Any[[2]], plot_list_Any[[3]], plot_list_Any[[4]], layout_matrix = rbind(c(1, 1), c(2, 3), c(4, 5)))

dev.off()

p9 <- ggplot(data = trun_version, aes(x = Year, y = HMIS_pred, group = IHME_Region_name, colour = IHME_Region_name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = IHME_Region_name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + ylim(0, 1)+ theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8)) + scale_colour_discrete(name = "SSA region") 

plot_list_HMIS <- list()

for (k in 1:length(master.superregion.list)){
  
  # Country data:
  Countries_HMIS <- na.omit(HMIS_data_2[HMIS_data_2$IHME_Region_Name == ssa.list[k] & HMIS_data_2$Admin_Unit_Level == "ADMIN0" & HMIS_data_2$Year >= 2000, c("Admin_Unit_Name", "Year", "HMIS_treat")])
  n_countries <- length(unique(Countries_HMIS$Admin_Unit_Name))
  Region <- trun_version[trun_version$IHME_Region_name == ssa.list[k], ]
  
  plot.title.k <- ssa.list[k]
  
  p9a <- ggplot(data = Region, aes(x = Year, y = HMIS_pred)) + geom_line() + geom_ribbon(data=Region,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle(plot.title.k)  + ylim(0, 1) 
  p9b <- p9a + geom_point(data = Countries_HMIS, aes(x = Year, y = HMIS_treat, group = Admin_Unit_Name, colour = Admin_Unit_Name))
  p9b <- p9b + scale_colour_discrete(name = "") + theme(legend.position="bottom", plot.title = element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=8))
  
  plot_list_HMIS[[k]] <- p9b 
  
}

pdf(paste(graphics.path, 'HMIS_Regional_Averages_SSA_from_2000.pdf'),width=10,height = 10)

grid.arrange(p9, plot_list_HMIS[[1]], plot_list_HMIS[[2]], plot_list_HMIS[[3]], plot_list_HMIS[[4]], layout_matrix = rbind(c(1, 1), c(2, 3), c(4, 5)))

dev.off()

# ------------ Plots without points for main paper:


pdf(paste(graphics.path, 'Regional_Averages_plus_SSA_Any.pdf'),width=11,height = 10)

grid.arrange(p3, p8, nrow = 2)

dev.off()

pdf(paste(graphics.path, 'Regional_Averages_plus_SSA_HMIS.pdf'),width=11,height = 10)

grid.arrange(p4, p9, nrow = 2)

dev.off()

# Compute global private treatment-seeking rates in 2000 and 2019:

global_mean_realisations <- data.frame("Year" = years)


global_aggreg <- function(pred_realisations = Any_pred_realisations, covariates = FALSE, cov.col = "ANC1_coverage_prop"){
  
  if(covariates){
    realisation_col <- cov.col
  }else{realisation_col <- as.character(1:10000)}
  
  pop_error <- c()
  
  # No populations (esp for the MAP infants category) for Cook Islands, Monaco, Nauru, Niue, Palau, Saint Kitts and Nevis, San Marino, Tokelau and Tuvalu.
  # Currently their regions' aggregation excludes these.
  
  Countries <- pred_realisations[pred_realisations$Admin_Unit_Level == "ADMIN0", c("IHME_location_id", "Year", realisation_col)]
  global_aggreg <- matrix(NA, nrow =length(years), ncol = length(realisation_col))
  for (i in 1:length(years)){ # Region-Year
    year_data <- Countries[Countries$Year == years[i], ]
    year_realisations <- year_data[, realisation_col]
    year_pop <- rep(NA, nrow(year_data))
    for (j in 1:length(year_pop)){ # Country-Year
      if(length(ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]]) > 0){
        year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]][1]
      }else{
        pop_error <- c(pop_error, year_data$IHME_location_id[j])
        print(paste("Country ", j, "/", length(year_pop), ", ", " IHME Population Error.", sep = ""))}
    }
    global_aggreg[i, ] <- colSums(as.matrix(year_realisations, nrow  = length(year_pop), ncol = length(realisation_col))*matrix(rep(year_pop, length(realisation_col)), nrow = length(year_pop), ncol = length(realisation_col)), na.rm = TRUE)/sum(year_pop, na.rm = TRUE)
    print(paste(years[i], ", ", (proc.time()[3] - temp.time)/60, " minutes taken.", sep = ""))
    
  }
  
  pop_error_countries <- unique(full_TreatSeek$Country_Name[full_TreatSeek$IHME_location_id %in% unique(pop_error)])
  return(list("temp_realisations" =  global_aggreg, "pop_error_countries" = pop_error_countries))
}

global_Any_pred_aggreg_list <- global_aggreg(pred_realisations = Any_pred_realisations) # About 40 minutes.
global_Any_pred_aggreg <- global_Any_pred_aggreg_list$temp_realisations
global_Any_pred_aggreg_list$pop_error_countries # Check.

global_HMIS_pred_aggreg_list <- global_aggreg(pred_realisations = HMIS_pred_realisations)
global_HMIS_pred_aggreg <- global_HMIS_pred_aggreg_list$temp_realisations
global_HMIS_pred_aggreg_list$pop_error_countries # Check.

global_Priv_pred_aggreg <- global_Any_pred_aggreg - global_HMIS_pred_aggreg
sum(global_Priv_pred_aggreg<0) # 0.
summary(as.vector(global_Priv_pred_aggreg))

global_Priv_prop_aggreg <- (global_Any_pred_aggreg - global_HMIS_pred_aggreg)/global_Any_pred_aggreg
sum(global_Priv_pred_aggreg<0) # 0.
sum(global_Priv_pred_aggreg>1) # 0.
summary(as.vector(global_Priv_pred_aggreg))

global_mean_realisations$Any_pred <- rowMeans(global_Any_pred_aggreg)
global_mean_realisations$Any_pred_low <- apply(global_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
global_mean_realisations$Any_pred_high <- apply(global_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

global_mean_realisations$HMIS_pred <- rowMeans(global_HMIS_pred_aggreg)
global_mean_realisations$HMIS_pred_low <- apply(global_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
global_mean_realisations$HMIS_pred_high <- apply(global_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

global_mean_realisations$Priv_pred <- rowMeans(global_Priv_pred_aggreg)
global_mean_realisations$Priv_pred_low <- apply(global_Priv_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
global_mean_realisations$Priv_pred_high <- apply(global_Priv_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

global_mean_realisations$Priv_prop <- rowMeans(global_Priv_prop_aggreg)
global_mean_realisations$Priv_prop_low <- apply(global_Priv_prop_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
global_mean_realisations$Priv_prop_high <- apply(global_Priv_prop_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

head(global_mean_realisations)

global_mean_realisations[, c("Year", "Priv_prop")]

write.csv(global_mean_realisations,  file = paste(data.path, 'global_mean_realisations.csv', sep = ''))

# ========================== Statistics for paper:

survey_list <- unique(clean_TreatSeek_Any[clean_TreatSeek_Any$Year>=1990, c("ISO3", "Year", "IHME_Super_Region_Name", "IHME_Region_Name")])
nrow(survey_list)
length(unique(survey_list$ISO3))
table(survey_list$IHME_Super_Region_Name)
