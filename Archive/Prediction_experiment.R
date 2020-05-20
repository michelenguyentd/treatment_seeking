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
load('Z:/GBD2019/Processing/Static_Covariates/IHME/IHME_Subnational_Covariates_GBD2019/covariates_for_treatment_seeking/IHME_covar_decomp2.RData')
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

# Remove Brazil 2016 data point from training set:
train_data_Any <- train_data_Any[-which(train_data_Any$ISO3 == "BRA" & train_data_Any$Year == 1996), ]
train_data_HMISfrac <- train_data_HMISfrac[-which(train_data_HMISfrac$ISO3 == "BRA" & train_data_HMISfrac$Year == 1996), ]

# Find units without data:

master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name) # This has grouped Oceania with SE Asia etc. Use full_TreatSeek if want true regions.
master.unit.list <- unique(full_TreatSeek_n$Admin_Unit_Name)
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
  Any_pts <- sum(train_data_Any$Admin_Unit_Name == as.character(master.unit.list[i])) + sum(test_data_Any$Admin_Unit_Name == as.character(master.unit.list[i]))
  HMISfrac_pts <- sum(train_data_HMISfrac$Admin_Unit_Name == as.character(master.unit.list[i])) + sum(test_data_HMISfrac$Admin_Unit_Name == as.character(master.unit.list[i]))
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
clean_TreatSeek_HMISfrac$Reg_Factor_2[!(clean_TreatSeek_HMISfrac$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Southeast Asia", "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa"))] <- 'Other'
clean_TreatSeek_HMISfrac$Reg_Factor_2 <- as.factor(clean_TreatSeek_HMISfrac$Reg_Factor_2)
unique(clean_TreatSeek_HMISfrac$Reg_Factor_2)
clean_TreatSeek_HMISfrac$Reg_Factor_2 <- relevel(clean_TreatSeek_HMISfrac$Reg_Factor_2, ref = 'Other')

full_TreatSeek_n$Reg_Factor_2 <- as.character(full_TreatSeek_n$IHME_Region_Name)
full_TreatSeek_n$Reg_Factor_2[!(full_TreatSeek_n$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Southeast Asia", "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa"))] <- 'Other'
full_TreatSeek_n$Reg_Factor_2 <- as.factor(full_TreatSeek_n$Reg_Factor_2)
unique(full_TreatSeek_n$Reg_Factor_2)
full_TreatSeek_n$Reg_Factor_2 <- relevel(full_TreatSeek_n$Reg_Factor_2, ref = 'Other')

# 1. RTTIR:

# formula_any_1 <- logit_Any ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name, k = 5) + s(accessibility, k = 3) + s(ANC1_coverage_prop, k = 3) + s(GDPpc_id_b2010, k = 3) + s(hospital_beds_per1000, k = 3) + s(VIIRS_nighttime, k = 3)
# formula_hmis_1<-  logit_HMIS ~ -1 + IHME_Region_Name + s(Year, by = IHME_Region_Name, k = 5)  + s(accessibility, k = 3) + s(ANC1_coverage_prop, k = 3) + s(education_all_ages_and_sexes_pc, k = 3) + s(frac_oop_hexp, k = 3) + s(hospital_beds_per1000, k = 3) + s(VIIRS_nighttime, k =3)

# 2. REF:

# formula_any_1 <-  logit_Any ~ s(Year, by = Time_Factor, k = 5) + s(accessibility, k = 3) + s(ANC1_coverage_prop, k = 3) + s(VIIRS_nighttime, k = 3) + s(GDPpc_id_b2010, k = 3)
# formula_hmis_1 <- logit_HMIS ~ s(Year, by = Time_Factor_2, k = 5)  + s(accessibility, k = 3) + s(ANC1_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(log_the_pc, k = 3) + s(prop_urban, k = 3) + s(VIIRS_nighttime, k = 3) + s(frac_oop_hexp, k = 3)

# 3. PREF:

formula_any_1 <-  logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(log_the_pc, k = 3)
formula_hmis_1 <-  logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2, k = 5) + s(ANC1_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(oop_hexp_cap, k = 3) + s(VIIRS_nighttime, k = 3)

# Insert dummy true Year for plotting:
full_TreatSeek_n$t.Year <- full_TreatSeek_n$Year
# Freeze non-linear temporal trend pre 1995:
full_TreatSeek_n$Year[full_TreatSeek_n$Year < 1995] <- 1995
unique(full_TreatSeek_n$Year)

# ------------ 1a. Explore the similarity of covariates for units without data ----------

full_TreatSeek_n$rf.unit <- full_TreatSeek_n$Admin_Unit_Name
full_TreatSeek_n$rf.unit.pf <- full_TreatSeek_n$Admin_Unit_Name
full_TreatSeek_n$trend.unit <- full_TreatSeek_n$Admin_Unit_Name
full_TreatSeek_n$trend.unit.pf <- full_TreatSeek_n$Admin_Unit_Name


ref_year_1 <- 2000; ref_year_2 <- 2010; ref_year_3 <- 2019

for (j in 1:length(master.superregion.list)){

  # Use ADMIN0 with data for matching with ADMIN0 and ADMIN1 without data:
  temp_region_cov <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == as.character(master.superregion.list[j]), ]

  # a) Trend: Use changes between dynamic covariates for years 2000-2010 and 2010-2019.

  t_Any_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$Admin_Unit_Name %in% c(units_no_Any_data, units_1_Any_data)), ]
  t_Any_region_cov_1 <- t_Any_region_cov[t_Any_region_cov$t.Year == ref_year_1, ]
  t_Any_region_cov_2 <- t_Any_region_cov[t_Any_region_cov$t.Year == ref_year_2, ]
  t_Any_region_cov_3 <- t_Any_region_cov[t_Any_region_cov$t.Year == ref_year_3, ]
  t_Any_region_no_data <- unique(t_Any_region_cov$Admin_Unit_Name[t_Any_region_cov$Admin_Unit_Name %in% c(units_no_Any_data, units_1_Any_data)])

  t_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$Admin_Unit_Name %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)), ]
  t_HMISfrac_region_cov_1 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$t.Year == ref_year_1, ]
  t_HMISfrac_region_cov_2 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$t.Year == ref_year_2, ]
  t_HMISfrac_region_cov_3 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$t.Year == ref_year_3, ]
  t_HMISfrac_region_no_data <- unique(t_HMISfrac_region_cov$Admin_Unit_Name[t_HMISfrac_region_cov$Admin_Unit_Name %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)])

  # Omit "DMSP_nighttime" because static.
  temp_df1 <- cbind(t_Any_region_cov_2[, c("ANC1_coverage_prop", "log_the_pc")] - t_Any_region_cov_1[, c("ANC1_coverage_prop", "log_the_pc")], t_Any_region_cov_3[, c("ANC1_coverage_prop", "log_the_pc")] - t_Any_region_cov_2[, c("ANC1_coverage_prop", "log_the_pc")])
  region.pca1 <- prcomp(temp_df1, center = TRUE, scale. = TRUE)

  query_df1 <- region.pca1$x[t_Any_region_cov_1$Admin_Unit_Name %in% c(units_no_Any_data, units_1_Any_data), 1:2]
  ref_df1 <- region.pca1$x[!(t_Any_region_cov_1$Admin_Unit_Name %in% c(units_no_Any_data, units_1_Any_data)), 1:2]

  # Omit "VIIRS_nighttime" because static.
  temp_df2 <- cbind(t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "oop_hexp_cap")] - t_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "hospital_beds_per1000",  "oop_hexp_cap")], t_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "hospital_beds_per1000", "oop_hexp_cap")] - t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "oop_hexp_cap")])
  region.pca2 <- prcomp(temp_df2, center = TRUE, scale. = TRUE)

  query_df2 <- region.pca2$x[t_HMISfrac_region_cov_1$Admin_Unit_Name %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data), 1:2]
  ref_df2 <- region.pca2$x[!(t_HMISfrac_region_cov_1$Admin_Unit_Name %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)), 1:2]

  # b) Random effect: Use significant covariates for years 1980, 2000 and 2019.

  rf_Any_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$Admin_Unit_Name %in% units_no_Any_data), ]
  rf_Any_region_cov_1 <- rf_Any_region_cov[rf_Any_region_cov$t.Year == ref_year_1, ]
  rf_Any_region_cov_2 <- rf_Any_region_cov[rf_Any_region_cov$t.Year == ref_year_2, ]
  rf_Any_region_cov_3 <- rf_Any_region_cov[rf_Any_region_cov$t.Year == ref_year_3, ]
  rf_Any_region_no_data <- unique(rf_Any_region_cov$Admin_Unit_Name[rf_Any_region_cov$Admin_Unit_Name %in% units_no_Any_data])

  rf_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$Admin_Unit_Name %in% units_no_HMISfrac_data), ]
  rf_HMISfrac_region_cov_1 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$t.Year == ref_year_1, ]
  rf_HMISfrac_region_cov_2 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$t.Year == ref_year_2, ]
  rf_HMISfrac_region_cov_3 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$t.Year == ref_year_3, ]
  rf_HMISfrac_region_no_data <- unique(rf_HMISfrac_region_cov$Admin_Unit_Name[rf_HMISfrac_region_cov$Admin_Unit_Name %in% units_no_HMISfrac_data])


  temp_df3 <- cbind(rf_Any_region_cov_1[, c("ANC1_coverage_prop", "DMSP_nighttime", "log_the_pc")], rf_Any_region_cov_2[, c("ANC1_coverage_prop", "log_the_pc")], rf_Any_region_cov_3[, c("ANC1_coverage_prop", "log_the_pc")])
  region.pca3 <- prcomp(temp_df3, center = TRUE, scale. = TRUE)

  query_df3 <- region.pca3$x[rf_Any_region_cov_1$Admin_Unit_Name %in% units_no_Any_data, 1:2]
  ref_df3 <- region.pca3$x[!(rf_Any_region_cov_1$Admin_Unit_Name %in% units_no_Any_data), 1:2]

  temp_df4 <- cbind(rf_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "oop_hexp_cap")], rf_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000",  "oop_hexp_cap")], rf_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "hospital_beds_per1000", "oop_hexp_cap")])
  region.pca4 <- prcomp(temp_df4, center = TRUE, scale. = TRUE)

  query_df4 <- region.pca4$x[rf_HMISfrac_region_cov_1$Admin_Unit_Name %in% units_no_HMISfrac_data, 1:2]
  ref_df4 <- region.pca4$x[!(rf_HMISfrac_region_cov_1$Admin_Unit_Name %in% units_no_HMISfrac_data), 1:2]

  ref_class_1 <- t_Any_region_cov_1[!(t_Any_region_cov_1$Admin_Unit_Name %in% t_Any_region_no_data), c("Admin_Unit_Name")]
  ref_class_2 <- t_HMISfrac_region_cov_1[!(t_HMISfrac_region_cov_1$Admin_Unit_Name %in% t_HMISfrac_region_no_data), c("Admin_Unit_Name")]
  ref_class_3 <- rf_Any_region_cov_1[!(rf_Any_region_cov_1$Admin_Unit_Name %in% rf_Any_region_no_data), c("Admin_Unit_Name")]
  ref_class_4 <- rf_HMISfrac_region_cov_1[!(rf_HMISfrac_region_cov_1$Admin_Unit_Name %in% rf_HMISfrac_region_no_data), c("Admin_Unit_Name")]

  neighbour_result_t_any <- knn1(test = query_df1, train = ref_df1, cl = ref_class_1)
  neighbour_result_t_pf <- knn1(test = query_df2, train = ref_df2, cl = ref_class_2)
  neighbour_result_rf_any <- knn1(test = query_df3, train = ref_df3, cl = ref_class_3)
  neighbour_result_rf_pf <- knn1(test = query_df4, train = ref_df4, cl = ref_class_4)

  for (i in 1:length(t_Any_region_no_data)){
    full_TreatSeek_n$trend.unit[full_TreatSeek_n$Admin_Unit_Name == t_Any_region_no_data[i]] <- neighbour_result_t_any[i]
  }
  for (i in 1:length(t_HMISfrac_region_no_data)){
    full_TreatSeek_n$trend.unit.pf[full_TreatSeek_n$Admin_Unit_Name == t_HMISfrac_region_no_data[i]] <- neighbour_result_t_pf[i]
  }
  for (i in 1:length(rf_Any_region_no_data)){
    full_TreatSeek_n$rf.unit[full_TreatSeek_n$Admin_Unit_Name == rf_Any_region_no_data[i]] <- neighbour_result_rf_any[i]
  }
  for (i in 1:length(rf_HMISfrac_region_no_data)){
         full_TreatSeek_n$rf.unit.pf[full_TreatSeek_n$Admin_Unit_Name == rf_HMISfrac_region_no_data[i]] <- neighbour_result_rf_pf[i]
  }

  pdf(paste(graphics.path, master.superregion.list[j], '.trend.covariates.PCA.pdf', sep = ""),width=12, height = 6)

  par(mfrow=c(1,2))

  plot(region.pca1$x[, 1:2], col = "white", main = "Any Treatment Seeking", xlim = c(min(region.pca1$x[, 1])*1.5, max(region.pca1$x[, 1])*1.5), ylim = c(c(min(region.pca1$x[, 2])*1.5, max(region.pca1$x[, 2])*1.5)))
  text(region.pca1$x[, 1:2], labels = t_Any_region_cov_1$Admin_Unit_Name, col = ifelse(t_Any_region_cov_1$Admin_Unit_Name %in% t_Any_region_no_data, "blue", "black"), cex = 0.7)

  plot(region.pca2$x[, 1:2], col = "white", main = "Public Fractions", xlim = c(min(region.pca2$x[, 1])*1.5, max(region.pca2$x[, 1])*1.5), ylim = c(c(min(region.pca2$x[, 2])*1.5, max(region.pca2$x[, 2])*1.5)))
  text(region.pca2$x[, 1:2], labels = t_HMISfrac_region_cov_1$Admin_Unit_Name, col = ifelse(t_HMISfrac_region_cov_1$Admin_Unit_Name %in% t_HMISfrac_region_no_data, "blue", "black"), cex = 0.7)

  dev.off()

  pdf(paste(graphics.path, master.superregion.list[j], '.rf.covariates.PCA.pdf', sep = ""),width=12, height = 6)

  par(mfrow=c(1,2))

  plot(region.pca3$x[, 1:2], col = "white", main = "Any Treatment Seeking", xlim = c(min(region.pca3$x[, 1])*1.5, max(region.pca3$x[, 1])*1.5), ylim = c(c(min(region.pca3$x[, 2])*1.5, max(region.pca3$x[, 2])*1.5)))
  text(region.pca3$x[, 1:2], labels = rf_Any_region_cov_1$Admin_Unit_Name, col = ifelse(rf_Any_region_cov_1$Admin_Unit_Name %in% rf_Any_region_no_data, "blue", "black"), cex = 0.7)

  plot(region.pca4$x[, 1:2], col = "white", main = "Public Fractions", xlim = c(min(region.pca4$x[, 1])*1.5, max(region.pca4$x[, 1])*1.5), ylim = c(c(min(region.pca4$x[, 2])*1.5, max(region.pca4$x[, 2])*1.5)))
  text(region.pca4$x[, 1:2], labels = rf_HMISfrac_region_cov_1$Admin_Unit_Name, col = ifelse(rf_HMISfrac_region_cov_1$Admin_Unit_Name %in% rf_HMISfrac_region_no_data, "blue", "black"), cex = 0.7)

  dev.off()

}

# Check:
sum(full_TreatSeek_n$rf.unit %in% units_no_Any_data)
sum(full_TreatSeek_n$rf.unit.pf %in% units_no_HMISfrac_data)

NN_results <- full_TreatSeek_n[full_TreatSeek_n$Admin_Unit_Name %in% c(units_no_Any_data, units_no_HMISfrac_data, units_1_Any_data, units_1_HMISfrac_data) & full_TreatSeek_n$Year == 2019, c("ISO3", "Country_Name", "Admin_Unit_Name", "IHME_Super_Region_Name", "IHME_Region_Name", "trend.unit", "trend.unit.pf", "rf.unit", "rf.unit.pf")]
write.csv(NN_results, file = paste(data.path, "trend_rf_NN.csv", sep = ""))

# Check Brazil and China random effects:

NN_results[NN_results$ISO3 == "CHN", ]
NN_results[NN_results$ISO3 == "BRA", ]

# Implement matched trends:

switch_units <- which(full_TreatSeek_n$Admin_Unit_Name != full_TreatSeek_n$trend.unit)
for (i in switch_units){
  full_TreatSeek_n$Time_Factor[i] <-  unique(full_TreatSeek_n$Time_Factor[full_TreatSeek_n$Admin_Unit_Name == full_TreatSeek_n$trend.unit[i]])
}

switch_units_2 <- which(full_TreatSeek_n$Admin_Unit_Name != full_TreatSeek_n$trend.unit.pf)
for (i in switch_units_2){
  full_TreatSeek_n$Time_Factor_2[i] <-  unique(full_TreatSeek_n$Time_Factor_2[full_TreatSeek_n$Admin_Unit_Name == full_TreatSeek_n$trend.unit.pf[i]])
}

# ----------------------------------------------- Fit the models...

# Reset if not using matched random effects:
# full_TreatSeek_n$rf.unit <- full_TreatSeek_n$Admin_Unit_Name
# full_TreatSeek_n$rf.unit.pf <- full_TreatSeek_n$Admin_Unit_Name


# Apply data outliering and post-1990 restriction:

Any_outliers <- which((clean_TreatSeek_Any$ISO3 == "BRA" & clean_TreatSeek_Any$Year == 1996) | (clean_TreatSeek_Any$ISO3 == "IND" & clean_TreatSeek_Any$Year == 1993) | (clean_TreatSeek_Any$ISO3 == "PAK" & clean_TreatSeek_Any$Year == 1991) | (clean_TreatSeek_Any$ISO3 == "NGA" & clean_TreatSeek_Any$Year == 2008)) # | (clean_TreatSeek_Any$ISO3 == "PHL" & clean_TreatSeek_Any$Year == 1993))
HMIS_outliers <- which((clean_TreatSeek_Any$ISO3 == "BRA" & clean_TreatSeek_Any$Year == 1996) | (clean_TreatSeek_HMISfrac$ISO3 == "IND" & clean_TreatSeek_HMISfrac$Year == 1993) | (clean_TreatSeek_HMISfrac$ISO3 == "PAK" & clean_TreatSeek_HMISfrac$Year == 1991) | (clean_TreatSeek_HMISfrac$ISO3 == "NGA" & clean_TreatSeek_HMISfrac$Year == 2008)) # | (clean_TreatSeek_Any$ISO3 == "PHL" & clean_TreatSeek_Any$Year == 1993))

Any_data <- clean_TreatSeek_Any[-Any_outliers, ]
HMIS_data <- clean_TreatSeek_HMISfrac[-HMIS_outliers, ]

Any_data <- Any_data[Any_data$Year > 1989, ]
HMIS_data <- HMIS_data[HMIS_data$Year > 1989, ]

Any_data$Admin_Unit_Name <- as.factor(Any_data$Admin_Unit_Name)
HMIS_data$Admin_Unit_Name <- as.factor(HMIS_data$Admin_Unit_Name)

# 1. RTTIR:

# Any_model <-  gam(formula_any_1, data = Any_data)
# HMIS_model <- gam(formula_hmis_1, data = HMIS_data)

# 1a. PREF:

# Any_model <-  gamm(formula_any_1, data = Any_data, random = list(Admin_Unit_Name = ~ 1))
# HMIS_model <- gamm(formula_hmis_1, data = HMIS_data, random = list(Admin_Unit_Name = ~ 1))

# 2. REF:

# Any_model <-  gamm(formula_any_1, data = Any_data, random = list(IHME_Region_Name = ~ 1, Admin_Unit_Name = ~ 1))
# HMIS_model <- gamm(formula_hmis_1, data = HMIS_data, random = list(IHME_Region_Name = ~ 1, Admin_Unit_Name = ~ 1))

# 3. PREF:
Any_model <-  gamm(formula_any_1, data = Any_data, random = list(Admin_Unit_Name = ~ 1)) #, control = list(niterEM=1, opt='optim', maxit = 500))
HMIS_model <- gamm(formula_hmis_1, data = HMIS_data, random = list(Admin_Unit_Name = ~ 1)) #, control = list(niterEM=1, opt='optim', maxit = 500))

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
  HMIS_rf[full_TreatSeek_n$rf.unit.pf == as.character(units_with_HMISfrac_data[i])] <- ifelse(is.na(ranef(HMIS_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ]), 0, ranef(HMIS_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ])
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
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')

      plotCI(years,unit.row$Any_pred_initial,ui=unit.row$Any_pred_initial_high,li=unit.row$Any_pred_initial_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i])

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
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers who sought treatment went to public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred_initial/unit.row$Any_pred_initial,ui=unit.row$HMIS_pred_initial_high/unit.row$Any_pred_initial_high,li=unit.row$HMIS_pred_initial_low/unit.row$Any_pred_initial_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

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
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment at public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred_initial,ui=unit.row$HMIS_pred_initial_high,li=unit.row$HMIS_pred_initial_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

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
    dataset$Time_Factor[i] <-  unique(full_TreatSeek_n$Time_Factor[full_TreatSeek_n$Admin_Unit_Name == full_TreatSeek_n$trend.unit[i]])
  }

  for (i in switch_units_2){
    dataset$Time_Factor_2[i] <-  unique(full_TreatSeek_n$Time_Factor_2[full_TreatSeek_n$Admin_Unit_Name == full_TreatSeek_n$trend.unit.pf[i]])
  }

  # For PTTFR onwards (coarser regional factors):

  dataset$Reg_Factor <- as.character(dataset$IHME_Region_Name)
  dataset$Reg_Factor[!(dataset$IHME_Region_Name %in% c('Southeast Asia','South Asia', "Central Asia", 'Eastern Sub-Saharan Africa', 'Western Sub-Saharan Africa'))] <- 'Other'
  dataset$Reg_Factor <- as.factor(dataset$Reg_Factor)
  unique(dataset$Reg_Factor)
  dataset$Reg_Factor <- relevel(dataset$Reg_Factor, ref = 'Other')

  dataset$Reg_Factor_2 <- as.character(dataset$IHME_Region_Name)
  dataset$Reg_Factor_2[!(dataset$IHME_Region_Name %in% c('Central Asia', 'South Asia', "Southeast Asia", "Eastern Sub-Saharan Africa", "Western Sub-Saharan Africa"))] <- 'Other'
  dataset$Reg_Factor_2 <- as.factor(dataset$Reg_Factor_2)
  unique(dataset$Reg_Factor_2)
  dataset$Reg_Factor_2 <- relevel(dataset$Reg_Factor_2, ref = 'Other')

  dataset$IHME_Region_Name  <- as.factor(dataset$IHME_Region_Name)

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

  Any_fit <- try(gamm(formula_any_1, data = Any_data, random = list(Admin_Unit_Name = ~ 1)))
  if("try-error" %in% class(Any_fit)){
    Any_fit <-  gamm(formula_any_1, data = Any_data, random = list(Admin_Unit_Name = ~ 1), control = list(niterEM=1, opt='optim', maxit = 500))
  }

  HMIS_fit <- try(gamm(formula_hmis_1, data = HMIS_data, random = list(Admin_Unit_Name = ~ 1)))
  if("try-error" %in% class(HMIS_fit)){
    HMIS_fit <- gamm(formula_hmis_1, data = HMIS_data, random = list(Admin_Unit_Name = ~ 1), control = list(niterEM=1, opt='optim', maxit = 500))
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
    HMIS_rf[full_TreatSeek_n$rf.unit.pf == as.character(units_with_HMISfrac_data[i])] <- ifelse(is.na(ranef(HMIS_fit$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ]), 0, ranef(HMIS_fit$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ])
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
  }else{print(paste("Dataset ", TS_j, ": Random effect matching error.", sep = ""))}

}

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


pdf(paste(graphics.path, 'Any_Treat.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')

      plotCI(years,unit.row$Any_pred,ui=unit.row$Any_pred_high,li=unit.row$Any_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i])

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
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers who sought treatment went to public facilities',xlab='Year')

      plotCI(years,unit.row$HMISfrac_pred,ui=unit.row$HMISfrac_pred_high,li=unit.row$HMISfrac_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

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
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment at public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred,ui=unit.row$HMIS_pred_high,li=unit.row$HMIS_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

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
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
      }
    }
  }
}

dev.off()

# full_TreatSeek_n <- full_TreatSeek_original

#  Save the results:

write.csv(full_TreatSeek_n, file = paste(data.path, 'TS_predictions.csv', sep = '')) # For Option 1: With the uncertainty in the model fit considered.
# write.csv(full_TreatSeek_n, file = paste(data.path, 'TS_predictions_Option2.csv', sep = '')) # For Option 2: Without the uncertainty in the model fit.













# ------------ 2b. Aggregate subnationals based on national realisations...

# full_TreatSeek_aggreg <- read.csv(file = paste(data.path, 'TS_predictions_aggreg.csv', sep = ''))

Any_pred_realisations <- cbind(full_TreatSeek[, c("ISO2", "ISO3", "GAUL_Code", "Country_Name", "Admin_Unit_Level", "Admin_Unit_Name", "WHO_Region", "WHO_Subregion", "IHME_location_id", "IHME_Super_Region_ID", "IHME_Super_Region_Name", "IHME_Region_ID", "IHME_Region_Name", "Year")], full_Any_pred_raw)
HMIS_pred_realisations <- cbind(full_TreatSeek[, c("ISO2", "ISO3", "GAUL_Code", "Country_Name", "Admin_Unit_Level", "Admin_Unit_Name", "WHO_Region", "WHO_Subregion", "IHME_location_id", "IHME_Super_Region_ID", "IHME_Super_Region_Name", "IHME_Region_ID", "IHME_Region_Name", "Year")], full_Any_pred_raw*full_HMIS_pred_raw)

# Create aggregation function to use subnational realisations for the iso3 country in pred.realisations and the ihme infants population to obtain national realisations:

national_aggreg <- function(iso3 = "BRA", pred.realisations = Any_pred_realisations){
  Country <- pred.realisations[pred.realisations$ISO3 == iso3, c("Admin_Unit_Level", "IHME_location_id", "Year", as.character(1:10000))]
  Country_ADMIN0 <- Country[Country$Admin_Unit_Level == "ADMIN0", ]
  Country_ADMIN1_aggreg <- matrix(NA, nrow = nrow(Country_ADMIN0), ncol = 100000)
  Country_ADMIN1_data <- Country[Country$Admin_Unit_Level == "ADMIN1",]

  for (i in 1:length(years)){
    year_data <- Country_ADMIN1_data[Country_ADMIN1_data$Year == years[i], ]
    year_realisations <- year_data[, as.character(1:10000)]
    year_pop <- rep(NA, nrow(year_data))
    for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]]}
    Country_ADMIN1_aggreg[i, ] <- colSums(year_realisations*matrix(rep(year_pop, 10000), nrow = length(year_pop), ncol = 10000))/sum(year_pop)
  }
  return(Country_ADMIN1_aggreg)
}

# 1. Brazil:
Brazil_Any_aggreg <- national_aggreg(iso3 = "BRA", pred.realisations = Any_pred_realisations)
Brazil_HMIS_aggreg <- national_aggreg(iso3 = "BRA", pred.realisations = HMIS_pred_realisations)

# 2. China:
China_Any_aggreg <- national_aggreg(iso3 = "CHN", pred.realisations = Any_pred_realisations)
China_HMIS_aggreg <- national_aggreg(iso3 = "CHN", pred.realisations = HMIS_pred_realisations)

# 3. Ethiopia:
Ethiopia_Any_aggreg <- national_aggreg(iso3 = "ETH", pred.realisations = Any_pred_realisations)
Ethiopia_HMIS_aggreg <- national_aggreg(iso3 = "ETH", pred.realisations = HMIS_pred_realisations)

# 4. Indonesia:
Indonesia_Any_aggreg <- national_aggreg(iso3 = "IDN", pred.realisations = Any_pred_realisations)
Indonesia_HMIS_aggreg <- national_aggreg(iso3 = "IDN", pred.realisations = HMIS_pred_realisations)

# 5. India  - skip because aggregation is done on the case counts and we don't have populations for SMT individually.

# 6. Iran:
Iran_Any_aggreg <- national_aggreg(iso3 = "IRN", pred.realisations = Any_pred_realisations)
Iran_HMIS_aggreg <- national_aggreg(iso3 = "IRN", pred.realisations = HMIS_pred_realisations)

# 7. Mexico:
Mexico_Any_aggreg <- national_aggreg(iso3 = "MEX", pred.realisations = Any_pred_realisations)
Mexico_HMIS_aggreg <- national_aggreg(iso3 = "MEX", pred.realisations = HMIS_pred_realisations)

# 8. Nigeria:
Nigeria_Any_aggreg <- national_aggreg(iso3 = "NGA", pred.realisations = Any_pred_realisations)
Nigeria_HMIS_aggreg <- national_aggreg(iso3 = "NGA", pred.realisations = HMIS_pred_realisations)

# 9. Pakistan:
Pakistan_Any_aggreg <- national_aggreg(iso3 = "PAK", pred.realisations = Any_pred_realisations)
Pakistan_HMIS_aggreg <- national_aggreg(iso3 = "PAK", pred.realisations = HMIS_pred_realisations)

# 10. Philippines:
Philippines_Any_aggreg <- national_aggreg(iso3 = "PHL", pred.realisations = Any_pred_realisations)
Philippines_HMIS_aggreg <- national_aggreg(iso3 = "PHL", pred.realisations = HMIS_pred_realisations)

# 11. SouthAfrica:
SouthAfrica_Any_aggreg <- national_aggreg(iso3 = "ZAF", pred.realisations = Any_pred_realisations)
SouthAfrica_HMIS_aggreg <- national_aggreg(iso3 = "ZAF", pred.realisations = HMIS_pred_realisations)

# Replace national realisations:

Any_pred_realisations[Any_pred_realisations$ISO3 == "BRA" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Brazil_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "BRA"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Brazil_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "CHN" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- China_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "CHN"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- China_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "ETH" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Ethiopia_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "ETH"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Ethiopia_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "IDN" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Indonesia_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "IDN"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Indonesia_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "IRN" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Iran_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "IRN"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Iran_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "NGA" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Nigeria_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "NGA"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Nigeria_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "MEX" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Mexico_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "MEX"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Mexico_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "PAK" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Pakistan_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "PAK"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Pakistan_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "PHL" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Philippines_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "PHL"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- Philippines_HMIS_aggreg

Any_pred_realisations[Any_pred_realisations$ISO3 == "ZAF" & Any_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- SouthAfrica_Any_aggreg
HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "ZAF"& HMIS_pred_realisations$Admin_Unit_Level == "ADMIN0", as.character(1:10000)] <- SouthAfrica_HMIS_aggreg

full_TreatSeek_aggreg <- full_TreatSeek_n

full_Any_pred_aggreg <- Any_pred_realisations[, as.character(1:10000)]
full_HMIS_pred_aggreg <- HMIS_pred_realisations[, as.character(1:10000)]

# Compute mean and 95% confidence intervals:

full_TreatSeek_aggreg$Any_pred <- rowMeans(full_Any_pred_aggreg)
full_TreatSeek_aggreg$Any_pred_low <- apply(full_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_aggreg$Any_pred_high <- apply(full_Any_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

full_TreatSeek_aggreg$HMIS_pred <- rowMeans(full_HMIS_pred_aggreg)
full_TreatSeek_aggreg$HMIS_pred_low <- apply(full_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_aggreg$HMIS_pred_high <- apply(full_HMIS_pred_aggreg, MARGIN = 1, function(x){quantile(x, probs = 0.975)})

# Reproportion Brazil subnationals based on Brazil 1996 datapoint:
# reprop_fraction <- (full_TreatSeek_aggreg$Any_treat[full_TreatSeek_aggreg$Admin_Unit_Name == "Brazil" & full_TreatSeek_aggreg$t.Year == 1996])/(full_TreatSeek_aggreg$Any_pred[full_TreatSeek_aggreg$Admin_Unit_Name == "Brazil" & full_TreatSeek_aggreg$t.Year == 1996])
# Any_pred_realisations[Any_pred_realisations$ISO3 == "BRA", as.character(1:10000)] <- Any_pred_realisations[Any_pred_realisations$ISO3 == "BRA", as.character(1:10000)]*reprop_fraction
# HMIS_pred_realisations[Any_pred_realisations$ISO3 == "BRA", as.character(1:10000)] <- HMIS_pred_realisations[HMIS_pred_realisations$ISO3 == "BRA", as.character(1:10000)]*reprop_fraction

full_Any_pred_aggreg <- Any_pred_realisations[, as.character(1:10000)]
full_HMIS_pred_aggreg <- HMIS_pred_realisations[, as.character(1:10000)]

# Recompute mean and 95% confidence intervals:

full_TreatSeek_aggreg$Any_pred[full_TreatSeek_aggreg$ISO3 == "BRA"] <- rowMeans(full_Any_pred_aggreg[full_TreatSeek_aggreg$ISO3 == "BRA", ])
full_TreatSeek_aggreg$Any_pred_low[full_TreatSeek_aggreg$ISO3 == "BRA"] <- apply(full_Any_pred_aggreg[full_TreatSeek_aggreg$ISO3 == "BRA", ], MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_aggreg$Any_pred_high[full_TreatSeek_aggreg$ISO3 == "BRA"] <- apply(full_Any_pred_aggreg[full_TreatSeek_aggreg$ISO3 == "BRA", ], MARGIN = 1, function(x){quantile(x, probs = 0.975)})

full_TreatSeek_aggreg$HMIS_pred[full_TreatSeek_aggreg$ISO3 == "BRA"] <- rowMeans(full_HMIS_pred_aggreg[full_TreatSeek_aggreg$ISO3 == "BRA", ])
full_TreatSeek_aggreg$HMIS_pred_low[full_TreatSeek_aggreg$ISO3 == "BRA"] <- apply(full_HMIS_pred_aggreg[full_TreatSeek_aggreg$ISO3 == "BRA", ], MARGIN = 1, function(x){quantile(x, probs = 0.025)})
full_TreatSeek_aggreg$HMIS_pred_high[full_TreatSeek_aggreg$ISO3 == "BRA"] <- apply(full_HMIS_pred_aggreg[full_TreatSeek_aggreg$ISO3 == "BRA", ], MARGIN = 1, function(x){quantile(x, probs = 0.975)})

pdf(paste(graphics.path, 'Any_Treat_aggreg.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_aggreg[full_TreatSeek_aggreg$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')

      plotCI(years,unit.row$Any_pred,ui=unit.row$Any_pred_high,li=unit.row$Any_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i])

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


pdf(paste(graphics.path, 'HMIS_Treat_aggreg.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.region.list)){
  region.data <- full_TreatSeek_aggreg[full_TreatSeek_aggreg$IHME_Region_Name == master.region.list[j], ]
  countries <- unique(region.data$ISO3)
  for (i in 1:length(countries)){
    unit.list <- unique(region.data$Admin_Unit_Name[region.data$ISO3 == countries[i]])
    for (k in 1:length(unit.list)){
      unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$Admin_Unit_Name == unit.list[k], ]
      plot(0,0,type='n',ylim=c(0,1),xlim=c(years[1], years[length(years)]),main=paste(master.region.list[j], ': ', unit.list[k] , ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment at public facilities',xlab='Year')

      plotCI(years,unit.row$HMIS_pred,ui=unit.row$HMIS_pred_high,li=unit.row$HMIS_pred_low,ylim=c(0,1),add=T)

      in_out<-which(clean_TreatSeek_HMISfrac$Admin_Unit_Name == unit.list[k] & as.character(clean_TreatSeek_HMISfrac$ISO3) == countries[i])

      if(length(in_out)>0){

        country_line<- clean_TreatSeek_HMISfrac[in_out,]

        points.col <- ifelse(country_line$Year>1989, "red", "blue")
        if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
        if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
        if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
        if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}

        plotCI(country_line$Year, country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
      }
      if (countries[i] == 'AFG'){
        legend('topright',legend=c('DHS','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
      }
    }
  }
}

dev.off()

#  Save the results:

write.csv(full_TreatSeek_aggreg, file = paste(data.path, 'TS_predictions_aggreg.csv', sep = ''))

# Rogue points:
# write.csv(na.omit(clean_TreatSeek_Any[clean_TreatSeek_Any$Report_Treat > clean_TreatSeek_Any$Any_treat, c("Country_Name", "Admin_Unit_Name", "Year", "HMIS_treat", "Any_treat", "Report_Treat")]), file = paste(data.path, "Rogue_Any_Treat.csv", sep = ""))

# ------------ 2d. Calculate superregion treatment seeking rates:

# Set Super Regions of GUF and MYT to Latin America and Carribean and Sub-Saharan Africa (i.e. that of Suriname and Comoros respectively):

full_TreatSeek$IHME_Super_Region_Name[full_TreatSeek$ISO3 == "GUF"] <- "Latin America and Caribbean"
full_TreatSeek$IHME_Super_Region_Name[full_TreatSeek$ISO3 == "MYT"] <- "Sub-Saharan Africa"

master.superregion.list <- unique(full_TreatSeek$IHME_Super_Region_Name)
# master.superregion.list <- master.superregion.list[master.superregion.list != "High-income"] # Omit High-Income because do not have IHME populations for Monaco and not of interest?

# superregion_units <- unique(data.frame("Country_Name" = full_TreatSeek$Country_Name, "IHME_Super_Region_Name" = full_TreatSeek$IHME_Super_Region_Name))

# write.csv(superregion_units, "superregion_units.csv")

superregion_mean_realisations <- data.frame("IHME_Super_Region_name" = rep(master.superregion.list, each = length(years)), "Year" = rep(years, length(master.superregion.list)))

superregion_aggreg <- function(pred_realisations = Any_pred_realisations, covariates = FALSE, cov.col = "ANC1_coverage_prop"){

  temp.time <- proc.time()[3]
  if(covariates){
    realisation_col <- cov.col
  }else{realisation_col <- as.character(1:10000)}

  temp_realisations <- rep(NA, length(realisation_col))
  names(temp_realisations) <- realisation_col
  pop_error <- c()

  # No populations (esp for the MAP infants category) for Cook Islands, Monaco, Nauru, Niue, Palau, Saint Kitts and Nevis, San Marino, Tokelau and Tuvalu.
  # Currently their regions' aggregation excludes these.

  for (k in 1:length(master.superregion.list)){ # Region
    Countries <- pred_realisations[pred_realisations$IHME_Super_Region_Name == master.superregion.list[k] & pred_realisations$Admin_Unit_Level == "ADMIN0", c("IHME_location_id", "Year", realisation_col)]
    Region_aggreg <- matrix(NA, nrow =length(years), ncol = length(realisation_col))
    for (i in 1:length(years)){ # Region-Year
      year_data <- Countries[Countries$Year == years[i], ]
      year_realisations <- year_data[, realisation_col]
      year_pop <- rep(NA, nrow(year_data))
      for (j in 1:length(year_pop)){ # Country-Year
        if(length(ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]]) > 0){
          year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == years[i]][1]
          print(paste("Region ", k, ", ", master.superregion.list[k], ": ", years[i], ", Country ", j, "/", length(year_pop), ", ", (proc.time()[3] - temp.time)/60, " minutes taken.", sep = ""))
        }else{
          pop_error <- c(pop_error, year_data$IHME_location_id[j])
          print(paste("Region ", k, ", ", master.superregion.list[k], ": ", years[i], ", Country ", j, "/", length(year_pop), ", ", " IHME Population Error.", sep = ""))}
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

p1 <- ggplot(data = superregion_mean_realisations, aes(x = Year, y = Any_pred, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + geom_ribbon(data=superregion_mean_realisations,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = IHME_Super_Region_name),alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate")  + ylim(0, 1) + scale_colour_discrete(name = "IHME Super-region")

p2 <- ggplot(data = superregion_mean_realisations, aes(x = Year, y = HMIS_pred, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + geom_ribbon(data=superregion_mean_realisations,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = IHME_Super_Region_name), alpha=0.2, show.legend = FALSE, linetype = 0) + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + ylim(0, 1)+ scale_colour_discrete(name = "IHME Super-region")

pdf(paste(graphics.path, 'Regional_Trends.pdf'),width=12,height = 5)

grid_arrange_shared_legend(p1, p2)

dev.off()

trun_version <- superregion_mean_realisations[superregion_mean_realisations$Year >= 2000, ]

p3 <- ggplot(data = trun_version, aes(x = Year, y = Any_pred, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=Any_pred_low,ymax=Any_pred_high, fill = IHME_Super_Region_name),alpha=0.2, show.legend = FALSE, linetype = 0)   + xlab("Year") + ylab("") + ggtitle("Any treatment seeking rate")  + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1)

p4 <- ggplot(data = trun_version, aes(x = Year, y = HMIS_pred, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + geom_ribbon(data=trun_version,aes(ymin=HMIS_pred_low,ymax=HMIS_pred_high, fill = IHME_Super_Region_name),alpha=0.2, show.legend = FALSE, linetype = 0)   + xlab("Year") + ylab("") + ggtitle("Public treatment seeking rate") + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1)

pdf(paste(graphics.path, 'Regional_Trends_from2000.pdf'),width=12,height = 5)

grid_arrange_shared_legend(p3, p4)

dev.off()

# -----------------------------------------------------------------------------------

# Regionally averaged covariates (uncertainty not included):

temp_covariates <- data.frame(matrix(NA, nrow = nrow(superregion_mean_realisations), ncol = length(cov_names)))
names(temp_covariates) <- cov_names
superregion_mean_realisations <- cbind(superregion_mean_realisations, temp_covariates)

for (i in 1:length(cov_names)){
  temp_input <-  superregion_aggreg(pred_realisations = full_TreatSeek, covariates = TRUE, cov.col = cov_names[i])
  superregion_mean_realisations[, cov_names[i]] <- temp_input$temp_realisations
}

trun_version <- superregion_mean_realisations[superregion_mean_realisations$Year >= 2000, ]

c1 <- ggplot(data = trun_version, aes(x = Year, y = ANC1_coverage_prop, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[1]) + scale_colour_discrete(name = "IHME Super-region")
c2 <- ggplot(data = trun_version, aes(x = Year, y = ANC4_coverage_prop, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[2]) + scale_colour_discrete(name = "IHME Super-region")
c3 <- ggplot(data = trun_version, aes(x = Year, y = DTP3_coverage_prop, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[3]) + scale_colour_discrete(name = "IHME Super-region")
c4 <- ggplot(data = trun_version, aes(x = Year, y = hospital_beds_per1000, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[4]) + scale_colour_discrete(name = "IHME Super-region")
c5 <- ggplot(data = trun_version, aes(x = Year, y = IFD_coverage_prop, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[5]) + scale_colour_discrete(name = "IHME Super-region")
c6 <- ggplot(data = trun_version, aes(x = Year, y = LDI_pc, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[6]) + scale_colour_discrete(name = "IHME Super-region")
c7 <- ggplot(data = trun_version, aes(x = Year, y = measles_vacc_cov_prop, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[7]) + scale_colour_discrete(name = "IHME Super-region")
c8 <- ggplot(data = trun_version, aes(x = Year, y = SBA_coverage_prop, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[8]) + scale_colour_discrete(name = "IHME Super-region")
c9 <- ggplot(data = trun_version, aes(x = Year, y = GDPpc_id_b2010, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[9]) + scale_colour_discrete(name = "IHME Super-region")
c10 <- ggplot(data = trun_version, aes(x = Year, y = prop_urban, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[10]) + scale_colour_discrete(name = "IHME Super-region")
c11 <- ggplot(data = trun_version, aes(x = Year, y = oop_hexp_cap, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[11]) + scale_colour_discrete(name = "IHME Super-region")
c12 <- ggplot(data = trun_version, aes(x = Year, y = frac_oop_hexp, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[12]) + scale_colour_discrete(name = "IHME Super-region")
c13 <- ggplot(data = trun_version, aes(x = Year, y = measles_vacc_cov_prop_2, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[13]) + scale_colour_discrete(name = "IHME Super-region")
c14 <- ggplot(data = trun_version, aes(x = Year, y = ind_health, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[14]) + scale_colour_discrete(name = "IHME Super-region")
c15 <- ggplot(data = trun_version, aes(x = Year, y = education_all_ages_and_sexes_pc, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[15]) + scale_colour_discrete(name = "IHME Super-region")
c16 <- ggplot(data = trun_version, aes(x = Year, y = log_the_pc, group = IHME_Super_Region_name, colour = IHME_Super_Region_name)) + geom_line() + xlab("Year") + ylab("") + ggtitle(cov_names[16]) + scale_colour_discrete(name = "IHME Super-region")

pdf(paste(graphics.path, 'Regional_Covariates_from2000.pdf'),width=12,height = 12)

grid_arrange_shared_legend(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, ncol = 4, nrow = 4)

dev.off()

save(superregion_mean_realisations, file = paste(data.path, "superregion_mean_realisations.RData"))



# ------------ 3. Compare superregions mean trends -----------

# full_TreatSeek_n <- read.csv(file = paste(data.path, 'TS_predictions.csv', sep = ''))
#
# head(full_TreatSeek_n)
#
# summary(full_TreatSeek_n$IHME_Super_Region_Name)
#
# # Set Super Regions of GUF and MYT to Latin America and Carribean and Sub-Saharan Africa (i.e. that of Suriname and Comoros respectively):
#
# full_TreatSeek_n$IHME_Super_Region_Name[full_TreatSeek_n$ISO3 == "GUF"] <- "Latin America and Caribbean"
# full_TreatSeek_n$IHME_Super_Region_Name[full_TreatSeek_n$ISO3 == "MYT"] <- "Sub-Saharan Africa"
#
# # Only use ADMIN0 units:
#
# country_data <- full_TreatSeek_n[full_TreatSeek_n$Admin_Unit_Level == "ADMIN0", ]
#
# superregions <- as.character(country_data$IHME_Super_Region_Name)
#
# country_data$IHME_Super_Region_Name <- as.factor(superregions)
#
# summary(country_data$IHME_Super_Region_Name)
#
# superregions.list <- levels(country_data$IHME_Super_Region_Name)
#
# region.mt <- data.frame("IHME_Super_Region_Name" = rep(superregions.list, each = length(years)), "Year" = rep(years, length(superregions.list)), "Any_mean" = rep(NA, length(years)*length(superregions.list)), "HMIS_mean" = rep(NA, length(years)*length(superregions.list)))
#
# for (i in 1:length(superregions.list)){
#   superregion_i <- superregions.list[i]
#   for (j in 1:length(years)){
#     year_j <- years[j]
#     region.mt$Any_mean[region.mt$IHME_Super_Region_Name == superregion_i & region.mt$Year == year_j] <- mean(country_data$Any_pred[country_data$IHME_Super_Region_Name == superregion_i & country_data$t.Year == year_j])
#     region.mt$HMIS_mean[region.mt$IHME_Super_Region_Name == superregion_i & region.mt$Year == year_j] <- mean(country_data$HMIS_pred[country_data$IHME_Super_Region_Name == superregion_i & country_data$t.Year == year_j])
#   }
# }
#
# region.mt$Region_ID <- as.numeric(region.mt$IHME_Super_Region_Name)

# pdf(paste(graphics.path, 'Regional_Mean_Trends.pdf'),width=12,height = 6)
#
# par(mfrow = c(1, 2))
#
# plot(ylim = c(0, 1), region.mt$Year[region.mt$IHME_Super_Region_Name == superregions.list[1]], region.mt$Any_mean[region.mt$IHME_Super_Region_Name == superregions.list[1]], type = 'l', xlab = "", ylab = "", main = "Mean Any Treatment Seeking Rate", col = 1)
# for (i in 2:length(superregions.list)){
#   lines(region.mt$Year[region.mt$IHME_Super_Region_Name == superregions.list[i]], region.mt$Any_mean[region.mt$IHME_Super_Region_Name == superregions.list[i]], col = i)
# }
#
# plot(ylim = c(0, 1), region.mt$Year[region.mt$IHME_Super_Region_Name == superregions.list[1]], region.mt$HMIS_mean[region.mt$IHME_Super_Region_Name == superregions.list[1]], type = 'l', xlab = "", ylab = "", main = "Mean HMIS Treatment Seeking Rate", col = 1)
# for (i in 2:length(superregions.list)){
#   lines(region.mt$Year[region.mt$IHME_Super_Region_Name == superregions.list[i]], region.mt$HMIS_mean[region.mt$IHME_Super_Region_Name == superregions.list[i]], col = i)
# }
#
# legend("top", col = 1:length(superregions.list), lty = rep(1, length(superregions.list)), legend = superregions.list)
#
# dev.off()
#
# region.mt.trunc <- region.mt[region.mt$Year > 1999, ]
#
# pdf(paste(graphics.path, 'Regional_Mean_Trends_from2000.pdf'),width=12,height = 6)
#
# par(mfrow = c(1, 2))
#
# plot(ylim = c(0, 1), region.mt.trunc$Year[region.mt.trunc$IHME_Super_Region_Name == superregions.list[1]], region.mt.trunc$Any_mean[region.mt.trunc$IHME_Super_Region_Name == superregions.list[1]], type = 'l', xlab = "", ylab = "", main = "Mean Any Treatment Seeking Rate", col = 1)
# for (i in 2:length(superregions.list)){
#   lines(region.mt.trunc$Year[region.mt.trunc$IHME_Super_Region_Name == superregions.list[i]], region.mt.trunc$Any_mean[region.mt.trunc$IHME_Super_Region_Name == superregions.list[i]], col = i)
# }
#
# plot(ylim = c(0, 1), region.mt.trunc$Year[region.mt.trunc$IHME_Super_Region_Name == superregions.list[1]], region.mt.trunc$HMIS_mean[region.mt.trunc$IHME_Super_Region_Name == superregions.list[1]], type = 'l', xlab = "", ylab = "", main = "Mean HMIS Treatment Seeking Rate", col = 1)
# for (i in 2:length(superregions.list)){
#   lines(region.mt.trunc$Year[region.mt.trunc$IHME_Super_Region_Name == superregions.list[i]], region.mt.trunc$HMIS_mean[region.mt.trunc$IHME_Super_Region_Name == superregions.list[i]], col = i)
# }
#
# legend("top", col = 1:length(superregions.list), lty = rep(1, length(superregions.list)), legend = superregions.list)
#
# dev.off()

# ----------- ggplot2:

# library(ggplot2)
# library(grid)
# library(gridExtra)
#
# source("J:/Treatment_Seeking/Code/multiplot.R")
#
# p1 <- ggplot(data = region.mt.trunc, aes(x = Year, y = Any_mean, group = IHME_Super_Region_Name, colour = IHME_Super_Region_Name)) + geom_line() + geom_point() + xlab("Year") + ylab("") + ggtitle("Average any treatment seeking rate")  + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1)
#
# p2 <- ggplot(data = region.mt.trunc, aes(x = Year, y = HMIS_mean, group = IHME_Super_Region_Name, colour = IHME_Super_Region_Name)) + geom_line() + geom_point() + xlab("Year") + ylab("") + ggtitle("Average public treatment seeking rate") + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1)
#
#
# pdf(paste(graphics.path, 'Regional_Mean_Trends_from2000.pdf'),width=12,height = 5)
#
# grid_arrange_shared_legend(p1, p2)
#
# dev.off()
#
# p3 <- ggplot(data = region.mt, aes(x = Year, y = Any_mean, group = IHME_Super_Region_Name, colour = IHME_Super_Region_Name)) + geom_line() + geom_point() + xlab("Year") + ylab("") + ggtitle("Average any treatment seeking rate")  + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1)
#
# p4 <- ggplot(data = region.mt, aes(x = Year, y = HMIS_mean, group = IHME_Super_Region_Name, colour = IHME_Super_Region_Name)) + geom_line() + geom_point() + xlab("Year") + ylab("") + ggtitle("Average public treatment seeking rate") + scale_colour_discrete(name = "IHME Super-region") + ylim(0, 1)
#
# pdf(paste(graphics.path, 'Regional_Mean_Trends.pdf'),width=12,height = 5)
#
# grid_arrange_shared_legend(p3, p4)
#
# dev.off()
