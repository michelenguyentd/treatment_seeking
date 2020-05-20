
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

# Years to model:
years <- 1980:2019

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

# Use location id as factor instead of admin unit name so as to account for 2 Punjabs and 2 Mexicos:
train_data_Any$IHME_location_id <- as.factor(train_data_Any$IHME_location_id)
train_data_HMISfrac$IHME_location_id <- as.factor(train_data_HMISfrac$IHME_location_id)
test_data_Any$IHME_location_id <- as.factor(test_data_Any$IHME_location_id)
test_data_HMISfrac$IHME_location_id <- as.factor(test_data_HMISfrac$IHME_location_id)

clean_TreatSeek_Any$IHME_location_id <- as.factor(clean_TreatSeek_Any$IHME_location_id)
clean_TreatSeek_HMISfrac$IHME_location_id <- as.factor(clean_TreatSeek_HMISfrac$IHME_location_id)
full_TreatSeek_n$IHME_location_id <- as.factor(full_TreatSeek_n$IHME_location_id)
full_TreatSeek$IHME_location_id <- as.factor(full_TreatSeek$IHME_location_id)

formula_any_1 <-  logit_Any ~ -1 + Reg_Factor + s(Year, by = Time_Factor, k = 5) + s(ANC1_coverage_prop, k = 3) + s(DMSP_nighttime, k = 3) + s(GDPpc_id_b2010, k = 3) 

formula_hmis_1 <-  logit_HMIS ~ -1 + Reg_Factor_2 + s(Year, by = Time_Factor_2, k = 5) + s(ANC1_coverage_prop, k = 3) + s(hospital_beds_per1000, k = 3) + s(frac_oop_hexp, k = 3) + s(log_the_pc, k = 3) + s(measles_vacc_cov_prop, k = 3) + s(prop_urban, k = 3) + s(VIIRS_nighttime, k = 3)

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

ref_year_1 <- 2000; ref_year_2 <- 2010; ref_year_3 <- 2019

# Test Any treatment seeking trend matching for Sub-Saharan Africa:


N_pc <- 4

# Use ADMIN0 with data for matching with ADMIN0 and ADMIN1 without data:
temp_region_cov <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == "Sub-Saharan Africa", ]
temp_region_cov$Matched_Trend <- NA

# Countries in region with more than 1 data point - can't test subnational with more than 1 datapoint because not in original PCA set:
region_units_data_Any <- unique(temp_region_cov$IHME_location_id[temp_region_cov$Admin_Unit_Level == "ADMIN0" & temp_region_cov$IHME_location_id %in% units_with_Any_data & !(temp_region_cov$IHME_location_id %in% units_1_Any_data)])

# a) Trend: Use changes between dynamic covariates for years 2000-2010 and 2010-2019.

t_Any_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% c(units_no_Any_data, units_1_Any_data)), ]
t_Any_region_cov_1 <- t_Any_region_cov[t_Any_region_cov$Year == ref_year_1, ]
t_Any_region_cov_2 <- t_Any_region_cov[t_Any_region_cov$Year == ref_year_2, ]
t_Any_region_cov_3 <- t_Any_region_cov[t_Any_region_cov$Year == ref_year_3, ]

# Omit "DMSP_nighttime" because static.
temp_df1 <- cbind(t_Any_region_cov_2[, c("ANC1_coverage_prop", "GDPpc_id_b2010")] - t_Any_region_cov_1[, c("ANC1_coverage_prop",  "GDPpc_id_b2010")], t_Any_region_cov_3[, c("ANC1_coverage_prop",  "GDPpc_id_b2010")] - t_Any_region_cov_2[, c("ANC1_coverage_prop",   "GDPpc_id_b2010")])
region.pca1 <- prcomp(temp_df1, center = TRUE, scale. = TRUE)

for (i in 1:length(region_units_data_Any)){
  test_unit <- region_units_data_Any[i]
  
  query_df1 <- region.pca1$x[t_Any_region_cov_1$IHME_location_id == test_unit, 1:N_pc]
  ref_df1 <- region.pca1$x[t_Any_region_cov_1$IHME_location_id != test_unit & t_Any_region_cov_1$Admin_Unit_Level == "ADMIN0", 1:N_pc]
  
  ref_class_1 <- t_Any_region_cov_1[t_Any_region_cov_1$IHME_location_id != test_unit & t_Any_region_cov_1$Admin_Unit_Level == "ADMIN0", c("IHME_location_id")]
  
  matched_trend_unit <- as.character(knn1(test = query_df1, train = ref_df1, cl = ref_class_1))
  
  # Implement matched trends:
  
  temp_region_cov$Matched_Trend[temp_region_cov$IHME_location_id == test_unit] <-  unique(as.character(temp_region_cov$Time_Factor[temp_region_cov$IHME_location_id == matched_trend_unit]))
}

reduced_set <- temp_region_cov[temp_region_cov$IHME_location_id %in% region_units_data_Any, ]
NN_summary <- unique(reduced_set[, c("IHME_location_id", "Time_Factor", "Matched_Trend")])
sum(NN_summary$Time_Factor == NN_summary$Matched_Trend)/nrow(NN_summary)

# N_pc = 1: 0.6129032.
# N_pc = 2: 0.7741935.
# N_pc = 3: 0.6129032.
# N_pc = 4: 0.5483871.

# Test HMISfrac trend matching for Sub-Saharan Africa:

N_pc <- 4

# Use ADMIN0 with data for matching with ADMIN0 and ADMIN1 without data:
temp_region_cov <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == "Sub-Saharan Africa", ]
temp_region_cov$Matched_Trend <- NA

# Countries in region with more than 1 data point - can't test subnational with more than 1 datapoint because not in original PCA set:
region_units_data_HMISfrac <- unique(temp_region_cov$IHME_location_id[temp_region_cov$Admin_Unit_Level == "ADMIN0" & temp_region_cov$IHME_location_id %in% units_with_HMISfrac_data & !(temp_region_cov$IHME_location_id %in% units_1_HMISfrac_data)])

t_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)), ]
t_HMISfrac_region_cov_1 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$Year == ref_year_1, ]
t_HMISfrac_region_cov_2 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$Year == ref_year_2, ]
t_HMISfrac_region_cov_3 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$Year == ref_year_3, ]

# Omit "VIIRS_nighttime" because static.
temp_df2 <- cbind(t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")] - t_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], t_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")] - t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")])
region.pca2 <- prcomp(temp_df2, center = TRUE, scale. = TRUE)


for (i in 1:length(region_units_data_HMISfrac)){
  test_unit <- region_units_data_HMISfrac[i]
  
  query_df2 <- region.pca2$x[t_HMISfrac_region_cov_1$IHME_location_id == test_unit, 1:N_pc]
  ref_df2 <- region.pca2$x[t_HMISfrac_region_cov_1$IHME_location_id != test_unit & t_HMISfrac_region_cov_1$Admin_Unit_Level == "ADMIN0", 1:N_pc]
  
  ref_class_2 <- t_HMISfrac_region_cov_1[t_HMISfrac_region_cov_1$IHME_location_id != test_unit & t_HMISfrac_region_cov_1$Admin_Unit_Level == "ADMIN0", c("IHME_location_id")]
  
  matched_trend_unit <- as.character(knn1(test = query_df2, train = ref_df2, cl = ref_class_2))
  
  # Implement matched trends:
  
  temp_region_cov$Matched_Trend[temp_region_cov$IHME_location_id == test_unit] <-  unique(as.character(temp_region_cov$Time_Factor_2[temp_region_cov$IHME_location_id == matched_trend_unit]))
}

reduced_set <- temp_region_cov[temp_region_cov$IHME_location_id %in% region_units_data_HMISfrac, ]
NN_summary <- unique(reduced_set[, c("IHME_location_id", "Time_Factor_2", "Matched_Trend")])
sum(NN_summary$Time_Factor_2 == NN_summary$Matched_Trend)/nrow(NN_summary)

# For 31 test points:

# N_pc = 1: 0.4193548.
# N_pc = 2: 0.483871.
# N_pc = 3: 0.3548387.
# N_pc = 4: 0.3225806.


# Test HMISfrac trend matching for Latin America and Caribbean:

N_pc <- 4

# Use ADMIN0 with data for matching with ADMIN0 and ADMIN1 without data:
temp_region_cov <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == "Latin America and Caribbean", ]
temp_region_cov$Matched_Trend <- NA

# Countries in region with more than 1 data point - can't test subnational with more than 1 datapoint because not in original PCA set:
region_units_data_HMISfrac <- unique(temp_region_cov$IHME_location_id[temp_region_cov$Admin_Unit_Level == "ADMIN0" & temp_region_cov$IHME_location_id %in% units_with_HMISfrac_data & !(temp_region_cov$IHME_location_id %in% units_1_HMISfrac_data)])

t_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% c(units_no_HMISfrac_data, units_1_HMISfrac_data)), ]
t_HMISfrac_region_cov_1 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$Year == ref_year_1, ]
t_HMISfrac_region_cov_2 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$Year == ref_year_2, ]
t_HMISfrac_region_cov_3 <- t_HMISfrac_region_cov[t_HMISfrac_region_cov$Year == ref_year_3, ]

# Omit "VIIRS_nighttime" because static.
temp_df2 <- cbind(t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")] - t_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], t_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")] - t_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")])
region.pca2 <- prcomp(temp_df2, center = TRUE, scale. = TRUE)


for (i in 1:length(region_units_data_HMISfrac)){
  test_unit <- region_units_data_HMISfrac[i]
  
  query_df2 <- region.pca2$x[t_HMISfrac_region_cov_1$IHME_location_id == test_unit, 1:N_pc]
  ref_df2 <- region.pca2$x[t_HMISfrac_region_cov_1$IHME_location_id != test_unit & t_HMISfrac_region_cov_1$Admin_Unit_Level == "ADMIN0", 1:N_pc]
  
  ref_class_2 <- t_HMISfrac_region_cov_1[t_HMISfrac_region_cov_1$IHME_location_id != test_unit & t_HMISfrac_region_cov_1$Admin_Unit_Level == "ADMIN0", c("IHME_location_id")]
  
  matched_trend_unit <- as.character(knn1(test = query_df2, train = ref_df2, cl = ref_class_2))
  
  # Implement matched trends:
  
  temp_region_cov$Matched_Trend[temp_region_cov$IHME_location_id == test_unit] <-  unique(as.character(temp_region_cov$Time_Factor_2[temp_region_cov$IHME_location_id == matched_trend_unit]))
}

reduced_set <- temp_region_cov[temp_region_cov$IHME_location_id %in% region_units_data_HMISfrac, ]
NN_summary <- unique(reduced_set[, c("IHME_location_id", "Time_Factor_2", "Matched_Trend")])
sum(NN_summary$Time_Factor_2 == NN_summary$Matched_Trend)/nrow(NN_summary)

# For 8 test points:
# N_pc = 1: 0.5.
# N_pc = 2: 0.5.
# N_pc = 3: 0.375.
# N_pc = 4: 0.25.

# Test random effect matching for all:

# 1. Fit full model to get baseline effect for units with data.

Any_outliers <- which((clean_TreatSeek_Any$ISO3 == "BRA" & clean_TreatSeek_Any$Year == 1996) | (clean_TreatSeek_Any$ISO3 == "IND" & clean_TreatSeek_Any$Year == 1993) | (clean_TreatSeek_Any$ISO3 == "PAK" & clean_TreatSeek_Any$Year == 1991) | (clean_TreatSeek_Any$ISO3 == "NGA" & clean_TreatSeek_Any$Year == 2008)) 
HMIS_outliers <- which((clean_TreatSeek_HMISfrac$ISO3 == "BRA" & clean_TreatSeek_HMISfrac$Year == 1996) | (clean_TreatSeek_HMISfrac$ISO3 == "IND" & clean_TreatSeek_HMISfrac$Year == 1993) | (clean_TreatSeek_HMISfrac$ISO3 == "PAK" & clean_TreatSeek_HMISfrac$Year == 1991) | (clean_TreatSeek_HMISfrac$ISO3 == "NGA" & clean_TreatSeek_HMISfrac$Year == 2008)) 

Any_data <- clean_TreatSeek_Any[-Any_outliers, ]
HMIS_data <- clean_TreatSeek_HMISfrac[-HMIS_outliers, ]

Any_data <- Any_data[Any_data$Year > 1989, ]
HMIS_data <- HMIS_data[HMIS_data$Year > 1989, ]

Any_data$IHME_location_id <- as.factor(Any_data$IHME_location_id)
HMIS_data$IHME_location_id <- as.factor(HMIS_data$IHME_location_id)

Any_model <-  gamm(formula_any_1, data = Any_data, random = list(IHME_location_id = ~ 1)) #, control = list(niterEM=1, opt='optim', maxit = 500))
HMIS_model <- gamm(formula_hmis_1, data = HMIS_data, random = list(IHME_location_id = ~ 1)) #, control = list(niterEM=1, opt='optim', maxit = 500))

Any_rf_summary <- data.frame("IHME_location_id" = units_with_Any_data, "rf" = rep(NA, length(units_with_Any_data)), "matched_rf" = rep(NA, length(units_with_Any_data)))

for (i in 1:length(units_with_Any_data)){
  Any_rf_summary$rf[Any_rf_summary$IHME_location_id == as.character(units_with_Any_data[i])] <- ranef(Any_model$lme,level= 10)[paste("1/1/1/1/1/1/1/1/1/", units_with_Any_data[i], sep = ""), ]
}

HMISfrac_rf_summary <- data.frame("IHME_location_id" = units_with_HMISfrac_data, "rf" = rep(NA, length(units_with_HMISfrac_data)), "matched_rf" = rep(NA, length(units_with_HMISfrac_data)))

for (i in 1:length(units_with_HMISfrac_data)){
  HMISfrac_rf_summary$rf[HMISfrac_rf_summary$IHME_location_id == as.character(units_with_HMISfrac_data[i])] <- ranef(HMIS_model$lme,level= 13)[paste("1/1/1/1/1/1/1/1/1/1/1/1/", units_with_HMISfrac_data[i], sep = ""), ]
}

N_pc <- 6

ref_year_1 <- 2000; ref_year_2 <- 2010; ref_year_3 <- 2019

for (j in 1:length(master.superregion.list)){
  
  # Use ADMIN0 with data for matching with ADMIN0 and ADMIN1 without data:
  temp_region_cov <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == as.character(master.superregion.list[j]), ]
  
  
  # 2. For each super region, conduct PCA as before.
  
  rf_Any_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% units_no_Any_data), ]
 
  # if(j == 6){rf_Any_region_cov <- rf_Any_region_cov[rf_Any_region_cov$IHME_location_id != ETimor_id, ]} # Remove East Timor as possible match.
  
  
  rf_Any_region_cov_1 <- rf_Any_region_cov[rf_Any_region_cov$Year == ref_year_1, ]
  rf_Any_region_cov_2 <- rf_Any_region_cov[rf_Any_region_cov$Year == ref_year_2, ]
  rf_Any_region_cov_3 <- rf_Any_region_cov[rf_Any_region_cov$Year == ref_year_3, ]
  rf_Any_region_no_data <- unique(rf_Any_region_cov$IHME_location_id[rf_Any_region_cov$IHME_location_id %in% units_no_Any_data])
  
  rf_HMISfrac_region_cov <- temp_region_cov[temp_region_cov$Admin_Unit_Level == "ADMIN0" | (temp_region_cov$Admin_Unit_Level == "ADMIN1" & temp_region_cov$IHME_location_id %in% units_no_HMISfrac_data), ]
  
  # if(j == 6){rf_HMISfrac_region_cov <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$IHME_location_id != ETimor_id, ]} # Remove East Timor as possible match.
  rf_HMISfrac_region_cov_1 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$Year == ref_year_1, ]
  rf_HMISfrac_region_cov_2 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$Year == ref_year_2, ]
  rf_HMISfrac_region_cov_3 <- rf_HMISfrac_region_cov[rf_HMISfrac_region_cov$Year == ref_year_3, ]
  rf_HMISfrac_region_no_data <- unique(rf_HMISfrac_region_cov$IHME_location_id[rf_HMISfrac_region_cov$IHME_location_id %in% units_no_HMISfrac_data])
  
  
  temp_df3 <- cbind(rf_Any_region_cov_1[, c("ANC1_coverage_prop", "DMSP_nighttime", "GDPpc_id_b2010")], rf_Any_region_cov_2[, c("ANC1_coverage_prop", "DMSP_nighttime", "GDPpc_id_b2010")], rf_Any_region_cov_3[, c("ANC1_coverage_prop", "DMSP_nighttime", "GDPpc_id_b2010")])
  region.pca3 <- prcomp(temp_df3, center = TRUE, scale. = TRUE)
  
  temp_df4 <- cbind(rf_HMISfrac_region_cov_1[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], rf_HMISfrac_region_cov_2[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")], rf_HMISfrac_region_cov_3[, c("ANC1_coverage_prop", "VIIRS_nighttime", "hospital_beds_per1000", "frac_oop_hexp", "log_the_pc", "measles_vacc_cov_prop", "prop_urban")])
  region.pca4 <- prcomp(temp_df4, center = TRUE, scale. = TRUE)
  
  # 3. For each test location with data, do NN from reference set. 
  
  test_units <- unique(rf_Any_region_cov$IHME_location_id[rf_Any_region_cov$IHME_location_id %in% units_with_Any_data & rf_Any_region_cov$IHME_location_id %in% units_with_HMISfrac_data])
  test_units <- as.character(test_units)
  
  for (k in 1:length(test_units)){
    
    query_df3 <- region.pca3$x[rf_Any_region_cov_1$IHME_location_id == test_units[k], 1:N_pc]
    ref_df3 <- region.pca3$x[!(rf_Any_region_cov_1$IHME_location_id %in% units_no_Any_data) & rf_Any_region_cov_1$IHME_location_id != test_units[k], 1:N_pc]
    
    query_df4 <- region.pca4$x[rf_HMISfrac_region_cov_1$IHME_location_id == test_units[k], 1:N_pc]
    ref_df4 <- region.pca4$x[!(rf_HMISfrac_region_cov_1$IHME_location_id %in% units_no_HMISfrac_data) & rf_HMISfrac_region_cov_1$IHME_location_id != test_units[k], 1:N_pc]
    
    ref_class_3 <- rf_Any_region_cov_1[!(rf_Any_region_cov_1$IHME_location_id %in% units_no_Any_data) & rf_Any_region_cov_1$IHME_location_id != test_units[k], c("IHME_location_id")]
    ref_class_4 <- rf_HMISfrac_region_cov_1[!(rf_HMISfrac_region_cov_1$IHME_location_id %in% units_no_HMISfrac_data) & rf_HMISfrac_region_cov_1$IHME_location_id != test_units[k], c("IHME_location_id")]
    
    neighbour_result_rf_any <- knn1(test = query_df3, train = ref_df3, cl = ref_class_3)
    neighbour_result_rf_pf <- knn1(test = query_df4, train = ref_df4, cl = ref_class_4)
    
    Any_rf_summary$matched_rf[Any_rf_summary$IHME_location_id == test_units[k]] <- Any_rf_summary$rf[Any_rf_summary$IHME_location_id == as.character(neighbour_result_rf_any)]
    HMISfrac_rf_summary$matched_rf[HMISfrac_rf_summary$IHME_location_id == test_units[k]] <- HMISfrac_rf_summary$rf[HMISfrac_rf_summary$IHME_location_id == as.character(neighbour_result_rf_pf)]
    
  }
}

# 4. Compare matched unit's random effect to baseline effect. 

head(Any_rf_summary)
head(HMISfrac_rf_summary)

Any_rf_summary$squared_error <- (Any_rf_summary$rf-Any_rf_summary$matched_rf)^2
HMISfrac_rf_summary$squared_error <- (HMISfrac_rf_summary$rf-HMISfrac_rf_summary$matched_rf)^2

mean(Any_rf_summary$squared_error, na.rm = TRUE) 
# N_pc = 1: 0.2707213
# N_pc = 2: 0.2397487
# N_pc = 3: 0.2967159
# N_pc = 4: 0.3021562
# N_pc = 5: 0.275657
# N_pc = 6: 0.2731984

mean(HMISfrac_rf_summary$squared_error, na.rm = TRUE)
# N_pc = 1: 0.3516883
# N_pc = 2: 0.5215987
# N_pc = 3: 0.4833851
# N_pc = 4: 0.4549184
# N_pc = 5: 0.5585874
# N_pc = 6: 0.6116598