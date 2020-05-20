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
