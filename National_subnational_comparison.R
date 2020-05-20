# Compare national treatment seeking rates to that obtained by aggregating subnational rates:

rm(list = ls())
setwd('J:/Treatment_Seeking/')

graphics.path <- 'J:/Treatment_Seeking/graphics/'
data.path <- 'J:/Treatment_Seeking/Data/'

full_TreatSeek_n <- read.csv(file = paste(data.path, 'TS_predictions.csv', sep = '')) 
head(full_TreatSeek_n)

ihme_pop <- read.csv("Z:\\GBD2019\\Processing\\Stages\\03b_Population_Figures_Export\\Checkpoint_Outputs\\ihme_populations.csv") # This now includes GUF and MYT, and gets updated with API dump.

x.labels <- 1980:2019

# --------------------------------- Any treatment seeking (Can change between Any/Any)-------------------------- #

# 1. Brazil:

Brazil <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "BRA", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Brazil_ADMIN0 <- Brazil$Any_pred[Brazil$Admin_Unit_Level == "ADMIN0"]
Brazil_ADMIN1_aggreg <- rep(NA, length(Brazil_ADMIN0))

Brazil_ADMIN1_data <- Brazil[Brazil$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Brazil_ADMIN1_data[Brazil_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Brazil_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

# 2. China:

China <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "CHN", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

China_ADMIN0 <- China$Any_pred[China$Admin_Unit_Level == "ADMIN0"]
China_ADMIN1_aggreg <- rep(NA, length(China_ADMIN0))

China_ADMIN1_data <- China[China$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- China_ADMIN1_data[China_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  China_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

plot(x.labels, China_ADMIN0, type = 'l')
lines(x.labels, China_ADMIN1_aggreg, col = 2)

# 3. Ethiopia

Ethiopia <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "ETH", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Ethiopia_ADMIN0 <- Ethiopia$Any_pred[Ethiopia$Admin_Unit_Level == "ADMIN0"]
Ethiopia_ADMIN1_aggreg <- rep(NA, length(Ethiopia_ADMIN0))

Ethiopia_ADMIN1_data <- Ethiopia[Ethiopia$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Ethiopia_ADMIN1_data[Ethiopia_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Ethiopia_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

plot(x.labels, Ethiopia_ADMIN0, type = 'l')
lines(x.labels, Ethiopia_ADMIN1_aggreg, col = 2)

# 4. Indonesia

Indonesia <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "IDN", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Indonesia_ADMIN0 <- Indonesia$Any_pred[Indonesia$Admin_Unit_Level == "ADMIN0"]
Indonesia_ADMIN1_aggreg <- rep(NA, length(Indonesia_ADMIN0))

Indonesia_ADMIN1_data <- Indonesia[Indonesia$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Indonesia_ADMIN1_data[Indonesia_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Indonesia_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

plot(x.labels, Indonesia_ADMIN0, type = 'l')
lines(x.labels, Indonesia_ADMIN1_aggreg, col = 2)

# Skip India because national cases are aggregated from subnational case counts which use subnational treatment seeking.

# 6. Iran

Iran <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "IRN", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Iran_ADMIN0 <- Iran$Any_pred[Iran$Admin_Unit_Level == "ADMIN0"]
Iran_ADMIN1_aggreg <- rep(NA, length(Iran_ADMIN0))

Iran_ADMIN1_data <- Iran[Iran$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Iran_ADMIN1_data[Iran_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Iran_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

plot(x.labels, Iran_ADMIN0, type = 'l')
lines(x.labels, Iran_ADMIN1_aggreg, col = 2)

# 7. Mexico

Mexico <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "MEX", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Mexico_ADMIN0 <- Mexico$Any_pred[Mexico$Admin_Unit_Level == "ADMIN0"]
Mexico_ADMIN1_aggreg <- rep(NA, length(Mexico_ADMIN0))

Mexico_ADMIN1_data <- Mexico[Mexico$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Mexico_ADMIN1_data[Mexico_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Mexico_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

plot(x.labels, Mexico_ADMIN0, type = 'l')
lines(x.labels, Mexico_ADMIN1_aggreg, col = 2)

# 8. Nigeria

Nigeria <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "NGA", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Nigeria_ADMIN0 <- Nigeria$Any_pred[Nigeria$Admin_Unit_Level == "ADMIN0"]
Nigeria_ADMIN1_aggreg <- rep(NA, length(Nigeria_ADMIN0))

Nigeria_ADMIN1_data <- Nigeria[Nigeria$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Nigeria_ADMIN1_data[Nigeria_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Nigeria_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

# 9. Pakistan

Pakistan <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "PAK", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Pakistan_ADMIN0 <- Pakistan$Any_pred[Pakistan$Admin_Unit_Level == "ADMIN0"]
Pakistan_ADMIN1_aggreg <- rep(NA, length(Pakistan_ADMIN0))

Pakistan_ADMIN1_data <- Pakistan[Pakistan$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Pakistan_ADMIN1_data[Pakistan_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Pakistan_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

# 10. Philippines

Philippines <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "PHL", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

Philippines_ADMIN0 <- Philippines$Any_pred[Philippines$Admin_Unit_Level == "ADMIN0"]
Philippines_ADMIN1_aggreg <- rep(NA, length(Philippines_ADMIN0))

Philippines_ADMIN1_data <- Philippines[Philippines$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- Philippines_ADMIN1_data[Philippines_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  Philippines_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

# 11. South Africa

SAfrica <- full_TreatSeek_n[full_TreatSeek_n$ISO3 == "ZAF", ]

# Look at mean predictions first - otherwise need to aggregate from realisations at prediction stage.

SAfrica_ADMIN0 <- SAfrica$Any_pred[SAfrica$Admin_Unit_Level == "ADMIN0"]
SAfrica_ADMIN1_aggreg <- rep(NA, length(SAfrica_ADMIN0))

SAfrica_ADMIN1_data <- SAfrica[SAfrica$Admin_Unit_Level == "ADMIN1", ]

for (i in 1:length(x.labels)){
  year_data <- SAfrica_ADMIN1_data[SAfrica_ADMIN1_data$t.Year == x.labels[i], ]
  year_pop <- rep(NA, nrow(year_data))
  for (j in 1:length(year_pop)){year_pop[j] <- ihme_pop$total_pop[ihme_pop$ihme_id == year_data$IHME_location_id[j] & ihme_pop$age_bin == "MAP_infants" & ihme_pop$year == x.labels[i]]}
  SAfrica_ADMIN1_aggreg[i] <- sum(year_data$Any_pred*year_pop)/sum(year_pop)
}

# Under 5 = MAP_infants category.

pdf(paste(graphics.path, 'national_subnational_Any.pdf', sep = ''),width=12,height = 12)

par(mfrow = c(4, 3))

plot(x.labels, Brazil_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(a) Brazil", ylim = c(0, 1))
lines(x.labels, Brazil_ADMIN1_aggreg, col = 2)
legend("bottomright", lty = rep(1, 2), col = c(1, 2), legend = c("ADMIN0 mean", "Aggregated ADMIN1 mean"))

plot(x.labels, China_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(b) China", ylim = c(0, 1))
lines(x.labels, China_ADMIN1_aggreg, col = 2)

plot(x.labels, Ethiopia_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(c) Ethiopia", ylim = c(0, 1))
lines(x.labels, Ethiopia_ADMIN1_aggreg, col = 2)

plot(x.labels, Indonesia_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(d) Indonesia", ylim = c(0, 1))
lines(x.labels, Indonesia_ADMIN1_aggreg, col = 2)

plot(x.labels, Iran_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(e) Iran", ylim = c(0, 1))
lines(x.labels, Iran_ADMIN1_aggreg, col = 2)

plot(x.labels, Mexico_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(f) Mexico", ylim = c(0, 1))
lines(x.labels, Mexico_ADMIN1_aggreg, col = 2)

plot(x.labels, Nigeria_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(g) Nigeria", ylim = c(0, 1))
lines(x.labels, Nigeria_ADMIN1_aggreg, col = 2)

plot(x.labels, Pakistan_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(h) Pakistan", ylim = c(0, 1))
lines(x.labels, Pakistan_ADMIN1_aggreg, col = 2)

plot(x.labels, Philippines_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(i) Philippines", ylim = c(0, 1))
lines(x.labels, Philippines_ADMIN1_aggreg, col = 2)

plot(x.labels, SAfrica_ADMIN0, type = 'l', ylab = "", xlab = "Year", main = "(j) South Africa", ylim = c(0, 1))
lines(x.labels, SAfrica_ADMIN1_aggreg, col = 2)

dev.off()
