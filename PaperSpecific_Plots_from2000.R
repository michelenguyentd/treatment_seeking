# This R script plots the time series results and gives the final estimates table for the paper
# subsetted to endemic countries (and their subnationals) for 1990-2019 (plots) and 2000-2019 (table). 

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

full_TreatSeek_n <- read.csv(file = paste(data.path, 'TS_predictions_GAMkNN.csv', sep = '')) 
head(full_TreatSeek_n)

endemicity_config_2019 <- read.csv("Z:/GBD2019/Processing/Config_Data/endemicity_config_2019.csv")
head(endemicity_config_2019)
endemic_units <- unique(endemicity_config_2019$location_id[endemicity_config_2019$any_malaria_endemic == 1])

modelled.countries <- unique(full_TreatSeek_n$IHME_location_id[full_TreatSeek_n$Admin_Unit_Level == "ADMIN0"])
# Exclude modelled countries which are not endemic or GUF or MYT:
modelled.countries <- modelled.countries[modelled.countries %in% endemic_units]
modelled.countries <- c(modelled.countries, unique(full_TreatSeek_n$IHME_location_id[full_TreatSeek_n$ISO3 %in% c("GUF", "MYT")]))
modelled.subnationals <- unique(full_TreatSeek_n$IHME_location_id[full_TreatSeek_n$Admin_Unit_Level == "ADMIN1"])
plot.units <- c(modelled.countries, modelled.subnationals)
  
unique(endemicity_config_2019[endemicity_config_2019$location_id %in% endemic_units[!(endemic_units %in% plot.units)], c("location_name")])
# Endemic regions/countries not modelled: Hong Kong, Macao, UAE, Egypt.
# Plot units which are not in endemic list (for IHME):
unique(full_TreatSeek_n[full_TreatSeek_n$IHME_location_id %in% plot.units[which(!(plot.units %in% endemic_units))], c("Admin_Unit_Name", "Country_Name")])
unique(full_TreatSeek_n[full_TreatSeek_n$IHME_location_id %in% endemic_units[which(!(endemic_units %in% plot.units))], c("Admin_Unit_Name", "Country_Name")])
# No endemic units not to be plotted.

length(plot.units)/6 # 81 pages + 4 plots on 82nd page.

# Currently, all African ADMIN1 results are from GAMM model not pixels...

master.superregion.list <- unique(full_TreatSeek_n$IHME_Super_Region_Name)
master.superregion.list <-droplevels(master.superregion.list)

clean_TreatSeek_Any <- read.csv(paste(data.path, "clean_TreatSeek_Any.csv", sep = ""), stringsAsFactors = FALSE)
clean_TreatSeek_HMISfrac <- read.csv(paste(data.path, "clean_TreatSeek_HMISfrac.csv", sep = ""), stringsAsFactors = FALSE)

# ================== Any_Treat, HMIS_Treat time series plots from 1990:

year_from1990 <- 1990:2019

pdf(paste(graphics.path, 'Any_Treat_from1990.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.superregion.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == master.superregion.list[j] & full_TreatSeek_n$Year >= 1990, ]
  countries <- unique(region.data$ISO3)
  if(length(countries) > 0){
    for (i in 1:length(countries)){
      unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
      for (k in 1:nrow(unit.list)){
        if(unit.list$IHME_location_id[k] %in% plot.units){
          unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
          unit.row <- unit.row[unit.row$t.Year %in% year_from1990, ]
          plot(0,0,type='n',ylim=c(0,1),xlim=c(year_from1990[1], year_from1990[length(year_from1990)]),main=paste(master.superregion.list[j], ': ', unit.list$Admin_Unit_Name[k], ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment',xlab='Year')
          
          plotCI(year_from1990,unit.row$Any_pred,ui=unit.row$Any_pred_high,li=unit.row$Any_pred_low,ylim=c(0,1),add=T)
          
          in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i] & clean_TreatSeek_Any$Year > 1989)
          
          if(length(in_out)>0){
            
            points.col <- rep("red", length(in_out))
            country_line<- clean_TreatSeek_Any[in_out, ]
            
            if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
            if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
            if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
            if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}
            
            plotCI(country_line$Year, country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
          }
          if (countries[i] == 'AFG'){
            legend('topright',legend=c('Survey','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
          }
        }
      }
    }
  }
}

dev.off()


pdf(paste(graphics.path, 'Public_Treat_from1990.pdf'),width=8.7,height = 11.2)

par(mfrow=c(3,2))

for (j in 1:length(master.superregion.list)){
  region.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Super_Region_Name == master.superregion.list[j] & full_TreatSeek_n$Year >= 1990, ]
  countries <- unique(region.data$ISO3)
  if(length(countries) > 0){
    for (i in 1:length(countries)){
      unit.list <- unique(region.data[region.data$ISO3 == countries[i], c("IHME_location_id", "Admin_Unit_Name")])
      for (k in 1:nrow(unit.list)){
        if(unit.list$IHME_location_id[k] %in% plot.units){
          unit.row <- region.data[region.data$ISO3 == countries[i] & region.data$IHME_location_id == unit.list$IHME_location_id[k], ]
          unit.row <- unit.row[unit.row$t.Year %in% year_from1990, ]
          plot(0,0,type='n',ylim=c(0,1),xlim=c(year_from1990[1], year_from1990[length(year_from1990)]),main=paste(master.superregion.list[j], ': ', unit.list$Admin_Unit_Name[k], ", ", countries[i], sep = ''), ylab='% U5 fevers sought treatment at public facilities',xlab='Year')
          
          plotCI(year_from1990,unit.row$HMIS_pred,ui=unit.row$HMIS_pred_high,li=unit.row$HMIS_pred_low,ylim=c(0,1),add=T)
          
          in_out<-which(clean_TreatSeek_Any$Admin_Unit_Name == unit.list$Admin_Unit_Name[k] & as.character(clean_TreatSeek_Any$ISO3) == countries[i] & clean_TreatSeek_Any$Year > 1989)
          
          if(length(in_out)>0){
            
            points.col <- rep("red", length(in_out))
            country_line<- clean_TreatSeek_Any[in_out, ]
            
            if(countries[i] == "IND"){points.col[country_line$Year == 1993] <- "blue"}
            if(countries[i] == "PAK"){points.col[country_line$Year == 1991] <- "blue"}
            if(countries[i] == "NGA"){points.col[country_line$Year == 2008] <- "blue"}
            if(countries[i] == "BRA"){points.col[country_line$Year == 1996] <- "blue"}
            
            plotCI(country_line$Year, country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,ylim=c(0,1),add=T, col = points.col)
          }
          if (countries[i] == 'AFG'){
            legend('topright',legend=c('Survey','Omitted in model fit', 'Predicted'),pch=1,col=c('red','blue', 'black'))
          }
        }
      }
    }
  }
}

dev.off()

# =========== 2. Subset matched units table:

NN_results <- read.csv(file = paste(data.path, "trend_rf_NN.csv", sep = ""))

# 1. Subset units (using plot.units)
NN_results_subset <- NN_results[NN_results$IHME_location_id %in% plot.units, ]
nrow(NN_results_subset)

# 2. Remove IHME location id column:
NN_results_subset <- NN_results_subset[, -which(colnames(NN_results_subset) == "IHME_location_id")]
head(NN_results_subset)

write.csv(NN_results_subset, file = paste(data.path, 'TS_SupplementaryAppendix3.csv', sep = ''))


# =========== 3. Subset results table:

full_TreatSeek_n <- read.csv(file = paste(data.path, 'TS_predictions_GAMkNN.csv', sep = '')) 
table_subset <- full_TreatSeek_n[, c("ISO2", "ISO3", "GAUL_Code", "Country_Name", "Admin_Unit_Level", "Admin_Unit_Name", "IHME_location_id",
                                     "IHME_Super_Region_Name", "IHME_Region_Name", "Year", "Any_pred", "Any_pred_low", "Any_pred_high",
                                     "HMIS_pred", "HMIS_pred_low", "HMIS_pred_high", "HMISfrac_pred", "HMISfrac_pred_low", "HMISfrac_pred_high")]
head(table_subset)

# 1. Subset columns:
colnames(table_subset)[colnames(table_subset) %in% c("HMIS_pred", "HMIS_pred_low", "HMIS_pred_high", "HMISfrac_pred", "HMISfrac_pred_low", "HMISfrac_pred_high")] <- 
  c("Public_pred", "Public_pred_low", "Public_pred_high", "PublicFrac_pred", "PublicFrac_pred_low", "PublicFrac_pred_high")
head(table_subset)

# 2. Subset years (note years here is frozen prior 1995):
table_subset <- table_subset[table_subset$Year > 1999, ]
head(table_subset)

# 3. Subset units (using plot.units)
table_subset <- table_subset[table_subset$IHME_location_id %in% plot.units, ]
nrow(table_subset)/length(2000:2019)
length(plot.units)

# 4. Remove IHME location id column:
table_subset <- table_subset[, -which(colnames(table_subset) == "IHME_location_id")]
head(table_subset)

write.csv(table_subset, file = paste(data.path, 'TS_SupplementaryAppendix5.csv', sep = ''))

# ============ 4. Subset aggregated results table:

superregion_mean_realisations <- read.csv(file = paste(data.path, 'superregion_mean_realisations.csv', sep = ''))

ssa_mean_realisations <- read.csv(file = paste(data.path, 'ssa_mean_realisations.csv', sep = ''))

head(superregion_mean_realisations)

subset1 <- superregion_mean_realisations[, c("IHME_Super_Region_name", "Year", "Any_pred", "HMIS_pred")]
subset1 <- subset1[subset1$Year %in% c(2000, 2019), ]

subset2 <- ssa_mean_realisations[, c("IHME_Region_name", "Year", "Any_pred", "HMIS_pred")]
subset2 <- subset2[subset2$Year %in% c(2000, 2019), ]

write.csv(subset1, file = paste(data.path, 'superregion_subset.csv', sep = ''))
write.csv(subset2, file = paste(data.path, 'ssaregion_subset.csv', sep = ''))
