### Sought any treatment ### Original by Donal (last modified 02/02/2017); edited by Michele (24/01/2018). 

## ----------------- Step 1: Set up ---------------- ##

rm(list = ls())
setwd("J:/Treatment_Seeking")

library(mgcv) #for GAMM
library(MuMIn) #for AIC (model selection)
library(plotrix)
library(caTools)
library(VIM) # For matrixplot.

# Config data to select countries to model:
config_file <- read.csv("Z:/Config_Data/National_Config_Data.csv")
# Count number of countries to be modelled for treatment seeking: 
admin0_units <- config_file[config_file$MAP_Include == "Y", ]
nrow(admin0_units)

# Create list of iso for countries to model:
country_list <- admin0_units$ISO3

# Import dataset:
TreatSeek <- read.csv("Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/NationalSurveyResults_Weighted_With_UpdatedIndicators.csv") # ADMIN0. 
str(TreatSeek) # Country_Name is already a Factor.

# Only use DHS and MIS surveys, remove Tanzania 2015 DHS survey and India 1993 survey:
TreatSeek <- TreatSeek[TreatSeek$Year>1995 & as.character(TreatSeek$SurveyType) %in% c("DHS", "MIS") & as.character(TreatSeek$SurveyName) != "TZ2015DHS" & as.character(TreatSeek$SurveyName) != "IA1993DHS", ]
TreatSeek <- droplevels(TreatSeek)
str(TreatSeek)

# Add a column for iso3 and year together:
TreatSeek$CountryYear <- paste(TreatSeek$ISO3, TreatSeek$Year, sep=" ") # Use ISO3 intead of the country name.

#### adding out of pocket ####

WDI_Data <-read.csv('J:/Treatment_Seeking/WDI_csv/WDIData.csv',stringsAsFactors=F)

raw_out_pocket <- WDI_Data[WDI_Data$Indicator.Code == 'SH.XPD.OOPC.TO.ZS', ]

raw_out_pocket$Country.Code[which(raw_out_pocket$Country.Code=='ZAR')]<-'COD'

raw_out_pocket$Country.Code[which(raw_out_pocket$Country.Code=='TMP')]<-'TLS'

# raw_out_pocket$X2016<-NA

TreatSeek$Out_pocket<-NA

raw_to_fill<-(1:length(TreatSeek[,1]))#[-c(which(is.na(TreatSeek$Any_treat)))]

for (i in raw_to_fill){
  
  row_num<-which(as.character(raw_out_pocket$Country.Code) == as.character(TreatSeek$ISO3[i]))
  
  col_num<-grep(as.character(TreatSeek$Year[i]),names(raw_out_pocket))
  
  TreatSeek$Out_pocket[i]<-raw_out_pocket[row_num,col_num] # There are some NAs.
  
}

sum(is.na(TreatSeek$Out_pocket))

apply(TreatSeek, FUN = function(x){sum(is.na(x))}, MARGIN = 2) # There exists NAs for 4 Any_treat_low/Any_treat_high. Why?

## Missing values in treatment seeking surveys:

years <- 1980:2017

Any_survey_matrix <- matrix(NA, nrow = length(country_list), ncol = length(years))

for (i in 1:length(country_list)){
  for (j in 1:length(years)){
    country_year_value <- TreatSeek$Any_treat[as.character(TreatSeek$ISO3) == as.character(country_list[i]) & TreatSeek$Year == years[j]]
    if (length(country_year_value)>0){
      Any_survey_matrix[i, j] <- country_year_value   
    }
  }
}

row.names(Any_survey_matrix) <- country_list
labels <- years

paper_subset <- Any_survey_matrix[, 21:ncol(Any_survey_matrix)]
y.ticks <- which(apply(paper_subset, MARGIN = 1, FUN = function(x){sum(!is.na(x))}) == 0) # Find the countries which have no data in between 2000-2017.
x.ticks <- which(years %in% c(1980, 1995, 2000, 2017))

pdf(paste("Any_survey", "_missingvalues.pdf", sep = ""), height = 10, width = 12)
par(mar = c(2, 4, 2, 2))
matrixplot(Any_survey_matrix[, ], main = paste("Missing values for Any_Treat"), xlab = '', ylab = "", axes = FALSE, labels = "")
axis(side = 1, at = x.ticks, labels = labels[x.ticks], tick = FALSE, las = 1, tck = 0, col = 'white')
axis(side = 2, at = y.ticks, labels = country_list[y.ticks], tick = FALSE, las = 1, tck = 0, col = 'white')
dev.off()

HMIS_survey_matrix <- matrix(NA, nrow = length(country_list), ncol = length(years))

for (i in 1:length(country_list)){
  for (j in 1:length(years)){
    country_year_value <- TreatSeek$HMIS_treat[as.character(TreatSeek$ISO3) == as.character(country_list[i]) & TreatSeek$Year == years[j]]
    if (length(country_year_value)>0){
      HMIS_survey_matrix[i, j] <- country_year_value   
    }
  }
}

row.names(HMIS_survey_matrix) <- country_list
labels <- years

paper_subset <- HMIS_survey_matrix[, 21:ncol(HMIS_survey_matrix)]
y.ticks <- which(apply(paper_subset, MARGIN = 1, FUN = function(x){sum(!is.na(x))}) == 0) # Find the countries which have no data in between 2000-2017.
x.ticks <- which(years %in% c(1980, 1995, 2000, 2017))

pdf(paste("HMIS_survey", "_missingvalues.pdf", sep = ""), height = 10, width = 12)
par(mar = c(2, 4, 2, 2))
matrixplot(HMIS_survey_matrix[, ], main = paste("Missing values for HMIS_Treat"), xlab = '', ylab = "", axes = FALSE, labels = "")
axis(side = 1, at = x.ticks, labels = labels[x.ticks], tick = FALSE, las = 1, tck = 0, col = 'white')
axis(side = 2, at = y.ticks, labels = country_list[y.ticks], tick = FALSE, las = 1, tck = 0, col = 'white')
dev.off()



TreatSeek_2000 <- TreatSeek[TreatSeek$Year > 1999, ] # Only use data from 2000 onwards.

cleandata_any <- droplevels(na.omit(TreatSeek[,c('Country_Name','ISO3', 'WHO_Subregion', 'Year', 'GDPGrowth', 'HealthExTotal', 'HealthExPub', 'PregWomenCare', 'PrimaryComplete', 'RuralPop', 'DPT', 'NurseMidwives', 'Out_pocket', 'Any_treat', 'Any_treat_low_SVY', 'Any_treat_high_SVY', 'HMIS_treat', 'HMIS_treat_low_SVY', 'HMIS_treat_high_SVY')]))
cleandata_any_2000 <-  droplevels(na.omit(TreatSeek_2000[,c('Country_Name','ISO3', 'WHO_Subregion', 'Year', 'GDPGrowth', 'HealthExTotal', 'HealthExPub', 'PregWomenCare', 'PrimaryComplete', 'RuralPop', 'DPT', 'NurseMidwives', 'Out_pocket', 'Any_treat', 'Any_treat_low_SVY', 'Any_treat_high_SVY', 'HMIS_treat', 'HMIS_treat_low_SVY', 'HMIS_treat_high_SVY')]))
  
length(unique(cleandata_any$ISO3)) # 57 countries and 164 surveys.
nrow(cleandata_any)
length(unique(cleandata_any_2000$ISO3)) # 55 countries and 135 surveys. 
nrow(cleandata_any_2000)

# Add columns for HMIS_frac:

cleandata_any$HMIS_frac <- cleandata_any$HMIS_treat/cleandata_any$Any_treat
cleandata_any_2000$HMIS_frac <- cleandata_any_2000$HMIS_treat/cleandata_any_2000$Any_treat

################## National any treatment seeking model #######################

formula_any1 <-  Any_treat ~ WHO_Subregion + s(Year) + GDPGrowth +  HealthExTotal + PregWomenCare + PrimaryComplete + RuralPop + DPT + NurseMidwives

# Any_treat (data from 1995):

model_any1 <- uGamm(formula_any1, data=cleandata_any)

summary(model_any1)

model.select.any <- dredge(model_any1)

subset(model.select.any, delta < 2) #takes the best model with delta AICc less than 2

best_any1 <- Any_treat ~ s(Year) + WHO_Subregion + GDPGrowth + HealthExTotal + PregWomenCare + PrimaryComplete

chosen_model_any <- gam(best_any1, data = cleandata_any)


# Any_treat (data from 2000):

model_any1_2000 <- uGamm(formula_any1, data=cleandata_any_2000)

summary(model_any1_2000)

model.select.any_2000 <- dredge(model_any1_2000)

subset(model.select.any_2000, delta < 2) #takes the best model with delta AICc less than 2

best_any1_2000 <- Any_treat ~ s(Year) + WHO_Subregion + GDPGrowth + HealthExTotal + PregWomenCare + PrimaryComplete # Same model formula!

chosen_model_any_2000 <- gam(best_any1_2000, data = cleandata_any_2000)

linear_mod_any <- lm(Any_treat ~ Year + WHO_Subregion + GDPGrowth + HealthExTotal + PregWomenCare + PrimaryComplete, data = cleandata_any_2000)

summary(linear_mod_any)

#########
#Plot the Year effects

pdf('Any_treat_Year_effect.pdf',width=8.7,height = 11.2)


par(mfrow=c(2,1))
par(mar=c(4,4,1,1))

plot(chosen_model_any, main = "Any_treat Data from 1995") # Not linear! Plateaus off...

plot(chosen_model_any_2000, main = "Any_treat Data from 2000") # Slightly insignificant linear upwards trend.

dev.off()

# Just use data from 2000 onwards as originally written in paper?


################## National HMIS proportion treatment seeking model #######################

formula_hmis1 <-  HMIS_frac ~ WHO_Subregion + s(Year) + GDPGrowth +  HealthExPub + PregWomenCare + PrimaryComplete + RuralPop + DPT + NurseMidwives + Out_pocket

# HMIS frac (data from 1995):

model_hmis1 <- uGamm(formula_hmis1, data=cleandata_any)

summary(model_hmis1)

model.select.any <- dredge(model_hmis1)

subset(model.select.any, delta < 2) #takes the best model with delta AICc less than 2

best_hmis1 <- HMIS_frac ~ s(Year) + WHO_Subregion + HealthExPub + PregWomenCare + PrimaryComplete

chosen_model_hmis <- gam(best_hmis1, data = cleandata_any)

# HMIS frac (data from 2000):

model_hmis1_2000 <- uGamm(formula_hmis1, data=cleandata_any_2000)

summary(model_hmis1_2000)

model.select.hmis_2000 <- dredge(model_hmis1_2000)

subset(model.select.hmis_2000, delta < 2) #takes the best model with delta AICc less than 2

best_hmis1_2000 <- HMIS_frac ~ s(Year) + WHO_Subregion + GDPGrowth + DPT + HealthExPub + PregWomenCare # Same model formula!

chosen_model_hmis_2000 <- gam(best_hmis1_2000, data = cleandata_any_2000)


#########
#Plot the Year effects

pdf('HMIS_frac_Year_effect.pdf',width=8.7,height = 11.2)


par(mfrow=c(2,1))
par(mar=c(4,4,1,1))

plot(chosen_model_hmis, main = "HMIS_frac Data from 1995")

plot(chosen_model_hmis_2000, main = "HMIS_frac Data from 2000") 

dev.off()



#########
#Plot the true data versus the predicted data


pdf('True_vs_predicted.pdf',width=8.7,height = 11.2)

par(mfrow=c(2,1))
par(mar=c(4,4,1,1))

#Any
plot(cleandata_any_2000$Any_treat*100,chosen_model_any_2000$fitted.values*100, xlab="Observed values (any treatment)", 
     ylab="Fitted values",
     main="", xlim=c(0,100), ylim=c(0,100))
abline(0,1, col="blue")

#HMIS
plot(cleandata_any_2000$HMIS_frac*100,chosen_model_any_2000$fitted.values*100, xlab="Observed values (HMIS fraction)", 
     ylab="Fitted values",
     main="", xlim=c(0,100), ylim=c(0,100))
abline(0,1, col="blue")

dev.off()

# HMIS fraction model much worse at predicting. 

##### Force fit models that Donal was working with:

donal_any <- Any_treat ~ s(Year) + PrimaryComplete + PregWomenCare
  
donal_hmis <- HMIS_frac ~ s(Year) + HealthExPub + PregWomenCare + Out_pocket + WHO_Subregion

donal_any_model <- gam(donal_any, data = cleandata_any)
donal_any_model_2000 <- gam(donal_any, data = cleandata_any_2000)

summary(donal_any_model)
summary(donal_hmis_model) # Out of pocket is not significant!

pdf('Donal_treat_Year_effect.pdf',width=8.7,height = 11.2)

par(mfrow=c(2,1))
par(mar=c(4,4,1,1))

plot(donal_any_model, main = "Any_treat Data from 1995")
plot(donal_any_model_2000, main = "Any_treat Data from 2000") # Use Donal's model with data from 2000 but can't say that used AICc and best model?

dev.off()

donal_hmis_model <- gam(donal_hmis, data = cleandata_any)
donal_hmis_model_2000 <- gam(donal_hmis, data = cleandata_any_2000)  # Use Donal's model with data from 2000 but can't say that used AICc and best model?

pdf('Donal_HMIS_frac_Year_effect.pdf',width=8.7,height = 11.2)

par(mfrow=c(2,1))
par(mar=c(4,4,1,1))

plot(donal_hmis_model, main = "HMIS_frac Data from 1995")
plot(donal_hmis_model_2000, main = "HMIS_frac Data from 2000")

dev.off()

# Q: Did he use a different technique to select models? E.g. forward selection by AIC?


#########
#Plot the true data versus the predicted data


pdf('Donal_true_vs_predicted.pdf',width=8.7,height = 11.2)

par(mfrow=c(2,1))
par(mar=c(4,4,1,1))

#Any
plot(cleandata_any_2000$Any_treat*100,donal_any_model_2000$fitted.values*100, xlab="Observed values (any treatment)", 
     ylab="Fitted values",
     main="", xlim=c(0,100), ylim=c(0,100))
abline(0,1, col="blue")

#HMIS
plot(cleandata_any_2000$HMIS_frac*100,donal_hmis_model_2000$fitted.values*100, xlab="Observed values (HMIS fraction)", 
     ylab="Fitted values",
     main="", xlim=c(0,100), ylim=c(0,100))
abline(0,1, col="blue")

dev.off()

# Somehow Donal's model performs better for HMIS fraction than AICc best model? 

