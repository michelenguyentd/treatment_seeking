##Building treatment seeking model

#load libraries
library(mgcv) #for GAMM
library(MuMIn) #for AIC (model selection)

#import dataset
rm(list = ls())
#set working directory and read in the dataset
setwd("C:/Battle/Dropbox/04 API Adjustments/Data")
#setwd("C:/Users/Katherine Battle/Dropbox/03 PvPR update/API/World Bank National Data/")
TreatSeek <- read.csv("DHS_MICS_national_alldata_withmissing3.csv")


#Shorten some of the long country names
TreatSeek$Country <- as.character(TreatSeek$Country)
TreatSeek$Country[TreatSeek$Country == "Congo, Democratic Republic"] <- "DRC"
TreatSeek$Country[TreatSeek$Country == "Dominican Republic"] <- "Dominican Rep."
TreatSeek$Country[TreatSeek$Country == "Tanzania (United Republic of)"] <- "Tanzania"
TreatSeek$Country[TreatSeek$Country == "Lao People's Democratic Republic"] <- "Lao PDR"

TreatSeek$Country <- as.factor(TreatSeek$Country)

#Add a column for country and year together
TreatSeek$CountryYear <- paste(TreatSeek$Country, TreatSeek$Year, sep=" ")


#remove NA values (Zim does't have any health expenditure data)
cleandata <- droplevels(na.omit(TreatSeek))
#summary(cleandata$WHO_Sub)
#write.csv(cleandata, file = "C:/Battle/Dropbox/03 PvPR update/API/World Bank National Data/cleandata.csv")


########Seperate model selection for HMIS treatment seeking and Any treatment seeking#############
#HMIS_treat
formula_hmis1 <-  HMIS_treat ~ WHO_Sub + s(Year) + GDPGrowth +  HealthExPub + PregWomenCare + PrimaryComplete + RuralPop + DPT + NurseMidwives

#formula_hmis1 <-  HMIS_treat ~ WHO_Sub + s(Year) + GDPGrowth +  HealthExPub + PregWomenCare + PrimaryComplete + RuralPop

model_hmis1 = uGamm(formula_hmis1, data=cleandata, random=list(Country=~1) )

summary(model_hmis1)
summary(model_hmis1$lme)
summary(model_hmis1$gam)

model.select.hmis <- dredge(model_hmis1, fixed=c("WHO_Sub"), m.min=4)
model.select.hmis[1:10]
summary(model.select.hmis[1])

best.hmis<-subset(model.select.hmis, delta < 2.08) #takes the best model with delta AICc less than 2
hmis.avg<-model.avg(model.select.hmis, delta < 2.08, fit=TRUE) #finds the model average of those

best.hmis
hmis.avg

summary(hmis.avg)

avgmod.95p.hmis <- model.avg(best.hmis, cumsum(weight) <= .95) #calculates the 95% CI for averaged model

confint(avgmod.95p.hmis) #shows the 95% CI

summary(model.avg(best.hmis))

#Any_treat
formula_any1 <-  Any_treat ~ WHO_Sub + s(Year) + GDPGrowth +  HealthExTotal + PregWomenCare + PrimaryComplete + RuralPop + DPT + NurseMidwives

model_any1 = uGamm(formula_any1, data=cleandata, random=list(Country=~1) )

summary(model_any1)
summary(model_any1$lme)
summary(model_any1$gam)

model.select.any <- dredge(model_any1, fixed=c("WHO_Sub"), m.min=4)
model.select.any[1:10]

best.any<-subset(model.select.any, delta < 2) #takes the best model with delta AICc less than 2
any.avg<-model.avg(model.select.any, delta < 2, fit=TRUE) #finds the model average of those

best.any
any.avg

avgmod.95p.any <- model.avg(best.any, cumsum(weight) <= .95) #calculates the 95% CI for averaged model

confint(avgmod.95p.any) #shows the 95% CI

summary(any.avg)

############################################################################################################
###########################SEE SIMULATIONS CODE#############################################################

###Generate predictions from both best model averages
missingdata <- read.csv("DataMECs_missing_mod3.csv")
missingdata_cl <- droplevels(na.omit(missingdata))

##HMIS treatment
pred.hmis <- predict(hmis.avg, cleandata, se.fit=TRUE)
pred.hmis.miss <- predict(hmis.avg, na.omit(missingdata), se.fit=TRUE)
#plot prediction versus true numbers
plot(pred.hmis$fit, cleandata$HMIS_treat)

#Add a column to the missingdata that has the fit and the treatment seeking
missingdata_cl$HMIS_treat <- pred.hmis.miss$fit*100
#add 95%CI
missingdata_cl$HMIS_treat_low <- ((pred.hmis.miss$fit*100) - (1.96*(pred.hmis.miss$se.fit*100)))
missingdata_cl$HMIS_treat_high <- ((pred.hmis.miss$fit*100) + (1.96*(pred.hmis.miss$se.fit*100)))
missingdata_cl$HMIS_se <- pred.hmis.miss$se.fit*100

##Any treatment 
pred.any.miss <- predict(any.avg, na.omit(missingdata), se.fit=TRUE)
#Add a column to the missingdata that has the fit and the treatment seeking
missingdata_cl$Any_treat <- pred.any.miss$fit*100
#add 95%CI
missingdata_cl$Any_treat_low <- ((pred.any.miss$fit*100) - (1.96*(pred.any.miss$se.fit*100)))
missingdata_cl$Any_treat_high <- ((pred.any.miss$fit*100) + (1.96*(pred.any.miss$se.fit*100)))
missingdata_cl$Any_se <- pred.any.miss$se.fit*100

#export predicted missing data
#write.csv(missingdata_cl, file = "C:/Users/Katherine Battle/Dropbox/03 PvPR update/API/World Bank National Data/predictedmissingdata.csv")

write.csv(missingdata_cl, file = "C:/Battle/Dropbox/03 PvPR update/API/World Bank National Data/predictedmissingdata270116.csv")


###################################################################################################################
###################################################################################################################



#plot predicted HMIS treatment for each country
library(ggplot2)
library(RColorBrewer)
library(grid)

#use color brewer color scale for WHO regions to be able to tell them apart
display.brewer.pal(n = 9, name = 'Set1')
Colors <- brewer.pal(n = 9, name = "Set1")
names(Colors) <- levels(cleandata$WHO_Sub)
colScale <- scale_colour_manual(name = "Region",values = Colors)

par(mfrow=c(1,1))
  limits.hmis <- aes(ymax =missingdata_cl$HMIS_treat_high, ymin=missingdata_cl$HMIS_treat_low)
  ggplot(missingdata_cl, aes(Country, HMIS_treat)) +
    geom_errorbar(limits.hmis, width=0.25) +
    geom_point((aes(colour = WHO_Sub)), size=4) +
    colScale +
    #scale_colour_brewer(expression(bold("Region")), palette="Set1")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
    ylab("% seeking government treatment") +
    xlab("")+
    ylim(0,110)


limits.any <- aes(ymax =missingdata_cl$Any_treat_high, ymin=missingdata_cl$Any_treat_low)
ggplot(missingdata_cl, aes(Country, Any_treat)) +
  geom_errorbar(limits.any, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  colScale +
  #scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110)

###same plots with panels for the regions
ggplot(missingdata_cl, aes(Country, HMIS_treat)) +
  facet_grid(.~WHO_Sub, scales="free", space="free") +
  geom_errorbar(aes(ymax =HMIS_treat_high, ymin=HMIS_treat_low), width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  colScale +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking government treatment") +
  xlab("")+
  ylim(0,110) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())+
  theme(plot.margin = unit(c(-0.75,0,0,0), "lines" ) )


ggplot(missingdata_cl, aes(Country, Any_treat)) +
  geom_errorbar(aes(ymax =Any_treat_high, ymin=Any_treat_low), width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  facet_grid(.~WHO_Sub, scales="free", space="free") +
  colScale +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())+
  theme(plot.margin = unit(c(-0.75,0,0,0), "lines" ) )

##Compare these results with a random sample of the other data
#load modified data
treatseek_mod <- read.csv("DataMECs_All_mod.csv")
#creat a new column that combines the country and year columns
treatseek_mod$CountryYear <- paste(treatseek_mod$Country, treatseek_mod$Year, sep=" ")
missingdata_cl$CountryYear <- paste(missingdata_cl$Country, missingdata_cl$Year, sep=" ")

treatseek_mod$HMIS_treat <- treatseek_mod$HMIS_treat*100
treatseek_mod$Any_treat <- treatseek_mod$Any_treat*100

######take a random subset of the "true data"
treat_samp_rand <- treatseek_mod[sample(1:nrow(treatseek_mod), 30, replace=FALSE),]

#rbind the predicted data with the subset of the "true" data
binded_rand <- rbind(treat_samp_rand, missingdata_cl)

#Plot all the data
limits.hmis.bind.r <- aes(ymax =binded_rand$HMIS_treat_high, ymin=binded_rand$HMIS_treat_low)
ggplot(binded_rand, aes(Country, HMIS_treat)) +
  geom_errorbar(limits.hmis.bind.r, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking government treatment") +
  xlab("")+
  ylim(0,110)

limits.any.bind.r <- aes(ymax =binded_rand$Any_treat_high, ymin=binded_rand$Any_treat_low)
ggplot(binded_rand, aes(Country, Any_treat)) +
  geom_errorbar(limits.any.bind.r, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110)


######Only with post 2010 data
treat_samp <- subset(treatseek_mod, Year>=2010)
#rbind the predicted data with the subset of the "true" data
firstbind <- rbind(treat_samp, missingdata_cl)

write.csv(binded, file = "C:/Battle/Dropbox/03 PvPR update/API/World Bank National Data/binded.csv",  row.names = FALSE)
binded <- read.csv("binded.csv")
#Plot all the data
limits.hmis.bind <- aes(ymax =binded$HMIS_treat_high, ymin=binded$HMIS_treat_low)
ggplot(binded, aes(Country, HMIS_treat)) +
  facet_grid(.~WHO_Sub, scales="free", space="free") +
  geom_errorbar(limits.hmis.bind, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking government treatment") +
  xlab("")+
  ylim(0,110) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())+
  theme(plot.margin = unit(c(-0.75,0,0,0), "lines" ) )
  

limits.any.bind <- aes(ymax =binded$Any_treat_high, ymin=binded$Any_treat_low)
ggplot(binded, aes(Country, Any_treat)) +
  facet_grid(.~WHO_Sub, scales="free", space="free") +
  geom_errorbar(limits.any.bind, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110) +
  theme(strip.text.y = element_blank()) +
  theme(strip.background = element_blank())+
  theme(plot.margin = unit(c(-0.75,0,0,0), "lines" ) )


######
#Predict data for the "true" data and plot together
###Generate predictions from both best model averages
treatseek_cl <- read.csv("DataMECs_All_withmissing.csv")
treatseek_cl <- droplevels(na.omit(treatseek_cl))
treatseek_cl$HMIS_treat <- treatseek_cl$HMIS_treat*100
treatseek_cl$Any_treat <- treatseek_cl$Any_treat*100

##HMIS treatment
pred.hmis.ts <- predict(hmis.avg, treatseek_cl, se.fit=TRUE)

#Add a column to the missingdata that has the fit and the treatment seeking
treatseek_cl$HMIS_treat_pred <- pred.hmis.ts$fit*100
#add 95%CI
treatseek_cl$HMIS_treat_low <- ((pred.hmis.ts$fit*100) - (1.96*(pred.hmis.ts$se.fit*100)))
treatseek_cl$HMIS_treat_high <- ((pred.hmis.ts$fit*100) + (1.96*(pred.hmis.ts$se.fit*100)))


##Any treatment 
pred.any.ts <- predict(any.avg, treatseek_cl, se.fit=TRUE)
#Add a column to the missingdata that has the fit and the treatment seeking
treatseek_cl$Any_treat_pred <- pred.any.ts$fit*100
#add 95%CI
treatseek_cl$Any_treat_low <- ((pred.any.ts$fit*100) - (1.96*(pred.any.ts$se.fit*100)))
treatseek_cl$Any_treat_high <- ((pred.any.ts$fit*100) + (1.96*(pred.any.ts$se.fit*100)))

##plotting just the predicted data
limits.hmis.ts <- aes(ymax =treatseek_cl$HMIS_treat_high, ymin=treatseek_cl$HMIS_treat_low)
ggplot(treatseek_cl, aes(Country, HMIS_treat_pred)) +
  #geom_errorbar(limits.hmis.ts, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking HMIS treatment") +
  xlab("")+
  ylim(0,110)+
  geom_point(aes(y = HMIS_treat), size=3)


limits.any.ts <- aes(ymax =treatseek_cl$Any_treat_high, ymin=treatseek_cl$Any_treat_low)
ggplot(treatseek_cl, aes(Country, Any_treat_pred)) +
  #geom_errorbar(limits.any.ts, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110)+
  geom_point(aes(y = Any_treat), size=3)


#plot again, but with each country/year separated
#creat a new column that combines the country and year columns
treatseek_cl$CountryYear <- paste(treatseek_cl$Country, treatseek_cl$Year, sep=" ")
#take a subset of this data
treatseek_cl_rand <- treatseek_cl[sample(1:nrow(treatseek_cl), 30, replace=FALSE),]

limits.hmis.ts.rd <- aes(ymax =treatseek_cl_rand$HMIS_treat_high, ymin=treatseek_cl_rand$HMIS_treat_low)
ggplot(treatseek_cl_rand, aes(CountryYear, HMIS_treat_pred)) +
  geom_errorbar(limits.hmis.ts.rd, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4)) +
  ylab("% seeking government treatment") +
  xlab("")+
  #ylim(0,110)+
  geom_point(aes(y = HMIS_treat), size=3)


limits.any.ts.rd <- aes(ymax =treatseek_cl_rand$Any_treat_high, ymin=treatseek_cl_rand$Any_treat_low)
ggplot(treatseek_cl_rand, aes(CountryYear, Any_treat_pred)) +
  geom_errorbar(limits.any.ts.rd, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  scale_colour_brewer(expression(bold("Region")), palette="Set1")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110)+
  geom_point(aes(y = Any_treat), size=3)


#mean square error for the average models
#r.rmse <- sqrt(mean(model$residuals^2))
library(hydroGOF)
hmis.rmse <- rmse(sim=treatseek_cl$HMIS_treat_pred, obs=treatseek_cl$HMIS_treat)
hmis.rmse

any.rsme <- rmse(sim=treatseek_cl$Any_treat_pred, obs=treatseek_cl$Any_treat)
any.rsme

#calculate and plot residuals
par(mfrow=c(2,2))
par(mar=c(7,4,2,1))
#HMIS and any
hmis.r <- (treatseek_cl$HMIS_treat - treatseek_cl$HMIS_treat_pred)
any.r <- (treatseek_cl$Any_treat - treatseek_cl$Any_treat_pred)

# plot(treatseek_cl$Country, hmis.r, ylab="Residuals", xlab="", cex.axis=0.5, las=2,
#      main="Residuals (government) verus country")
# abline(0, 0) 

# plot(treatseek_cl$Country, any.r, ylab="Residuals", xlab="", cex.axis=0.5, las=2,
#      main="Residuals (any) verus country")
# abline(0, 0)  

plot(treatseek_cl$HMIS_treat_pred, hmis.r, ylab="Residuals", xlab="Fitted value", 
     main="Residuals versus fitted public values (public)")
abline(0, 0) 

plot(treatseek_cl$Any_treat_pred, hmis.r, ylab="Residuals", xlab="Fitted value", 
     main="Residuals versus fitted values (any)")
abline(0, 0)

hist(hmis.r, main="Histogram of residuals (public)", xlim=c(-30,30), xlab="Residuals")

hist(any.r, main="Histogram of residuals (any)", xlim=c(-30, 40), xlab="Residuals")
# 
# plot(treatseek_cl$PregWomenCare, hmis.r, ylab="Residuals", xlab="Independent variable (pregnant women care)", 
#      main="Residuals versus independent variable")
# abline(0, 0) 


# plot(treatseek_cl$PregWomenCare, any.r, ylab="Residuals", xlab="Independent variable (pregnant women care)", 
#      main="Residuals versus independent variable")
# abline(0, 0) 

#########
#Plot the true data versus the predicted data
par(mfrow=c(2,1))
par(mar=c(4,4,1,1))
#HMIS
plot(treatseek_cl$HMIS_treat,treatseek_cl$HMIS_treat_pred, xlab="Observed values (public treatment)", 
     ylab="Fitted values",
     main="", xlim=c(0,100), ylim=c(0,100))
abline(0,1, col="blue")

#Any
plot(treatseek_cl$Any_treat,treatseek_cl$Any_treat_pred, xlab="Observed values (any treatment)", 
     ylab="Fitted values",
     main="", xlim=c(0,100), ylim=c(0,100))
abline(0,1, col="blue")

##Identify what rows were dropped
treatseek_mod$CountryYear[!treatseek_mod$CountryYear %in% cleandata$CountryYear]






####################################################################################################################################
###Checking predictions

#Public treatment

# creating the list of the five best models

formula_hmis1 <-  HMIS_treat ~ WHO_Sub + s(Year) + PregWomenCare + PrimaryComplete

formula_hmis2 <-  HMIS_treat ~ WHO_Sub + s(Year) + HealthExPub + PregWomenCare + PrimaryComplete

formula_hmis3 <-  HMIS_treat ~ WHO_Sub + s(Year) + HealthExPub + PregWomenCare 

formula_hmis4 <-  HMIS_treat ~ WHO_Sub + s(Year) + GDPGrowth + PregWomenCare + PrimaryComplete 

formula_hmis5 <-  HMIS_treat ~ WHO_Sub + s(Year) + GDPGrowth +  HealthExPub + PregWomenCare + PrimaryComplete 

# creating training and prediction dataset

set.seed(1235) # setting the seed to have same results from the code

n_row<-sample(c(1:dim(cleandata)[1]),round(dim(cleandata)[1]*0.7))

training_db<-cleandata[n_row,]

prediction_db<-cleandata[-c(n_row),]

prediction_db<-prediction_db[order(prediction_db$Country,prediction_db$Year),]

# creating the model list

results=list() 

results[[1]]= gamm(formula_hmis1, data=training_db, random=list(Country=~1) )
results[[2]]= gamm(formula_hmis2, data=training_db, random=list(Country=~1) )
results[[3]]= gamm(formula_hmis3, data=training_db, random=list(Country=~1) )
results[[4]]= gamm(formula_hmis4, data=training_db, random=list(Country=~1) )
results[[5]]= gamm(formula_hmis5, data=training_db, random=list(Country=~1) )

# model averaging

msAICc <- model.avg(results)

# prediction with 955 CI

model_se<-as.numeric(predict(msAICc,newdata=prediction_db,se.fit=T)$se.fit)*100

model_prediction<-as.numeric(predict(msAICc,newdata=prediction_db))*100

model_prediction[model_prediction<0]<-0

prediction_db$HMIS_treat_pred <- model_prediction

up_CI<-model_prediction+model_se*1.96

up_CI[up_CI>100]<-100

prediction_db$HMIS_treat_high <- up_CI

low_CI<-model_prediction-model_se*1.96

low_CI[low_CI<0]<-0

prediction_db$HMIS_treat_low <- low_CI

# plotting results

library(plotrix)

survey_labels<-paste(substr(prediction_db$Country,1,4),prediction_db$Year,sep='_')

plotCI(1:57,model_prediction,ui=up_CI,li=low_CI,pt.bg=2,pch=21,ylim=c(0,100),main='Public',axes=F,ann=F)  

points(prediction_db$HMIS_treat*100,pch=21,bg=3)

axis(1,at=1:57,labels=survey_labels,las=2,cex.axis=0.7)

axis(2)

box()

##again using ggplot for consistency with other plots
library(ggplot2)
library(RColorBrewer)
library(grid)

display.brewer.pal(n = 9, name = 'Set1')
Colors <- brewer.pal(n = 9, name = "Set1")
names(Colors) <- levels(cleandata$WHO_Sub)
colScale <- scale_colour_manual(name = "Region",values = Colors)

limits.h <- aes(ymax = prediction_db$HMIS_treat_high, ymin = prediction_db$HMIS_treat_low)
ggplot(prediction_db, aes(CountryYear, HMIS_treat_pred)) +
  geom_errorbar(limits.h, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  colScale +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking government treatment") +
  xlab("")+
  ylim(0,110)+
  geom_point(aes(y = HMIS_treat*100), size=3)



# limits.hmis.ts.rd <- aes(ymax =treatseek_cl_rand$HMIS_treat_high, ymin=treatseek_cl_rand$HMIS_treat_low)
# ggplot(treatseek_cl_rand, aes(CountryYear, HMIS_treat_pred)) +
#   geom_errorbar(limits.hmis.ts.rd, width=0.25) +
#   geom_point((aes(colour = WHO_Sub)), size=4) +
#   scale_colour_brewer(expression(bold("Region")), palette="Set1")+
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4)) +
#   ylab("% seeking government treatment") +
#   xlab("")+
#   #ylim(0,110)+
#   geom_point(aes(y = HMIS_treat), size=3)


#estimating MSE

print(paste('Public MSE=',round(mean(sqrt((prediction_db$HMIS_treat-predict(msAICc,newdata=prediction_db))^2))*100,1),sep=' '))




#Any treatment

## same passages explained for the 'Public' session


formula_any1 <-  Any_treat ~ WHO_Sub + s(Year)+ GDPGrowth + PregWomenCare + PrimaryComplete

formula_any2 <-  Any_treat ~ WHO_Sub + s(Year) + PregWomenCare + PrimaryComplete

formula_any3 <-  Any_treat ~ WHO_Sub + s(Year) + GDPGrowth + PregWomenCare 

formula_any4 <-  Any_treat ~ WHO_Sub + s(Year) + HealthExPub + PregWomenCare + PrimaryComplete 

formula_any5 <-  Any_treat ~ WHO_Sub + s(Year) + GDPGrowth +  HealthExPub + PregWomenCare + PrimaryComplete 

set.seed(12546)
 
n_row<-sample(c(1:dim(cleandata)[1]),round(dim(cleandata)[1]*0.7))

training_db<-cleandata[n_row,]

prediction_db<-cleandata[-c(n_row),]

resultsA=list() 

resultsA[[1]]= gamm(formula_any1, data=training_db, random=list(Country=~1) )
resultsA[[2]] = gamm(formula_any2, data=training_db, random=list(Country=~1) )
resultsA[[3]]= gamm(formula_any3, data=training_db, random=list(Country=~1) )
resultsA[[4]]= gamm(formula_any4, data=training_db, random=list(Country=~1) )
resultsA[[5]]= gamm(formula_any5, data=training_db, random=list(Country=~1) )

msAICcA <- model.avg(resultsA)

model_seA<-as.numeric(predict(msAICcA,newdata=prediction_db,se.fit=T)$se.fit)*100

model_predictionA<-as.numeric(predict(msAICcA,newdata=prediction_db))*100

model_predictionA[model_predictionA<0]<-0

prediction_db$Any_treat_pred <- model_predictionA

up_CIA<-model_predictionA+model_seA*1.96

up_CIA[up_CIA>100]<-100

prediction_db$Any_treat_high <- up_CIA

low_CIA<-model_predictionA-model_seA*1.96

low_CIA[low_CIA<0]<-0

prediction_db$Any_treat_low <- low_CIA

############
library(plotrix)

survey_labels<-paste(substr(prediction_db$Country,1,4),prediction_db$Year,sep='_')

plotCI(1:57,model_prediction,ui=up_CI,li=low_CI,pt.bg=2,pch=21,ylim=c(0,100),main='Public',axes=F,ann=F)  

points(prediction_db$Any_treat*100,pch=21,bg=3)

axis(1,at=1:57,labels=survey_labels,las=2,cex.axis=0.7)

axis(2)

box()
#############

limits.a <- aes(ymax = prediction_db$Any_treat_high, ymin = prediction_db$Any_treat_low)
ggplot(prediction_db, aes(CountryYear, Any_treat_pred)) +
  geom_errorbar(limits.a, width=0.25) +
  geom_point((aes(colour = WHO_Sub)), size=4) +
  colScale +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.25)) +
  ylab("% seeking any treatment") +
  xlab("")+
  ylim(0,110)+
  geom_point(aes(y = Any_treat*100), size=3)

#estimating MSE

print(paste('Any MSE=',round(mean(sqrt((prediction_db$Any_treat-predict(msAICcA,newdata=prediction_db))^2))*100,1),sep=' '))





