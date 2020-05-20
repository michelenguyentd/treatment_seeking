#### Plot treatment seeking results from previous GBD run ####

rm(list = ls())
setwd("J:/Treatment_Seeking")

prev_treat_any <- read.csv("Z:/GBD2017/Processing/Stages/02b_Model_TS/Verified_Outputs/treat_seek_any_no_year_19_Jul_2017.csv")
prev_pub_treat <-read.csv("Z:/GBD2017/Processing/Stages/02b_Model_TS/Verified_Outputs/treat_seek_pub_no_year_19_Jul_2017.csv")

prev_sub_treat_any <- read.csv("Z:/GBD2017/Processing/Stages/02b_Model_TS/Verified_Outputs/sub_national_treat_seek_any_no_year_01_Feb_2017.csv")
prev_sub_pub_treat <-read.csv("Z:/GBD2017/Processing/Stages/02b_Model_TS/Verified_Outputs/sub_national_treat_seek_pub_no_year_01_Feb_2017.csv")

TreatSeek <- read.csv("Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/NationalSurveyResults_Weighted_With_UpdatedIndicators.csv") # ADMIN0. 
str(TreatSeek) # Country_Name is already a Factor.

# Only use DHS and MIS surveys, remove Tanzania 2015 DHS survey and India 1993 survey:
TreatSeek <- TreatSeek[TreatSeek$Year>1995 & as.character(TreatSeek$SurveyType) %in% c("DHS", "MIS") & as.character(TreatSeek$SurveyName) != "TZ2015DHS" & as.character(TreatSeek$SurveyName) != "IA1993DHS", ]
TreatSeek <- droplevels(TreatSeek)
str(TreatSeek)

sub_national_india_1999<-read.csv('Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/FST_ByRegion_156_IA1999DHS.csv')
sub_national_india_2006<-read.csv('Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/FST_ByRegion_264_IA2006DHS.csv')
sub_national_india_2015<-read.csv('Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/FST_ByRegion_355_IA2015DHS.csv')
sub_national_india <-rbind(sub_national_india_1999, sub_national_india_2006, sub_national_india_2015)

names(sub_national_india) <- c(names(sub_national_india)[1:3], "Year", names(sub_national_india)[5:9], "map_state", names(sub_national_india)[11:15], 
                               "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY") 

head(sub_national_india)

subnational = TRUE

########## Any ##########

if (subnational == TRUE){
  
  cleandata <- sub_national_india
  treat_results <- prev_sub_treat_any # What to plot.

  
  pdf('prev_india_sub_treat_seek_any.pdf',width=8.7,height = 11.2)
  
}else{
  
  pdf('prev_treat_seek_any.pdf',width=8.7,height = 11.2)
  
  cleandata <- droplevels(TreatSeek[,c('Any_treat','WHO_Subregion','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country_Name','ISO3','Any_treat_low_SVY', 'Any_treat_high_SVY','SurveyType')])
  treat_results <- prev_treat_any # What to plot
  cleandata$ISO3 <- factor(cleandata$ISO3, levels = levels(treat_results$iso))
  
}

treat_results <- treat_results[order(as.character(treat_results$iso), treat_results$year), ]

cleandata$point_color<-'red'

cleandata$point_color[cleandata$SurveyType == 'MIS']<-'blue'


par(mfrow=c(3,2))

for( i in unique(treat_results$iso) ){
  
  country_db<-treat_results[ treat_results$iso==i, ]
  
  plot(0,0,type='n',ylim=c(0,1),xlim=c(1980,2016),main=i,ylab='% Sought Treatment',xlab='Year')
  
  plotCI(1980:2016,country_db$mean,ui=country_db$upper,li=country_db$lower,add=T)
  
  abline(h=0.5,col=3,lwd=1.5,lty=2)
  
  if (subnational == TRUE){
    in_out<-which(cleandata$map_state==i)
    
    if(length(in_out)>0){
      
      country_line<- cleandata[which(cleandata$map_state==i),]
      
      plotCI(as.numeric(country_line$Year),country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,add=T)
      
      points(country_line$Year,country_line$Any_treat,pch=21,bg=country_line$point_color)
      
    }
  }else{
    in_out<-which(cleandata$ISO3==i)
    
    if(length(in_out)>0){
      
      
      country_line<- cleandata[which(cleandata$ISO3==i),]
      
      plotCI(as.numeric(country_line$Year),country_line$Any_treat,ui=country_line$Any_treat_high_SVY,li=country_line$Any_treat_low_SVY,add=T)
      
      points(country_line$Year,country_line$Any_treat,pch=21,bg=country_line$point_color)
      
    }
  }
  
  
  if(i=='AFG'){
    
    legend('topleft',legend=c('DHS','MIS'),pt.bg=c('red','blue'),pch=21)
    
    
  }
  
}

dev.off()

############ HMIS #########

if (subnational == TRUE){
  
  cleandata <- sub_national_india
  
  treat_results <- prev_sub_pub_treat
  
  pdf('prev_india_sub_pub_treat.pdf',width=8.7,height = 11.2)
  
}else{  cleandata <- droplevels(TreatSeek[,c('Any_treat','HMIS_treat','WHO_Subregion','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country_Name','ISO3','Any_treat_low_SVY', 'Any_treat_high_SVY','HMIS_treat_low_SVY', 'HMIS_treat_high_SVY','SurveyType')])

  treat_results <- prev_pub_treat

  cleandata$ISO3 <- factor(cleandata$ISO3, levels = levels(treat_results$iso))

  pdf('prev_treat_pub_treat.pdf',width=8.7,height = 11.2)

}

treat_results <- treat_results[order(as.character(treat_results$iso), treat_results$year), ]


cleandata$point_color<-'red'

cleandata$point_color[cleandata$SurveyType == 'MIS']<-'blue'

par(mfrow=c(3,2))

for( i in unique(treat_results$iso) ){
  
  country_db<-treat_results[ treat_results$iso==i, ]
  
  plot(0,0,type='n',ylim=c(0,1),xlim=c(1980,2016),main=i,ylab='% Sought Public Treatment',xlab='Year')
  
  plotCI(1980:2016,country_db$mean,ui=country_db$upper,li=country_db$lower,add=T)
  
  abline(h=0.5,col=3,lwd=1.5,lty=2)
  
  if (subnational == TRUE){
    in_out<-which(cleandata$map_state==i)
    
    if(length(in_out)>0){
      
      country_line<- cleandata[which(cleandata$map_state==i),]
      
      plotCI(as.numeric(country_line$Year),country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,add=T)
      
      points(country_line$Year,country_line$HMIS_treat,pch=21,bg=country_line$point_color)
      
    }
  }else{
    in_out<-which(cleandata$ISO3==i)
    
    if(length(in_out)>0){
      
      
      country_line<- cleandata[which(cleandata$ISO3==i),]
      
      plotCI(as.numeric(country_line$Year), country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,add=T)
      
      points(country_line$Year,country_line$HMIS_treat,pch=21,bg=country_line$point_color)
      
    }
  }
  
  
  if(i=='AFG'){
    
    legend('topleft',legend=c('DHS','MIS'),pt.bg=c('red','blue'),pch=21)
    
    
  }
  
}

dev.off()
