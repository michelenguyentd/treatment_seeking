### Sought any treatment ### Original by Donal (last modified 02/02/2017); edited by Michele (24/01/2018). 

## ----------------- Step 1: Set up ---------------- ##

rm(list = ls())
setwd("J:/Treatment_Seeking")

library(mgcv) #for GAMM
library(plotrix)
library(caTools)

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

# Add a column for adding out of pocket:

out_pocket <- read.csv("Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/out_of_pocket.csv", sep = ";")

TreatSeek$Out_pocket<-NA

raw_to_fill<- 1:nrow(TreatSeek)

for (i in raw_to_fill){
  
  row_num<-which(as.character(out_pocket$iso) == as.character(TreatSeek$ISO3[i]) & out_pocket$year == TreatSeek$Year[i])
  
  TreatSeek$Out_pocket[i]<-out_pocket$gp_value[row_num]
  
}

## Prepare subnational india data ##

sub_national_india_1999<-read.csv('Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/FST_ByRegion_156_IA1999DHS.csv')
sub_national_india_2006<-read.csv('Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/FST_ByRegion_264_IA2006DHS.csv')
sub_national_india_2015<-read.csv('Z:/GBD2017/Processing/Stages/02a_Muster_Raw_TS/Checkpoint_Outputs/FST_ByRegion_355_IA2015DHS.csv')
sub_national_india <-rbind(sub_national_india_1999, sub_national_india_2006, sub_national_india_2015)

names(sub_national_india) <- c(names(sub_national_india)[1:3], "Year", names(sub_national_india)[5:9], "map_state", names(sub_national_india)[11:15], 
                               "HMIS_treat", "HMIS_treat_low_SVY", "HMIS_treat_high_SVY", "Any_treat", "Any_treat_low_SVY", "Any_treat_high_SVY") 

head(sub_national_india)
str(sub_national_india)

subnational = TRUE # If predicting for India subnationals only. Repeat for subnational = FALSE for national estimates.


################## National any treatment seeking model #######################

union_national_any <-list()

temptime <- proc.time()[3]

set.seed(1)

for (i_sim in 1 :10){ 
  

  ## ---------------- Step 2a: Clean data (remove NA values for what we need in the national models) ---------------- ##
  
  cleandata_any <- droplevels(na.omit(TreatSeek[,c('Country_Name','ISO3', 'Year', 'PrimaryComplete', 'PregWomenCare', 'Any_treat', 'Any_treat_low_SVY', 'Any_treat_high_SVY')]))
  

  ## --------------- Step 2b: Simulate from data distribution (1/10), i.e. likelihood ------------ ##
  
  any_treat <-numeric()
  
  for (i in 1:dim(cleandata_any)[1]){
    
    mean_y<-cleandata_any$Any_treat[i]
    
    sd_y <- (cleandata_any$Any_treat_high_SVY[i] - cleandata_any$Any_treat_low_SVY[i])/(2*qnorm(0.975)) 
    
    any_treat<- c(any_treat,rnorm(1,mean=mean_y,sd=sd_y)) # any_treat is a vector of simulations from each country's any treatment seeking level. 
  }
  
  cleandata_any$Any_treat<-any_treat # Replace Any_treat data with the simulations in pub_treat.
  
  ## --------------- Step 2c: Fit model (1/10) ------------ ##
  
  
  formula_any <-  Any_treat~  Year + PrimaryComplete + PregWomenCare # Linear trends with respect to these variables and a Gaussian assumption.
  
  chosen_model_any <-gam(formula_any, data=cleandata_any ) # Fit a generalised additive model. 

 
  
  if (subnational == TRUE){
    sub_matrix <- sub_national_india
    sub_matrix$Year <- factor( sub_matrix $Year,levels = 1980:2016) 
    d_m3 <-tapply(sub_matrix$Any_treat,list(sub_matrix$map_state, sub_matrix$Year), mean)
    
      
  }else{
    
    ## --------------- Step 2d: Predict for countries where we have treatment seeking data based on fitted model (1/10)  ------------ ##
    cleandata_any$Year<-factor(cleandata_any$Year,levels = 1980:2016) # Need to be updated every year; so as to get d_m3 matrix.
  
    d_m3 <-tapply(cleandata_any$Any_treat,list(cleandata_any$ISO3,cleandata_any$Year), mean) # Creates a iso3 by year matrix containing the corresponding (mean) any_treat values as elements.
  }

  ## --------------- Step 2di: Simulate from covariate input distributions (10 times in parallel) ------------ ##
  
  file_name<-c('Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/primary_school_jul_2017.csv',
    'Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/pregnant_jul_2017.csv')
  
  column_names<-c('PrimaryComplete' , 'PregWomenCare')
  
  pred_list<- list()
    
    
  for (i_pred in 1:10){ # Previously parallel but hit problems with proper 'qr' component for lm object. 
  
    list_value<-list()
    
    for (i_f in 1:2){ # Two covariates (other than Year).
      
      cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
      
      cov_db <- cov_db[cov_db$year != 2017, ]  # Exclude year 2017 which is not required for GBD2017.
      
      if (subnational == TRUE){ cov_db <- cov_db[cov_db$iso == "IND", ]}
      
      cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
      
      new_value<-numeric()
      
      for (i_iso in sort(unique(cov_db$iso))){
        
        country_db<- cov_db[cov_db$iso==i_iso, ]
        
        
        for (i in 1:37){
          
          new_value<-c(new_value, rnorm(1,mean=country_db$gp_value[i],sd=sqrt(country_db$mse[i]))) 
          
        }
        
      }
      
      list_value[[length(list_value)+1]]<-new_value
    }
    
    pred_model_data<- cbind(cov_db[, 1:3], do.call(cbind,list_value))
    # head(pred_model_data)
    names(pred_model_data)[4:5]<-column_names
    
    ## --------------- Step 2dii: Simulate from the model parameter distributions for fixed model (10 times in parallel - same session as covariate inputs) ------------ ##    
    
    mean_model<-summary(chosen_model_any)$p.table[,1]
    
    sd_model<-summary(chosen_model_any)$p.table[,2]
    
    model_coef2<-numeric()
    
    for (i_m in 1:length(mean_model)){
      
      model_coef2<-c(model_coef2,rnorm(1,mean=mean_model[i_m],sd=sd_model[i_m]))
      
    }
    

    ## --------------- Step 2diii: Gapfill the treatment seeking time series from front, back and interpolate in-between ------------ ##    
    
    
    list_alg<-as.numeric()
    
    for (cnty in rownames(d_m3)){

      # all country
      
      model_coef<-model_coef2
      
      
      if (cnty=='SWZ'){
        model_coef[4]<-0 # Why?

      }
      
      
      d_m_pos <-which(rownames(d_m3)==cnty) # Row index.
      
      if (subnational == TRUE){
        cov_country <-pred_model_data[pred_model_data$iso=="IND", ] 
      }else{cov_country <-pred_model_data[pred_model_data$iso==cnty, ] }

      
      seek_value <-d_m3[d_m_pos,] # Treatment-seeking data.
      
      year_data<-37-length(which(is.na(seek_value))) # Number of years with data.
      
      pos_data<-which(seek_value>0) # Where we have data.
      
      # Start point
      
      start_na<-as.numeric()
      
      value=1 # Find first data point.
      
      while (is.na(seek_value[value]))
      {  
        start_na<-c(start_na,value)
        
        value=value+1 
      } 
      
      for (v in rev(start_na)) # Fill up points before first data point using national model.
      {  
        
        year_cov1<-cov_country[value, ]
        
        # year_cov1<-cov_country[v+1, ]
        
        year_cov2<-cov_country[v, ]
        
        intercept_rand<-seek_value[value]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
        
        missing_data<-intercept_rand+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
        
        
        seek_value[v]<-missing_data
        
      }
      
      
      #end point
      
        end_na<-as.numeric()
        
        value=length(seek_value)  
        
        while (is.na(seek_value[value])) # Fill up points from the end towards the last data point using national model.
        {  
          end_na<-c(end_na,value)
          
          value=value-1
        } 
        
        
        
        if (value<37) # value is not the last data point. 
          
        {
          
          for (v in rev(end_na)) # points to fill up.
            
          {
            
            
            year_cov1<-cov_country[v-1, ]
            
            year_cov2<-cov_country[v, ]
            

            intercept_rand<-seek_value[value]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
            
            missing_data<-intercept_rand+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            seek_value[v]<-missing_data
            
          }
          
          
        }

      #  missing value in the middle of time series
      
      
      if(sum(as.numeric(is.na(seek_value)))>0)
        
      {
        
        
        empty_v<-which(is.na(seek_value))
        
        middle_na<-as.numeric()
        
        value=length(seek_value)  
        
        
        # first primer
        
        
        seek_value2<-seek_value
        
        front_back<-as.numeric()
        
        for (v in 1:value)
        {  
          
          #print (v)
          
          if (is.na(seek_value2[v])){
            
            prev<-v-1
            
            after_value<-NA
            
            n=1
            
            while ( is.na(after_value))
            {
              
              
              after<-v+n
              
              after_value<-seek_value2[after]
              
              n=n+1
            }
            
            
            year_cov1<-cov_country[prev, ]
            
            year_cov2<-cov_country[v, ]
            
            year_cov3<-cov_country[after, ]
            
            intercept_rand1<-seek_value2[prev]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
            
            missing_data1<-intercept_rand1+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            intercept_rand2<-seek_value2[after]-(year_cov3[,3]*model_coef[2])-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])
            
            missing_data2<-intercept_rand2+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            exp_mid3<-(missing_data1+missing_data2)/2
            
            
            seek_value2[v]<-exp_mid3
            
            front_back<-c(front_back,exp_mid3)
            
            
          }
        } 
        
        
        
        # second primer
        
        
        seek_value3<-seek_value
        
        
        back_front<-as.numeric()
        
        for (v in value:1)
        {  
          
          # print (v)
          
          if (is.na(seek_value3[v])){
            
            after<-v+1
            
            pre_value<-NA
            
            n=1
            
            while ( is.na(pre_value))
            {
              
              
              prev<-v-n
              
              pre_value<-seek_value3[prev]
              
              n=n+1
            }
            
            
            year_cov1<-cov_country[prev, ]
            
            year_cov2<-cov_country[v, ]
            
            year_cov3<-cov_country[after, ]
            
            intercept_rand1<-seek_value3[prev]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
            
            missing_data1<-intercept_rand1+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            intercept_rand2<-seek_value3[after]-(year_cov3[,3]*model_coef[2])-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])
            
            missing_data2<-intercept_rand2+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            
            exp_mid3<-(missing_data1+missing_data2)/2
            
            seek_value3[v]<-exp_mid3
            
            
            back_front<-c(back_front,exp_mid3)
            
            
          }
        } 
        
        
        
        seek_value<-(seek_value2+seek_value3)/2
        
        
      }
      
      #plot(1980:2016,seek_value,ylim=c(0,1),main=cnty)
      #points(1980:2016, d_m3[d_m_pos,] ,pch=21,bg=3)
      #lines(1980:2016, runmean(seek_value, 5), col = 2)
        
      list_alg<-c(list_alg,runmean(seek_value,5)) # moving window mean. why? to make it smoother?
      
    } 
    pred_list[[i_pred]] <- list_alg
}


first_sim_res<-do.call(cbind,pred_list)  

pred_algo_data<-data.frame(iso=rep(rownames(d_m3),each=37),year= rep(1980:2016,length(rownames(d_m3))))

pred_algo_data<-cbind(pred_algo_data,first_sim_res)
    

## --------------- Step 2div: Predict the treatment seeking time series for countries with no data using model ------------ ##    

if (subnational == TRUE){
  union_national_any[[length(union_national_any) + 1]] <- pred_algo_data
}else{ extra_countries<-sort(gdb_2016_country_list[-c(match(rownames(d_m3),gdb_2016_country_list))])
  
  if (length(extra_countries) != 0){
    pred_list2<-list()
    
    for (i_pred2 in 1:10){ 
      
      list_value<-list()
      
      for (i_f in 1:2){
        
        cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
        
        cov_db <- cov_db[cov_db$year != 2017, ]  # Exclude year 2017 which is not required for GBD2017.
        
        cov_db<-cov_db[cov_db$iso %in% extra_countries,]
        
        cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
        
        new_value<-numeric()
        
        for (i_iso in sort(unique(cov_db$iso))){
          
          country_db<- cov_db[cov_db$iso==i_iso,]
          
          for ( i in 1:37){
            
            new_value<-c(new_value, rnorm(1,mean=country_db$gp_value[i],sd=sqrt(country_db$mse[i]))) # Simulate from covariate distribution.
            
          }
          
        }
        
        list_value[[length(list_value)+1]]<-new_value
        
      }
      
      cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
      
      pred_model_data<-cbind(cov_db[,1:3],do.call(cbind,list_value))
      
      names(pred_model_data)<-c('Country','iso','Year',column_names)
      
      pred_model_data<-pred_model_data[ order(pred_model_data$iso,pred_model_data$Year),   ]
      
      x_pred<-as.data.frame(predict(chosen_model_any,pred_model_data,se.fit=T))
      
      pred_seek_treat<-numeric()
      
      for(i_nor in 1: dim(x_pred)[1]){
        
        pred_seek_treat<-c(pred_seek_treat,rnorm(1,mean=x_pred[i_nor,1],sd=x_pred[i_nor,2])) # Simulation from the predictive distribution.
        
      }
      
      
      pred_list2[[i_pred2]] <- pred_seek_treat
      
    }
    
    cov_db<-read.csv(file_name[1],sep=';',stringsAsFactors=F)
    
    cov_db <- cov_db[cov_db$year != 2017, ]  # Exclude year 2017 which is not required for GBD2017.
    
    cov_db<-cov_db[cov_db$iso %in% extra_countries,]
    
    cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
    
    pred_model_data<-cbind(cov_db[,2:3],do.call(cbind,pred_list2))
    
    #### UNION (of countries with data and without data) #####
    
    union_data_set<-rbind(pred_algo_data, pred_model_data) ## For countries with data, use algorithm to predict. For countries without data, use model to predict.
    
    union_national_any[[length(union_national_any)+1]]<-union_data_set
  }else{union_national_any[[length(union_national_any) + 1]] <- pred_algo_data}
}

print(paste("Session ", i_sim, "/10", " done.", sep = ""))
}

timetaken <- proc.time()[3] - temptime # 2.225 minutes for national; 52.88s for subnational.

## --------------- Step 2e: Combine results from 10 sessions -> 100 realisations for each country and time point ------------ ##    

multi_list<-list()

for (i_u in 1:length(union_national_any)){
  
  x<-union_national_any[[i_u]]
  
  multi_list[[length(multi_list)+1]]<-x[,-c(1,2)]
  
  
  
}

multi_db<-do.call(cbind,multi_list)

## --------------- Step 2f: Postprocessing - apply limits to treatment seeking predictions and confidence intervals ------------ ##    

#sim_mean<-runmean(apply(multi_db,1,mean),1) 

sim_mean<-apply(multi_db,1,mean) 

sim_sd<-apply(multi_db,1,sd)

sim_ui<-runmean(sim_mean+1.96*sim_sd,1)

sim_li<-runmean(sim_mean-1.96*sim_sd,1)  


sim_mean[which(sim_mean<0)]<-0

sim_mean[which(sim_mean>1)]<-1

sim_ui[which(sim_ui<0)]<-0

sim_ui[which(sim_ui>1)]<-1

sim_li[which(sim_li<0)]<-0

sim_li[which(sim_li>1)]<-1


x<-union_national_any[[i_u]]

treat_seek_any<-data.frame(iso=x[,1],year=x[,2],mean=sim_mean,upper=sim_ui,lower=sim_li)
treat_seek_any <- treat_seek_any[order(treat_seek_any$iso, treat_seek_any$year), ]

treat_seek_any$mean[treat_seek_any$mean<0.05]<-0.05
treat_seek_any$upper[treat_seek_any$upper<0.05]<-0.07
treat_seek_any$lower[treat_seek_any$lower<0.05]<-0.03

## --------------- Step 2g: Postprocessing - save and plot results ------------ ##    

if (subnational == TRUE){
  write.csv(treat_seek_any,'India_sub_treat_seek_any.csv',quote=F,row.names=F)
 
  pdf('india_sub_treat_seek_any.pdf',width=8.7,height = 11.2)
  
  cleandata <- sub_national_india
  
}else{write.csv(treat_seek_any,'treat_seek_any.csv',quote=F,row.names=F)
  
  pdf('treat_seek_any.pdf',width=8.7,height = 11.2)
  
  cleandata <- droplevels(TreatSeek[,c('Any_treat','WHO_Subregion','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country_Name','ISO3','Any_treat_low_SVY', 'Any_treat_high_SVY','ID')])
  cleandata$ISO3 <- factor(cleandata$ISO3, levels = levels(treat_seek_any$iso))
  
}

cleandata$point_color<-'red'

cleandata$point_color[grep('MICS3',cleandata$ID)]<-'yellow'

cleandata$point_color[grep('MICS4',cleandata$ID)]<-'green'

cleandata$point_color[grep('MICS5',cleandata$ID)]<-'purple'


par(mfrow=c(3,2))

for( i in unique(treat_seek_any$iso) ){

  country_db<-treat_seek_any[ treat_seek_any$iso==i, ]
  
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
    
    legend('topleft',legend=c('DHS','MICS3','MICS4','MICS5'),pt.bg=c('red','green','yellow','purple'),pch=21)
    
    
  }
  
}

dev.off()

################## National HMIS proportion treatment seeking model #######################

union_national_hmis <-list()

temptime2 <- proc.time()[3]
  
set.seed(1)
  
for (i_sim in 1:10){ 

    ## ---------------- Step 3a: Clean data (remove NA values for what we need in the national models) ---------------- ##
    
    cleandata_hmis <- droplevels(na.omit(TreatSeek[, c('Country_Name','ISO3','Year','WHO_Subregion','HealthExPub','PregWomenCare', 'Out_pocket', 'Any_treat','Any_treat_low_SVY', 'Any_treat_high_SVY', 'HMIS_treat', 'HMIS_treat_low_SVY', 'HMIS_treat_high_SVY')]))
    
    
    ## --------------- Step 3b: Simulate from data distribution (1/10), i.e. likelihood ------------ ##
    
    HMIS_treat <-numeric()
    
    for (i in 1:dim(cleandata_hmis)[1]){
      
      mean_y<-cleandata_hmis$HMIS_treat[i]
      
      sd_y <- (cleandata_hmis$HMIS_treat_high_SVY[i] - cleandata_hmis$HMIS_treat_low_SVY[i])/(2*qnorm(0.975)) 
      
      HMIS_treat<- c(HMIS_treat,rnorm(1,mean=mean_y,sd=sd_y)) # HMIS_treat is a vector of simulations from each country's hmis treatment seeking level. 
    }
    
    any_treat <-numeric()
    
    for (i in 1:dim(cleandata_hmis)[1]){
      
      mean_y<-cleandata_hmis$Any_treat[i]
      
      sd_y <- (cleandata_hmis$Any_treat_high_SVY[i] - cleandata_hmis$Any_treat_low_SVY[i])/(2*qnorm(0.975)) 
      
      any_treat<- c(any_treat,rnorm(1,mean=mean_y,sd=sd_y)) 
    }
    
    cleandata_hmis$HMIS_treat <- HMIS_treat/any_treat # Proportion of treatment seeking going to public facilities.
    cleandata_hmis$HMIS_treat[cleandata_hmis$HMIS_treat<0]<- 0 
    cleandata_hmis$HMIS_treat[cleandata_hmis$HMIS_treat>1]<- 1 
    
    ## --------------- Step 3c: Fit model (1/10) ------------ ##
    
    # Check on Out_pocket covariate:
    # formula_hmis <- HMIS_treat ~ Year+ HealthExPub +PregWomenCare + Out_pocket + WHO_Subregion 
    # Out_pocket doesn't have standard errors associated, model prediction not set up with WHO_Subregion for countries without data.
    formula_hmis <- HMIS_treat ~ Year + HealthExPub +PregWomenCare + Out_pocket # Linear trends with respect to these variables and a Gaussian assumption.
    
    chosen_model_hmis <-gam(formula_hmis, data=cleandata_hmis ) # Fit a generalised additive model. 
    summary(chosen_model_hmis)
    
    if (subnational == TRUE){
      sub_matrix <- sub_national_india
      sub_matrix$Year <- factor(sub_matrix$Year,levels = 1980:2016) 
      sub_matrix$HMIS_treat <- sub_matrix$HMIS_treat/sub_matrix$Any_treat # Proportion of treatment seeking going to public facilities.
      sub_matrix$HMIS_treat[sub_matrix$HMIS_treat<0]<- 0 
      sub_matrix$HMIS_treat[sub_matrix$HMIS_treat>1]<- 1 
      d_m3 <-tapply(sub_matrix$HMIS_treat,list(sub_matrix$map_state, sub_matrix$Year), mean)
      
      
    }else{
      
      ## --------------- Step 3d: Predict for countries where we have treatment seeking data based on fitted model (1/10)  ------------ ##
      cleandata_hmis$Year<-factor(cleandata_hmis$Year,levels = 1980:2016) # Need to be updated every year; so as to get d_m3 matrix.
      d_m3 <-tapply(cleandata_hmis$HMIS_treat,list(cleandata_hmis$ISO3,cleandata_hmis$Year), mean) # Creates a iso3 by year matrix containing the corresponding (mean) HMIS_treat values as elements.
    }
    
    ## --------------- Step 3di: Simulate from covariate input distributions (10 times in parallel) ------------ ##
    
    file_name<-c('Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/public_exp_jul_2017.csv',
                 'Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/pregnant_jul_2017.csv', 
                 'Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/out_of_pocket.csv')
    
    column_names<-c('HealthExPub', 'PregWomenCare', 'Out_pocket')
    
    pred_list<- list()
    
    
    for (i_pred in 1:10){ # Previously parallel but hit problems with proper 'qr' component for lm object. 
      
      list_value<-list()
      
      for (i_f in 1:length(file_name)){
        
        cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
        
        cov_db<-cov_db[cov_db$year<2017,]
        
        cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
        
        new_value<-numeric()
        
        for (i_iso in sort(unique(cov_db$iso))){
          
          country_db<- cov_db[cov_db$iso==i_iso,]
          
          
          
          for ( i in 1:37){
            
            new_value<-c(new_value, rnorm(1,mean=country_db$gp_value[i],sd=sqrt(country_db$mse[i]))) 
            
            
          }
          
          
        }
        
        list_value[[length(list_value)+1]]<-new_value
        
      }
      
      cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
      
      pred_model_data<-cbind(cov_db[,1:3],do.call(cbind,list_value))
      
      names(pred_model_data)[4:6]<-column_names
      
      
      #model_coef<-coef(chosen_model_hmis2)
      
      mean_model<-summary(chosen_model_hmis)$p.table[,1]
      
      sd_model<-summary(chosen_model_hmis)$p.table[,2]
      
      model_coef2<-numeric()
      
      for (i_m in 1:length(mean_model)){
        
        model_coef2<-c(model_coef2,rnorm(1,mean=mean_model[i_m],sd=sd_model[i_m]))
        
        
      }
      
      
      
      list_alg<-as.numeric()
      
      for (cnty in rownames(d_m3)){
        
        # all country
        
        model_coef<-model_coef2
        
        
        if (cnty=='SWZ'){
          model_coef[4]<-0 # Why?
          
        }
        
        
        d_m_pos <-which(rownames(d_m3)==cnty) # Row index.
        
        if (subnational == TRUE){
          cov_country <-pred_model_data[pred_model_data$iso=="IND", ] 
        }else{cov_country <-pred_model_data[pred_model_data$iso==cnty, ] }
        
        
        seek_value <-d_m3[d_m_pos,] # Treatment-seeking data.
        
        year_data<-37-length(which(is.na(seek_value))) # Number of years with data.
        
        pos_data<-which(seek_value>0) # Where we have data.
        
        # Start point
        
        start_na<-as.numeric()
        
        value=1 # Find first data point.
        
        while (is.na(seek_value[value]))
        {  
          start_na<-c(start_na,value)
          
          value=value+1 
        } 
        
        for (v in rev(start_na)) # Fill up points before first data point using national model.
        {  
          
          year_cov1<-cov_country[value, ]
          
          # year_cov1<-cov_country[v+1, ]
          
          year_cov2<-cov_country[v, ]
          
          intercept_rand<-seek_value[value]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
          
          missing_data<-intercept_rand+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
          
          
          seek_value[v]<-missing_data
          
        }
        
        
        #end point
        
        end_na<-as.numeric()
        
        value=length(seek_value)  
        
        while (is.na(seek_value[value])) # Fill up points from the end towards the last data point using national model.
        {  
          end_na<-c(end_na,value)
          
          value=value-1
        } 
        
        
        
        if (value<37) # value is not the last data point. 
          
        {
          
          for (v in rev(end_na)) # points to fill up.
            
          {
            
            
            year_cov1<-cov_country[v-1, ]
            
            year_cov2<-cov_country[v, ]
            
            
            intercept_rand<-seek_value[value]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
            
            missing_data<-intercept_rand+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
            
            seek_value[v]<-missing_data
            
          }
          
          
        }
        
        #  missing value in the middle of time series
        
        
        if(sum(as.numeric(is.na(seek_value)))>0)
          
        {
          
          
          empty_v<-which(is.na(seek_value))
          
          middle_na<-as.numeric()
          
          value=length(seek_value)  
          
          
          # first primer
          
          
          seek_value2<-seek_value
          
          front_back<-as.numeric()
          
          for (v in 1:value)
          {  
            
            #print (v)
            
            if (is.na(seek_value2[v])){
              
              prev<-v-1
              
              after_value<-NA
              
              n=1
              
              while ( is.na(after_value))
              {
                
                
                after<-v+n
                
                after_value<-seek_value2[after]
                
                n=n+1
              }
              
              
              year_cov1<-cov_country[prev, ]
              
              year_cov2<-cov_country[v, ]
              
              year_cov3<-cov_country[after, ]
              
              intercept_rand1<-seek_value2[prev]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
              
              missing_data1<-intercept_rand1+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
              
              intercept_rand2<-seek_value2[after]-(year_cov3[,3]*model_coef[2])-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])-(year_cov3[,6]*model_coef[5])
              
              missing_data2<-intercept_rand2+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
              
              exp_mid3<-(missing_data1+missing_data2)/2
              
              
              seek_value2[v]<-exp_mid3
              
              front_back<-c(front_back,exp_mid3)
              
              
            }
          } 
          
          
          
          # second primer
          
          
          seek_value3<-seek_value
          
          
          back_front<-as.numeric()
          
          for (v in value:1)
          {  
            
            # print (v)
            
            if (is.na(seek_value3[v])){
              
              after<-v+1
              
              pre_value<-NA
              
              n=1
              
              while ( is.na(pre_value))
              {
                
                
                prev<-v-n
                
                pre_value<-seek_value3[prev]
                
                n=n+1
              }
              
              
              year_cov1<-cov_country[prev, ]
              
              year_cov2<-cov_country[v, ]
              
              year_cov3<-cov_country[after, ]
              
              intercept_rand1<-seek_value3[prev]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
              
              missing_data1<-intercept_rand1+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
              
              intercept_rand2<-seek_value3[after]-(year_cov3[,3]*model_coef[2])-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])-(year_cov3[,6]*model_coef[5])
              
              missing_data2<-intercept_rand2+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
              
              
              exp_mid3<-(missing_data1+missing_data2)/2
              
              seek_value3[v]<-exp_mid3
              
              
              back_front<-c(back_front,exp_mid3)
              
              
            }
          } 
          
          
          
          seek_value<-(seek_value2+seek_value3)/2
          
          
        }
        
        #plot(1980:2016,seek_value,ylim=c(0,1),main=cnty)
        #points(1980:2016, d_m3[d_m_pos,] ,pch=21,bg=3)
        #lines(1980:2016, runmean(seek_value, 5), col = 2)
        
        list_alg<-c(list_alg,runmean(seek_value,5)) # moving window mean. why? to make it smoother?
        
      } 
      pred_list[[i_pred]] <- list_alg
    }
    
    
    first_sim_res<-do.call(cbind,pred_list)  
    
    pred_algo_data<-data.frame(iso=rep(rownames(d_m3),each=37),year= rep(1980:2016,length(rownames(d_m3))))
    
    pred_algo_data<-cbind(pred_algo_data,first_sim_res)
    
    
    ## --------------- Step 3div: Predict the treatment seeking time series for countries with no data using model ------------ ##    
    
    if (subnational == TRUE){
      union_national_hmis[[length(union_national_hmis) + 1]] <- pred_algo_data
    }else{ extra_countries<-sort(gdb_2016_country_list[-c(match(rownames(d_m3),gdb_2016_country_list))])
    
    if (length(extra_countries) != 0){
      pred_list2<-list()
      
      for (i_pred2 in 1:10){ 
        
        list_value<-list()
        
        for (i_f in 1:length(file_name)){
          
          cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
          
          cov_db <- cov_db[cov_db$year != 2017, ]  # Exclude year 2017 which is not required for GBD2017.
          
          cov_db<-cov_db[cov_db$iso %in% extra_countries,]
          
          cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
          
          new_value<-numeric()
          
          for (i_iso in sort(unique(cov_db$iso))){
            
            country_db<- cov_db[cov_db$iso==i_iso,]
            
            for ( i in 1:37){
              
              new_value<-c(new_value, rnorm(1,mean=country_db$gp_value[i],sd=sqrt(country_db$mse[i]))) # Simulate from covariate distribution.
              
            }
            
          }
          
          list_value[[length(list_value)+1]]<-new_value
          
        }
        
        cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
        
        pred_model_data<-cbind(cov_db[,1:3],do.call(cbind,list_value))
        
        names(pred_model_data)<-c('Country','iso','Year',column_names)
        
        pred_model_data<-pred_model_data[ order(pred_model_data$iso,pred_model_data$Year),   ]
        
        x_pred<-as.data.frame(predict(chosen_model_hmis,pred_model_data,se.fit=T))
        
        pred_seek_treat<-numeric()
        
        for(i_nor in 1: dim(x_pred)[1]){
          
          pred_seek_treat<-c(pred_seek_treat,rnorm(1,mean=x_pred[i_nor,1],sd=x_pred[i_nor,2])) # Simulation from the predictive distribution.
          
        }
        
        
        pred_list2[[i_pred2]] <- pred_seek_treat
        
      }
      
      cov_db<-read.csv(file_name[1],sep=';',stringsAsFactors=F)
      
      cov_db <- cov_db[cov_db$year != 2017, ]  # Exclude year 2017 which is not required for GBD2017.
      
      cov_db<-cov_db[cov_db$iso %in% extra_countries,]
      
      cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
      
      pred_model_data<-cbind(cov_db[,2:3],do.call(cbind,pred_list2))
      
      #### UNION (of countries with data and without data) #####
      
      union_data_set<-rbind(pred_algo_data, pred_model_data) ## For countries with data, use algorithm to predict. For countries without data, use model to predict.
      
      union_national_hmis[[length(union_national_hmis)+1]]<-union_data_set
    }else{union_national_hmis[[length(union_national_hmis) + 1]] <- pred_algo_data}
    }
    
    print(paste("Session ", i_sim, "/10", " done.", sep = ""))
  }
  
  timetaken2 <- proc.time()[3] - temptime2 # 2.225 minutes for national; 52.88s for subnational.
  
  ## --------------- Step 3e: Combine results from 10 sessions -> 100 realisations for each country and time point ------------ ##    
  
  multi_list<-list()
  
  for (i_u in 1:length(union_national_hmis)){
    
    x<-union_national_hmis[[i_u]]
    
    multi_list[[length(multi_list)+1]]<-x[,-c(1,2)]
    
    
    
  }
  
  multi_db<-do.call(cbind,multi_list)
  
  ## --------------- Step 3f: Postprocessing - apply limits to treatment seeking predictions and confidence intervals ------------ ##    
  
  #sim_mean<-runmean(apply(multi_db,1,mean),1) 
  
  sim_mean<-apply(multi_db,1,mean) 
  
  sim_sd<-apply(multi_db,1,sd)
  
  sim_ui<-runmean(sim_mean+1.96*sim_sd,1)
  
  sim_li<-runmean(sim_mean-1.96*sim_sd,1)  
  
  
  sim_mean[which(sim_mean<0)]<-0
  
  sim_mean[which(sim_mean>1)]<-1
  
  sim_ui[which(sim_ui<0)]<-0
  
  sim_ui[which(sim_ui>1)]<-1
  
  sim_li[which(sim_li<0)]<-0
  
  sim_li[which(sim_li>1)]<-1
  
  
  x<-union_national_hmis[[i_u]]
  
  if (subnational == TRUE){
    treat_seek_any <- read.csv("J:/Treatment_Seeking/India_sub_treat_seek_any.csv")
  }else{ treat_seek_any <- read.csv("J:/Treatment_Seeking/treat_seek_any.csv")}
  
 
  
  treat_seek_hmis<-data.frame(iso=x[,1],year=x[,2],mean=sim_mean,upper=sim_ui,lower=sim_li)
  any_iso_year<-paste(treat_seek_any$iso,treat_seek_any$year,sep='_')
  pub_iso_year<-paste(treat_seek_hmis$iso,treat_seek_hmis$year,sep='_')
  treat_seek_any<-treat_seek_any[match(pub_iso_year,any_iso_year),]
  
  
  treat_seek_hmis$mean[treat_seek_hmis$mean<0.05]<-0.05
  treat_seek_hmis$upper[treat_seek_hmis$upper<0.05]<-0.07
  treat_seek_hmis$lower[treat_seek_hmis$lower<0.05]<-0.03
  
  ## Public treatment seeking proportions ##
  
  mean_pub<-round(treat_seek_hmis[,3]*treat_seek_any[,3],5)
  
  upper_pub<-round(treat_seek_hmis[,4]*treat_seek_any[,4],5)
  
  lower_pub<-round(treat_seek_hmis[,5]*treat_seek_any[,5],5) 
  
  treat_seek_pub2<-cbind(treat_seek_hmis[,1:2],data.frame(mean=mean_pub,upper=upper_pub,lower=lower_pub))
  
  treat_seek_pub2 <- treat_seek_pub2[order(as.character(treat_seek_pub2$iso), treat_seek_pub2$year), ]
  
  ## Private treatment seeking proportions ##
  
  mean_priv<-round(treat_seek_any[,3]*(1-treat_seek_hmis[,3]),5)
  
  upper_priv<-round(treat_seek_any[,4]*(1-treat_seek_hmis[,5]),5)
  
  lower_priv<-round(treat_seek_any[,5]*(1-treat_seek_hmis[,4]),5) 
  
  treat_seek_priv<-cbind(treat_seek_hmis[,1:2],data.frame(mean=mean_priv,upper=upper_priv,lower=lower_priv))
  
  ## --------------- Step 3g: Postprocessing - save and plot results ------------ ##    
  
  if (subnational == TRUE){
    write.csv(treat_seek_pub2,'India_sub_pub_treat.csv',quote=F,row.names=F)
    write.csv(treat_seek_priv,'India_sub_priv_treat.csv',quote=F,row.names=F)
    
    pdf('india_sub_pub_treat.pdf',width=8.7,height = 11.2)
    
    cleandata <- sub_national_india

  }else{write.csv(treat_seek_pub2,'treat_seek_pub_treat.csv',quote=F,row.names=F)
    write.csv(treat_seek_priv,'treat_seek_priv_treat.csv',quote=F,row.names=F)
    
    pdf('treat_seek_pub_treat.pdf',width=8.7,height = 11.2)
    
    cleandata <- droplevels(TreatSeek[,c('HMIS_treat','WHO_Subregion','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country_Name','ISO3','HMIS_treat_low_SVY', 'HMIS_treat_high_SVY','ID')])
    cleandata$ISO3 <- factor(cleandata$ISO3, levels = levels(treat_seek_hmis$iso))

  }
  
  cleandata$point_color<-'red'
  
  cleandata$point_color[grep('MICS3',cleandata$ID)]<-'yellow'
  
  cleandata$point_color[grep('MICS4',cleandata$ID)]<-'green'
  
  cleandata$point_color[grep('MICS5',cleandata$ID)]<-'purple'
  
  
  par(mfrow=c(3,2))
  
  for( i in unique(treat_seek_pub2$iso) ){
    
    country_db<-  treat_seek_pub2[treat_seek_pub2$iso==i, ]
    
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
        
        plotCI(as.numeric(country_line$Year),country_line$HMIS_treat,ui=country_line$HMIS_treat_high_SVY,li=country_line$HMIS_treat_low_SVY,add=T)
        
        points(country_line$Year,country_line$HMIS_treat,pch=21,bg=country_line$point_color)
        
      }
    }
    
    
    if(i=='AFG'){
      
      legend('topleft',legend=c('DHS','MICS3','MICS4','MICS5'),pt.bg=c('red','green','yellow','purple'),pch=21)
      
      
    }
    
  }
  
  dev.off()
  

  
  




