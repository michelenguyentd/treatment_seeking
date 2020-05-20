# Sought tretment at public facilities



##Building treatment seeking model

#load libraries
library(mgcv) #for GAMM
library(plotrix)
library(igraph)
library(RColorBrewer)

library(doMC)

library(caTools)

registerDoMC(cores=10)

gdb_2016_country_list <- read.csv("/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/gdb_2016_country_list.csv", sep=",",stringsAsFactors=F)


#import dataset
#rm(list = ls())
#set working directory and read in the dataset
setwd("/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour")
#setwd("C:/Users/Katherine Battle/Dropbox/03 PvPR update/API/World Bank National Data/")
#TreatSeek <- read.csv("DHS_MICS_national_alldata_withmissing3.csv")

TreatSeek <- read.csv("NationalSurveyResults_Weighted_With_UpdatedIndicators.csv")

#Shorten some of the long country names
TreatSeek$Country <- as.character(TreatSeek$Country)
TreatSeek$Country[TreatSeek$Country == "Congo Democratic Republic"] <- "DRC"
TreatSeek$Country[TreatSeek$Country == "Dominican Republic"] <- "Dominican Rep."
TreatSeek$Country[TreatSeek$Country == "United Republic of Tanzania"] <- "Tanzania"
TreatSeek$Country[TreatSeek$Country == "Lao People's Democratic Republic"] <- "Lao PDR"

TreatSeek$Country <- as.factor(TreatSeek$Country)

#Add a column for country and year together
TreatSeek$CountryYear <- paste(TreatSeek$Country, TreatSeek$Year, sep=" ")

#TreatSeek<-TreatSeek[-c(69,71),]

#TreatSeek<-TreatSeek[TreatSeek$Year>1989,]

TreatSeek<-TreatSeek[TreatSeek$Year>1995,]


union_list<-list()

for (i_sim in 1 :10){
  
  x_time1<-proc.time()
  
  print (i_sim) 
  
  #remove NA values (Zim does't have any health expenditure data)
  #cleandata <- droplevels(na.omit(TreatSeek))
  #summary(cleandata$WHO_Sub)
  #write.csv(cleandata, file = "C:/Battle/Dropbox/03 PvPR update/API/World Bank National Data/cleandata.csv")
  
  
  cleandata <- droplevels(na.omit(TreatSeek[,c('Any_treat','WHO_Sub','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country','Country_ISO3Code','Any_treat_low_SVY', 'Any_treat_high_SVY','SurveyType','SurveyName')]))
  
  cleandata<-droplevels(cleandata[c(grep('MIS',cleandata$SurveyType),grep('DHS',cleandata$SurveyType)),])
  
  cleandata<-droplevels(cleandata[-c(grep('TZ2015DHS',cleandata$SurveyName)),])
  
  
  #cleandata <- cleandata [-c(which(cleandata$Year<1995)),]
  
  
  ## remove India 1993
  #cleandata<-cleandata[-77,]
  
  # remove all survey before 1995
  
 
  
  
  pub_treat<-numeric()
  
  for (i in 1:dim(cleandata)[1]){
    
    mean_y<-cleandata$Any_treat[i]
    
    sd_y<-(round(cleandata$Any_treat[i],2)- round(cleandata$Any_treat_low_SVY[i],2))/2
    
    #print(rnorm(1,mean=mean_y,sd=sd_y))
    
    pub_treat<- c(pub_treat,rnorm(1,mean=mean_y,sd=sd_y))
  }
  
  cleandata$Any_treat<-pub_treat 
  
  #formula_hmis2 <-  Any_treat~  (Year) + PrimaryComplete+ PregWomenCare
  
  formula_hmis2 <-  Any_treat~ Year+PrimaryComplete+ PregWomenCare
  
  #chosen_model_hmis2<-gamm(formula_hmis2, data=cleandata, random=list(WHO_Sub=~1) )
  
  chosen_model_hmis2<-gam(formula_hmis2, data=cleandata )
  
  
  ##### Create database prediction #####
  
  
  file_name<-c(#'~/Desktop/treatment_seeking_behaviour/public_exp.csv',
               '/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/primary_school.csv',
               #'/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/DPT.csv',
               '/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/pregnant.csv',
               #'/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/GDP_grow.csv',
               '/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/tot_ex.csv'
  )
  
  
  column_names<-c('PrimaryComplete' , 'PregWomenCare')
  
  
  ###############################################################################################
  ###############################################################################################
  
  cleandata <- droplevels(TreatSeek)
  
  #cleandata <- droplevels(TreatSeek[-c(which(is.na(TreatSeek$Any_treat))),])

  
  #cleandata <- droplevels(na.omit(TreatSeek[,c('Any_treat','WHO_Sub','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country','Country_ISO3Code','Any_treat_low', 'Any_treat_high')]))
  
  
  #cleandata<-cleandata[-77,]
  
  cleandata$Year<-factor(cleandata$Year,levels = 1980:2016)
  
  d_m3<-tapply(cleandata$Any_treat,list(cleandata$Country_ISO3Code,cleandata$Year),mean)
  
  pred_list<-foreach (i = 1:10) %dopar%{ 
    
    list_value<-list()
    
    for (i_f in 1:2){
      
      cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
      
      cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
      
      
      new_value<-numeric()
      
      for (i_iso in sort(unique(cov_db$iso))){
        
        country_db<- cov_db[cov_db$iso==i_iso,]
        
        
        for ( i in 1:37){
          
          new_value<-c(new_value, rnorm(1,mean=country_db$gp_value[i],sd=sqrt(country_db$mse[i]))) 
          
          
          #new_value<-c(new_value, country_db$gp_value[i]) 
          
        }
        
        
      }
      
    
      
      list_value[[length(list_value)+1]]<-new_value
      
    }
    
    cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
    
    pred_model_data<-cbind(cov_db[,1:3],do.call(cbind,list_value))
    
    names(pred_model_data)[4:5]<-column_names
    
    
    #model_coef<-coef(chosen_model_hmis2)
    
    mean_model<-summary(chosen_model_hmis2)$p.table[,1]
    
    sd_model<-summary(chosen_model_hmis2)$p.table[,2]
    
    model_coef2<-numeric()
    
    for (i_m in 1:length(mean_model)){
      
      model_coef2<-c(model_coef2,rnorm(1,mean=mean_model[i_m],sd=sd_model[i_m]))
      
      
    }
    

    
    cleandata <- droplevels(TreatSeek)
    
    #cleandata <- droplevels(TreatSeek[-c(which(is.na(TreatSeek$Any_treat))),])
    
    #cleandata <- droplevels(na.omit(TreatSeek[,c('Any_treat','WHO_Sub','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country','Country_ISO3Code','Any_treat_low', 'Any_treat_high')]))
    
    #cleandata<-cleandata[-77,]
    
    #cleandata<-droplevels(cleandata[-c(sample(c(1:dim(cleandata)[1]),60)),])
    
    cleandata$Year<-factor(cleandata$Year,levels = 1980:2016)
    
    
    
    pub_treat<-numeric()
    
    for (i in 1:dim(cleandata)[1]){
      
      mean_y<-cleandata$Any_treat[i]
      
      sd_y<-(round(cleandata$Any_treat[i],2)- round(cleandata$Any_treat_low_SVY[i],2))/2
      
      #print(rnorm(1,mean=mean_y,sd=sd_y))
      
      pub_treat<- c(pub_treat,rnorm(1,mean=mean_y,sd=sd_y))
    }
    
    cleandata$Any_treat<-pub_treat 
    
    
    
    d_m3<-tapply(cleandata$Any_treat,list(cleandata$Country_ISO3Code,cleandata$Year),mean)
    
    iso_country<-numeric()
    
    #par(mfrow=c(3,2))
    
    list_alg<-as.numeric()
    
    for (cnty in rownames(d_m3)){
      
      #print (cnty)
      
      # all country
      
      model_coef<-model_coef2
      
      
      if (cnty=='SWZ'){
        model_coef[4]<-0
        
      }
      
      
      
      iso_country<-c(iso_country,cnty)
      
      
      d_m_pos<-which(rownames(d_m3)==cnty)
      
      cov_country<-pred_model_data[ pred_model_data$iso==cnty, ] 
      
      #cov_country$year[cov_country$year<1980]<-1980
      
      #cov_country<-cov_country[,-3]
      
      #cov_country[,4]<-runmean(cov_country[,4],10)
      
     # cov_country[,5]<-runmean(cov_country[,5],10)
      
      
      
      #seek_value<-c(rep(NA,10),d_m3[d_m_pos,],NA,NA)
      
      seek_value<-d_m3[d_m_pos,]
      
      year_data<-37-length(which(is.na(seek_value)))
      
      if(year_data>3){
        
        
        #n_to_delete<-sample(1:(length(as.numeric(which(seek_value>0)))-1),1)
        
        n_to_delete<-1
        
        seek_value[sample(as.numeric(which(seek_value>0)),n_to_delete)]<-NA
        
      }
      
      pos_data<-which(seek_value>0)
      
    #  if (length(pos_data)>5){
        
     #   seek_value[sample(pos_data,1)]<-NA
        
    #  } 
      
      
      
      #start point
      
      start_na<-as.numeric()
      
      value=1  
      
      while (is.na(seek_value[value]))
      {  
        start_na<-c(start_na,value)
        
        value=value+1
      } 
      
      
      
      for (v in rev(start_na))
      {  
        
        year_cov1<-cov_country[value, ]
        
       # year_cov1<-cov_country[v+1, ]
        
        year_cov2<-cov_country[v, ]
        
        intercept_rand<-seek_value[value]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
        
        missing_data<-intercept_rand+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
        
        
        seek_value[v]<-missing_data
        
      }
      
      
      #end point
      
      #if (length(which(is.na(seek_value[-1])))==12){
      
      if (sum(seek_value[-1],na.rm=T)==0){
        
        old_method<-'no'
        
      }else{
        
        
        old_method<-'yes'
        
      }
      
      
      
      if (old_method=='yes')
        
      {  
        
        end_na<-as.numeric()
        
        value=length(seek_value)  
        
        while (is.na(seek_value[value]))
        {  
          end_na<-c(end_na,value)
          
          value=value-1
        } 
        
        
        
        if (value<37)
          
        {
          
          for (v in rev(end_na))
            
          {
            
            
            year_cov1<-cov_country[v-1, ]
            
            year_cov2<-cov_country[v, ]
            
            #intercept_rand<-seek_value[v-1]-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
            
           # missing_data<-intercept_rand+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            intercept_rand<-seek_value[value]-(year_cov1[,3]*model_coef[2])-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])
            
            missing_data<-intercept_rand+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            
            
            seek_value[v]<-missing_data
            
            
            
          }
          
          
        }
        
      }else{
        
        
        if (is.na(tail(seek_value,1)))
          
        {
          seek_value[length(seek_value)]<-tip_tail
          
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
            
            intercept_rand2<-seek_value2[after]-(year_cov1[,3]*model_coef[2])-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])
            
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
            
            intercept_rand2<-seek_value3[after]-(year_cov1[,3]*model_coef[2])-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])
            
            missing_data2<-intercept_rand2+(year_cov2[,3]*model_coef[2])+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])
            
            
            exp_mid3<-(missing_data1+missing_data2)/2
            
            seek_value3[v]<-exp_mid3
            
            
            back_front<-c(back_front,exp_mid3)
            
            
          }
        } 
        
        
        
        seek_value<-(seek_value2+seek_value3)/2
        
        
      }
      
      #plot(1980:2016,seek_value,ylim=c(0,1),main=cnty)
      
      #points(1980:2016,c(rep(NA,10),d_m3[d_m_pos,],NA,NA),pch=21,bg=3)
      
      list_alg<-c(list_alg,runmean(seek_value,5))
    }
    
    return(list_alg)
    
  }
  
  
  first_sim_res<-do.call(cbind,pred_list)  
  
  pred_algo_data<-data.frame(iso=rep(rownames(d_m3),each=37),year= rep(1980:2016,length(rownames(d_m3))))
  
  pred_algo_data<-cbind(pred_algo_data,first_sim_res)
  
  #############################################
  ###### predict place without values #########
  #############################################
  
  #cleandata <- cleandata [-c(which(cleandata$Year<1995)),]
  
  #cleandata <- droplevels(TreatSeek[-c(which(is.na(TreatSeek$Any_treat))),])
  
  cleandata <- droplevels(TreatSeek)
  
  cleandata$Year<-factor(cleandata$Year,levels = 1980:2016)
  
  d_m3<-tapply(cleandata$Any_treat,list(cleandata$ISO3,cleandata$Year),mean)
  
  extra_countries<-sort(gdb_2016_country_list$iso[-c(match(rownames(d_m3),gdb_2016_country_list$iso))])
  
  extra_country=1
  
  if(extra_country==1){
    
    
    pred_list2<-foreach (i = 1:10) %dopar%{ 
      
      list_value<-list()
      
      for (i_f in 1:2){
        
        cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
        
        cov_db<-cov_db[cov_db$iso %in% extra_countries,]
        
        cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
        
        new_value<-numeric()
        
        for (i_iso in sort(unique(cov_db$iso))){
          
          country_db<- cov_db[cov_db$iso==i_iso,]
          
          
          for ( i in 1:37){
            
            new_value<-c(new_value, rnorm(1,mean=country_db$gp_value[i],sd=sqrt(country_db$mse[i]))) 
            
            #new_value<-c(new_value, country_db$gp_value[i]) 
            
          }
          
          
        }
        
        list_value[[length(list_value)+1]]<-new_value
        
      }
      
      cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
      
      pred_model_data<-cbind(cov_db[,1:3],do.call(cbind,list_value))
      
      
      names(pred_model_data)<-c('Country','iso','Year',column_names)
      
      pred_model_data<-pred_model_data[ order(pred_model_data$iso,pred_model_data$Year),   ]
      
      #head(pred_model_data)
      
      x_pred<-as.data.frame(predict(chosen_model_hmis2,pred_model_data,se.fit=T))
      
      pred_seek_treat<-numeric()
      
      for(i_nor in 1: dim(x_pred)[1]){
        
        pred_seek_treat<-c(pred_seek_treat,rnorm(1,mean=x_pred[i_nor,1],sd=x_pred[i_nor,2]))
        
        
      }
      
      
      return(pred_seek_treat)
      
      
    }
    
    cov_db<-read.csv(file_name[1],sep=';',stringsAsFactors=F)
    
    cov_db<-cov_db[cov_db$iso %in% extra_countries,]
    
    cov_db<-cov_db[order(cov_db$iso,cov_db$year),]
    
    pred_model_data<-cbind(cov_db[,2:3],do.call(cbind,pred_list2))
    
    
    #plot(1980:2016,tail(pred_list[[1]],37),type='l')
    
    #for (i in 2:length(pred_list)){
    
    #  lines(1980:2016,tail(pred_list[[i]],37))
    
    
    #}
    
    
    #par(mfrow=c(3,3))
    
    #for (i in unique(cleandata$Country_ISO3Code)){
    
    # country_db<-cleandata[cleandata$Country_ISO3Code==i,]
    
    # plot(0,0,type='n',xlim=c(1980,2016),ylim=c(0,1),main=unique(country_db$Country))
    
    # for (i_col in 4:103){
    
    #  lines(country_pred$year,country_pred[,i_col],col=gray(.90))
    
    # }
    
    # plotCI( country_db$Year, country_db$Any_treat,ui=country_db$Any_treat_high,li=country_db$Any_treat_low
    #         ,pt.bg=3,pch=21,add=T)
    
    # country_pred<-pred_model_data[pred_model_data$iso==i,]
    
    
    
    #}
    
    
  }
  
  
  #### UNION #####
  
  
  union_data_set<-rbind(pred_algo_data, pred_model_data)
  
  #######################
  #######################
  #######################
  
  union_list[[length(union_list)+1]]<-union_data_set
  
  x_time2<-proc.time()
  
 # print(x_time2-x_time1 )
  
}


multi_list<-list()

for (i_u in 1:length(union_list)){
  
  x<-union_list[[i_u]]
  
  multi_list[[length(multi_list)+1]]<-x[,-c(1,2)]
  
  
  
}

multi_db<-do.call(cbind,multi_list)


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


x<-union_list[[i_u]]

treat_seek_any<-data.frame(iso=x[,1],year=x[,2],mean=sim_mean,upper=sim_ui,lower=sim_li)

treat_seek_any$mean[treat_seek_any$mean<0.05]<-0.05
treat_seek_any$upper[treat_seek_any$upper<0.05]<-0.07
treat_seek_any$lower[treat_seek_any$lower<0.05]<-0.03

write.csv(treat_seek_any,'treat_seek_any_no_year_19_Jul_2017.csv',quote=F,row.names=F)

#first_sim_res[which(first_sim_res<0)]<-0

#first_sim_res[which(first_sim_res>1)]<-1

#sim_mean<-apply(first_sim_res,1,mean) 

#sim_low<-apply(first_sim_res,1,quantile,prob=0.025)

#sim_low<-apply(first_sim_res,1,quantile,prob=0.025)


#cleandata <- droplevels(TreatSeek[-c(which(is.na(TreatSeek$Any_treat))),])

#cleandata <- droplevels(TreatSeek[-c(which(is.na(TreatSeek$Any_treat))),c('Any_treat','WHO_Sub','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country','Country_ISO3Code','Any_treat_low_SVY', 'Any_treat_high_SVY','ID')])

cleandata <- droplevels(TreatSeek[,c('Any_treat','WHO_Sub','Year','HealthExTotal','PregWomenCare','PrimaryComplete','DPT','GDPGrowth','Country','Country_ISO3Code','Any_treat_low_SVY', 'Any_treat_high_SVY','ID')])



cleandata$point_color<-'red'

cleandata$point_color[grep('MICS3',cleandata$ID)]<-'yellow'

cleandata$point_color[grep('MICS4',cleandata$ID)]<-'green'

cleandata$point_color[grep('MICS5',cleandata$ID)]<-'purple'

pdf('treat_seek_any_no_year_19_Jul_2017.pdf',width=8.7,height = 11.2)

par(mfrow=c(3,2))

for( i in unique(treat_seek_any$iso) ){

  
  
  country_db<-treat_seek_any[ treat_seek_any$iso==i, ]
  
  plot(0,0,type='n',ylim=c(0,1),xlim=c(1980,2016),main=i,ylab='% Sougth Treatment',xlab='Year')
  
  plotCI(1980:2016,country_db$mean,ui=country_db$upper,li=country_db$lower,add=T)
  
  abline(h=0.5,col=3,lwd=1.5,lty=2)
  
  in_out<-which(cleandata$Country_ISO3Code==i)
  
  if(length(in_out)>0){
    
    
   country_line<- cleandata[which(cleandata$Country_ISO3Code==i),]
    
   plotCI(country_line$Year,country_line$Any_treat,ui=country_line$Any_treat_high,li=country_line$Any_treat_low,add=T)
   
   points(country_line$Year,country_line$Any_treat,pch=21,bg=country_line$point_color)
   
   
   
  }
  
  #lines(1980:2016,runmean(country_db$mean,5),lwd=0.8,col=2)
  
  #lines(1980:2016,runmean(country_db$upper,5),lwd=0.8,col=2)
  
  #lines(1980:2016,runmean(country_db$lower,5),lwd=0.8,col=2)
  
  
  if(i=='AFG'){
    
    legend('topleft',legend=c('DHS','MICS3','MICS4','MICS5'),pt.bg=c('red','green','yellow','purple'),pch=21)
    
    
  }
  
}

dev.off()






