# Sought tretment at public facilities



##Building treatment seeking model

#load libraries
library(mgcv) #for GAMM
library(plotrix)

library(doMC)

library(MuMIn)
library(caTools)


registerDoMC(cores=10)
 
gdb_2016_country_list <- read.csv("/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/gdb_2016_country_list.csv", sep=",",stringsAsFactors=F)


#import dataset
#rm(list = ls())
#set working directory and read in the dataset
setwd("/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour")
#setwd("C:/Users/Katherine Battle/Dropbox/03 PvPR update/API/World Bank National Data/")
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

TreatSeek<-TreatSeek[TreatSeek$Year>1995,]


#### adding out of pocket ####

raw_out_pocket<-read.csv('/home/zool1286/Desktop/MAP_data/GBD2016/SubProjects/01_Treatment_Seeking/03_Processed_Data/SH.XPD.OOPC.ZS.csv',stringsAsFactors = F)

raw_out_pocket$Country.Code[which(raw_out_pocket$Country.Code=='ZAR')]<-'COD'

raw_out_pocket$Country.Code[which(raw_out_pocket$Country.Code=='TMP')]<-'TLS'

raw_out_pocket$X2016<-NA

TreatSeek$Out_pocket<-NA

raw_to_fill<-(1:length(TreatSeek[,1]))#[-c(which(is.na(TreatSeek$Any_treat)))]

for (i in raw_to_fill){
  
  row_num<-which(as.character(raw_out_pocket$Country.Code) == as.character(TreatSeek$Country_ISO3Code[i]))
  
  col_num<-grep(as.character(TreatSeek$Year[i]),names(raw_out_pocket))
  
  TreatSeek$Out_pocket[i]<-raw_out_pocket[row_num,col_num]
  
}




##############################



union_list<-list()

for (i_sim in 1 :10){

  print(i_sim)
  
#remove NA values (Zim does't have any health expenditure data)
#cleandata <- droplevels(na.omit(TreatSeek))
#summary(cleandata$WHO_Sub)
#write.csv(cleandata, file = "C:/Battle/Dropbox/03 PvPR update/API/World Bank National Data/cleandata.csv")


cleandata <- droplevels(TreatSeek[,c('Any_treat','HMIS_treat','WHO_Sub','Year','HealthExPub','PregWomenCare','PrimaryComplete','DPT','Country','Country_ISO3Code','HMIS_treat_low_SVY', 'HMIS_treat_high_SVY','Any_treat_low_SVY', 'Any_treat_high_SVY','Out_pocket','SurveyType','SurveyName')])

cleandata<-droplevels(cleandata[c(grep('MIS',cleandata$SurveyType),grep('DHS',cleandata$SurveyType)),])

cleandata<-droplevels(cleandata[-c(grep('TZ2015DHS',cleandata$SurveyName)),])

# remove India 1993
#cleandata<-cleandata[-77,]

pub_treat<-numeric()

for (i in 1:dim(cleandata)[1]){
  
  mean_y<-cleandata$HMIS_treat[i]
  
  sd_y<-abs((cleandata$HMIS_treat_high_SVY[i] - cleandata$HMIS_treat[i])/2)
    
  pub_treat<- c(pub_treat,rnorm(1,mean=mean_y,sd=sd_y))
}

any_treat<-numeric()

for (i in 1:dim(cleandata)[1]){
  
  mean_y<-cleandata$Any_treat[i]
  
  sd_y<-abs((round(cleandata$Any_treat[i],2)- round(cleandata$Any_treat_low[i],2))/2)
  
  #print(rnorm(1,mean=mean_y,sd=sd_y))
  
  any_treat<- c(any_treat,rnorm(1,mean=mean_y,sd=sd_y))
}

#cleandata$Any_treat<-any_treat-pub_treat 

cleandata$HMIS_treat<-(any_treat-pub_treat) 

cleandata$HMIS_treat[which(cleandata$HMIS_treat<0)]<-0

cleandata$HMIS_treat<-1-(cleandata$HMIS_treat / any_treat)

formula_hmis2 <-  HMIS_treat ~ (Year)+ HealthExPub+PregWomenCare + (Out_pocket) +WHO_Sub


#chosen_model_hmis2<-uGamm(formula_hmis2, data=cleandata, random=list(Country=~1) )

cleandata2<-droplevels((cleandata))

cleandata3<-cleandata2

chosen_model_hmis2<-gam(formula_hmis2, data=cleandata2)


##### Create database prediction #####


file_name<-c('/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/public_exp_jul_2017.csv',
             #'/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/primary_school_gam.csv',
             #'/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/DPT_gam.csv',
             '/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/pregnant_jul_2017.csv',
             #'~/Desktop/treatment_seeking_behaviour/GDP_grow.csv',
             #'~/Desktop/treatment_seeking_behaviour/tot_ex.csv',
             '/media/zool1286/7F09CEEF2EFD0C96/treatment_seeking_behaviour/out_of_pocket_jul_2017.csv'
             
)


column_names<-c('HealthExPub',  'PregWomenCare', 'Out_pocket')


###############################################################################################
###############################################################################################

cleandata$Year<-factor(cleandata$Year,levels = 1980:2016)

d_m3<-tapply(round(cleandata$HMIS_treat,15),list(cleandata$Country_ISO3Code,cleandata$Year),mean)


pred_list<-foreach (i = 1:10) %dopar%{ 
  
  list_value<-list()
  
  for (i_f in 1:3){
    
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

mean_model<-summary(chosen_model_hmis2)$p.table[,1]

sd_model<-summary(chosen_model_hmis2)$p.table[,2]

model_coef<-numeric()

for (i_m in 1:length(mean_model)){
  
  model_coef<-c(model_coef,rnorm(1,mean=mean_model[i_m],sd=sd_model[i_m]))

  
}


d_m3<-tapply(cleandata$HMIS_treat,list(cleandata$Country_ISO3Code,cleandata$Year),mean)

iso_country<-numeric()

#par(mfrow=c(3,2))

list_alg<-as.numeric()

for (cnty in rownames(d_m3)){
  
  # all country
  
  iso_country<-c(iso_country,cnty)
  
  
  d_m_pos<-which(rownames(d_m3)==cnty)

  cov_country<-pred_model_data[ pred_model_data$iso==cnty, ] 
  
  #cov_country$year[cov_country$year<2000]<- 2000
  
 seek_value<-d_m3[d_m_pos,]
 
 
 year_data<-37-length(which(is.na(seek_value)))
 
 if(year_data>2){
   
   
   #n_to_delete<-sample(1:(length(as.numeric(which(seek_value>0)))-1),1)
   
   n_to_delete<-1
   
   seek_value[sample(as.numeric(which(seek_value>0)),n_to_delete)]<-NA
   
 }
 
  
 pos_data<-which(seek_value>0)
  
 #if (length(pos_data)>2){
   
 #  seek_value[sample(pos_data,1)]<-NA
   
 #} 
 
 
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
    
    year_cov2<-cov_country[v, ]
    
    intercept_rand<-seek_value[value]-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
    
    missing_data<-intercept_rand+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
    
    
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
       
       intercept_rand<-seek_value[v-1]-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
       
       missing_data<-intercept_rand+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
       
       
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
        
        intercept_rand1<-seek_value2[prev]-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
        
        missing_data1<-intercept_rand1+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
        
        
        intercept_rand2<-seek_value2[after]-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
        
        missing_data2<-intercept_rand2+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
        
        
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
        
        intercept_rand1<-seek_value3[prev]-(year_cov1[,4]*model_coef[3])-(year_cov1[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
        
        missing_data1<-intercept_rand1+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
        
        
        intercept_rand2<-seek_value3[after]-(year_cov3[,4]*model_coef[3])-(year_cov3[,5]*model_coef[4])-(year_cov1[,6]*model_coef[5])
        
        missing_data2<-intercept_rand2+(year_cov2[,4]*model_coef[3])+(year_cov2[,5]*model_coef[4])+(year_cov2[,6]*model_coef[5])
        

        exp_mid3<-(missing_data1+missing_data2)/2
        
       seek_value3[v]<-exp_mid3
        
        
        back_front<-c(back_front,exp_mid3)
        
        
      }
    } 
    

    
   seek_value<-(seek_value2+seek_value3)/2
    
    
  }
  
  #plot(1980:2016,seek_value,ylim=c(0,1),main=cnty)
  
  #points(1980:2016,c(rep(NA,10),d_m3[d_m_pos,],NA,NA),pch=21,bg=3)
  
  seek_value[seek_value>0.98]<-0.98
  
  seek_value[seek_value<0.05]<-0.05
  
  list_alg<-c(list_alg,runmean(seek_value,5))
}

return(list_alg)

}

first_sim_res<-do.call(cbind,pred_list)

first_sim_res[first_sim_res>1]<-1  

pred_algo_data<-data.frame(iso=rep(rownames(d_m3),each=37),year= rep(1980:2016,length(rownames(d_m3))))

pred_algo_data<-cbind(pred_algo_data,first_sim_res)

#############################################
###### predict place without values #########
#############################################


#cleandata <- droplevels(TreatSeek[-c(which(is.na(TreatSeek$HMIS_treat))),])

#cleandata$Year<-factor(cleandata$Year,levels = 1980:2016)

cleandata <- droplevels(TreatSeek[,c('Any_treat','HMIS_treat','WHO_Sub','Year','HealthExPub','PregWomenCare','PrimaryComplete','DPT','Country','Country_ISO3Code','HMIS_treat_low_SVY', 'HMIS_treat_high_SVY','Any_treat_low_SVY', 'Any_treat_high_SVY','Out_pocket','SurveyType','SurveyName')])

cleandata<-droplevels(cleandata[c(grep('MIS',cleandata$SurveyType),grep('DHS',cleandata$SurveyType)),])

cleandata<-droplevels(cleandata[-c(grep('TZ2015DHS',cleandata$SurveyName)),])

d_m3<-tapply(cleandata$HMIS_treat,list(cleandata$Country_ISO3Code,cleandata$Year),mean)

extra_countries<-sort(gdb_2016_country_list$iso[-c(match(rownames(d_m3),gdb_2016_country_list$iso))])

extra_country=1


formula_hmis3 <-  HMIS_treat ~  HealthExPub+PregWomenCare+  Out_pocket+ (Year) 

chosen_model_hmis3<-gam(formula_hmis3, data=cleandata3)




if(extra_country==1){


pred_list2<-foreach (i = 1:10) %dopar%{ 
  
  list_value<-list()
  
  for (i_f in 1:3){
    
    cov_db<-read.csv(file_name[i_f],sep=';',stringsAsFactors=F)
    
    cov_db<-cov_db[cov_db$year<2017,]
    
    cov_db<-cov_db[cov_db$iso %in% extra_countries,]
    
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
  
  
  names(pred_model_data)<-c('Country','iso','Year',column_names)
  
  pred_model_data<-pred_model_data[ order(pred_model_data$iso,pred_model_data$Year),   ]
  
  ## Region ##
  
  pred_model_data$WHO_Sub<-gdb_2016_country_list$subregion[match(pred_model_data$iso, gdb_2016_country_list$iso)]
 
  
  #head(pred_model_data)
  
  x_pred<-as.data.frame(predict(chosen_model_hmis3,pred_model_data,se.fit=T))
  
  pred_seek_treat<-numeric()
  
  for(i_nor in 1: dim(x_pred)[1]){
    
    pred_seek_treat<-c(pred_seek_treat,rnorm(1,mean=x_pred[i_nor,1],sd=x_pred[i_nor,2]))
    
    
  }
  
  
  return(pred_seek_treat)
  
  
}

cov_db<-read.csv(file_name[1],sep=';',stringsAsFactors=F)

cov_db<-cov_db[cov_db$year<2017,]

cov_db<-cov_db[cov_db$iso %in% extra_countries,]

cov_db<-cov_db[order(cov_db$iso,cov_db$year),]

miss_countries <-do.call(cbind,pred_list2)

miss_countries[miss_countries>1]<-1

pred_model_data<-cbind(cov_db[,2:3],miss_countries)

#pred_model_data[pred_model_data$iso=='TC', ]

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
  
 # plotCI( country_db$Year, country_db$HMIS_treat,ui=country_db$HMIS_treat_high,li=country_db$HMIS_treat_low
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

}


multi_list<-list()

for (i_u in 1:length(union_list)){
  
  x<-union_list[[i_u]]
  
  multi_list[[length(multi_list)+1]]<-x[,-c(1,2)]
  
}



multi_db<-do.call(cbind,multi_list)


sim_mean<-apply(multi_db,1,mean) 

sim_sd<-apply(multi_db,1,sd)

sim_ui<-sim_mean+1.96*sim_sd

sim_li<-sim_mean-1.96*sim_sd  


sim_mean[which(sim_mean<0)]<-0

sim_mean[which(sim_mean>1)]<-1

sim_ui[which(sim_ui<0)]<-0

sim_ui[which(sim_ui>1)]<-1

sim_li[which(sim_li<0)]<-0

sim_li[which(sim_li>1)]<-1


x<-union_list[[i_u]]

treat_seek_pub<-data.frame(iso=x[,1],year=x[,2],mean=sim_mean,upper=sim_ui,lower=sim_li)


write.csv(treat_seek_pub,'treat_seek_fraction_pub_no_year_19_Jul_2017.csv',quote=F,row.names=F)

#first_sim_res[which(first_sim_res<0)]<-0

#first_sim_res[which(first_sim_res>1)]<-1

#sim_mean<-apply(first_sim_res,1,mean) 

#sim_low<-apply(first_sim_res,1,quantile,prob=0.025)

#sim_low<-apply(first_sim_res,1,quantile,prob=0.025)

#cleandata <- droplevels(na.omit(TreatSeek))

#cleandata <- droplevels(na.omit(TreatSeek[,c('HMIS_treat','WHO_Sub','Year','HealthExPub','PregWomenCare','PrimaryComplete','DPT','Country','Country_ISO3Code','HMIS_treat_low', 'HMIS_treat_high')]))

cleandata <- droplevels(TreatSeek[,c('Any_treat','HMIS_treat','WHO_Sub','Year','HealthExPub','PregWomenCare','PrimaryComplete','DPT','Country','Country_ISO3Code','HMIS_treat_low_SVY', 'HMIS_treat_high_SVY','Any_treat_low_SVY', 'Any_treat_high_SVY')])



pdf('treat_seek_fraction_pub_no_year_19_Jul_2017.pdf',width=8.7,height = 11.2)

par(mfrow=c(3,2))

for( i in unique(treat_seek_pub$iso) ){
  
  country_db<-treat_seek_pub[ treat_seek_pub$iso==i, ]
  
  plot(0,0,type='n',ylim=c(0,1),xlim=c(2000,2016),main=i,ylab='% Sougth Treatment',xlab='Year')
  
  plotCI(1980:2016,country_db$mean,ui=country_db$upper,li=country_db$lower,ylim=c(0,1),add=T)
  
  in_out<-which(cleandata$Country_ISO3Code==i)
  
  if(length(in_out)>0){
    
    
    country_line<- cleandata[which(cleandata$Country_ISO3Code==i),]
    
    points(country_line$Year,1-((country_line$Any_treat-country_line$HMIS_treat)/country_line$Any_treat),pch=21,bg=2)
    
  }
  
}
  
dev.off()

###### Fraction public #####

treat_seek_any<-read.csv('treat_seek_any_no_year_19_Jul_2017.csv')

any_iso_year<-paste(treat_seek_any$iso,treat_seek_any$year,sep='_')

pub_iso_year<-paste(treat_seek_pub$iso,treat_seek_pub$year,sep='_')

treat_seek_any<-treat_seek_any[match(pub_iso_year,any_iso_year),]


head(treat_seek_pub)

mean_pub<-round(treat_seek_any[,3]*treat_seek_pub[,3],5)

upper_pub<-round(treat_seek_any[,4]*treat_seek_pub[,3],5)
  
lower_pub<-round(treat_seek_any[,5]*treat_seek_pub[,3],5) 

treat_seek_pub2<-cbind(treat_seek_pub[,1:2],data.frame(mean=mean_pub,upper=upper_pub,lower=lower_pub))

head(treat_seek_pub2)

write.csv(treat_seek_pub2,'treat_seek_pub_no_year_19_Jul_2017.csv',quote=F,row.names=F)

pdf('treat_seek_pub_no_year_19_Jul_2017.pdf',width=8.7,height = 11.2)

par(mfrow=c(3,2))

for( i in unique(treat_seek_pub2$iso) ){
  
  country_db<-treat_seek_pub2[ treat_seek_pub2$iso==i, ]
  
  plot(0,0,type='n',ylim=c(0,1),xlim=c(2000,2016),main=i,ylab='% U5 fevers sought reatment at gov. facilities',xlab='Year')
  
  plotCI(1980:2016,country_db$mean,ui=country_db$upper,li=country_db$lower,ylim=c(0,1),add=T)
  
  in_out<-which(cleandata$Country_ISO3Code==i)
  
  if(length(in_out)>0){
    
    
    country_line<- cleandata[which(cleandata$Country_ISO3Code==i),]
    
    #points(country_line$Year,(country_line$HMIS_treat),pch=21,bg=2)
    
    plotCI(country_line$Year,country_line$HMIS_treat,ui = country_line$HMIS_treat_high,li = country_line$HMIS_treat_low,pch=21,pt.bg =2,add=T)
    
  }
  
  legend('topright',legend=c('DHS','Predicted'),pch=21,pt.bg=c('red','white'))
  
 # lines(1980:2016,runmean(country_db$mean,5),lwd=0.8,col=2)
  
 # lines(1980:2016,runmean(country_db$upper,5),lwd=0.8,col=2)
  
 # lines(1980:2016,runmean(country_db$lower,5),lwd=0.8,col=2)
  
}

dev.off()



###### Fraction private #####

treat_seek_any<-read.csv('treat_seek_any_no_year_19_Jul_2017.csv')

head(treat_seek_pub)

any_iso_year<-paste(treat_seek_any$iso,treat_seek_any$year,sep='_')

pub_iso_year<-paste(treat_seek_pub$iso,treat_seek_pub$year,sep='_')

treat_seek_any<-treat_seek_any[match(pub_iso_year,any_iso_year),]


mean_pub<-round(treat_seek_any[,3]*(1-treat_seek_pub[,3]),5)

upper_pub<-round(treat_seek_any[,4]*(1-treat_seek_pub[,5]),5)

lower_pub<-round(treat_seek_any[,5]*(1-treat_seek_pub[,4]),5) 

treat_seek_pub3<-cbind(treat_seek_pub[,1:2],data.frame(mean=mean_pub,upper=upper_pub,lower=lower_pub))

head(treat_seek_pub3)

write.csv(treat_seek_pub3,'treat_seek_priv_no_year_19_Jul_2017.csv',quote=F,row.names=F)

pdf('treat_seek_priv_no_year_19_Jul_2017.pdf',width=8.7,height = 11.2)

par(mfrow=c(3,2))

for( i in unique(treat_seek_pub3$iso) ){
  
  country_db<-treat_seek_pub3[ treat_seek_pub3$iso==i, ]
  
  plot(0,0,type='n',ylim=c(0,1),xlim=c(2000,2016),main=i,ylab='% U5 fevers sought treatment at private facilities',xlab='Year')
  
  plotCI(1980:2016,country_db$mean,ui=country_db$upper,li=country_db$lower,ylim=c(0,1),add=T)
  
  in_out<-which(cleandata$Country_ISO3Code==i)
  
  if(length(in_out)>0){
    
    
    country_line<- cleandata[which(cleandata$Country_ISO3Code==i),]
    
    points(country_line$Year,(country_line$Any_treat-country_line$HMIS_treat),pch=21,bg=2)
    
  }
  
  legend('topright',legend=c('DHS','Predicted'),pch=21,pt.bg=c('red','white'))
  
}

dev.off()




