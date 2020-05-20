
### This is a R script to conduct Gaussian process regression for the World Bank covariates - originally written by Donal, edited for out of pocket covariate by Michele ###

library(kernlab)

# library(doMC)

library("GPfit")

# registerDoMC(cores=10)

# gdb_2016_country_list <- read.csv("/home/local/ZOO/zool1286/Desktop/treatment_seeking_behaviour/gdb_2016_country_list.csv", sep=";",stringsAsFactors=F)
config_file <- read.csv("Z:/GBD2017/Processing/Stages/02b_Model_TS/Archive/Verified_Outputs/treat_seek_any_no_year_19_Jul_2017.csv")
# Create list of iso for countries to model (tentatively use output file from Donal):
gdb_2016_country_list <- unique(config_file$iso)
length(gdb_2016_country_list) # 107 countries.

config_file2 <- read.csv("Z:/GBD2017/Processing/Stages/01b2_World_Bank_Covariates/Checkpoint_Outputs/DPT_jul_2017.csv", sep = ";")
WB_country_list <- unique(config_file2$iso)

length(WB_country_list) # 109 countries. Why different from gdb_2016_country_list?

config_file3 <- read.csv("Z:/GBD2017/Processing/Config_Data/Combined_Config_Data.csv")
str(config_file3)

##### ------- To do: Update config file using GBD config file as per Treatment Seeking R Code.

setwd('J:/Treatment_Seeking/WDI_csv' )

list_file<-c('WDIData.csv') # Can include other raw data files from WB later.

data_name<-c('Out_pocket')

file_name<-c('J:/Treatment_Seeking/WDI_csv/out_of_pocket.csv')


# for (i_l in 1:length(list_file)){

i_l <- 1 # At the moment, only have one covariate to gap fill. 

WDI_Data<-read.csv(list_file[i_l],stringsAsFactors=F)

db_GDP<-WDI_Data[WDI_Data$Indicator.Name == "Out-of-pocket health expenditure (% of total expenditure on health)", ]

db_GDP$Country.Code[which(db_GDP$Country.Code=='ZAR')]<-'COD'

db_GDP$Country.Code[which(db_GDP$Country.Code=='TMP')]<-'TLS'

# Only consider those countries for which we model for GBD:

db_GDP <- db_GDP[db_GDP$Country.Code %in% WB_country_list , ]
nrow(db_GDP) # 107 countries. For GUF and MYT, we need to predict without data?
# WB_country_list[(!(WB_country_list %in% db_GDP$Country.Code))] # GUF and MYT.
# gdb_2016_country_list[(!(gdb_2016_country_list %in% db_GDP$Country.Code))] # GUF and MYT.
# db_GDP$Country.Code[(!(db_GDP$Country.Code %in% gdb_2016_country_list))] # CSS and OSS: Caribbean sm, Other small. 
# config_file2[config_file2$iso %in% c("CSS", "OSS"), ]

# par(mfrow=c(4,2))
# 
# 
# plot(0,0,xlim=c(0,100),ylim=c(0,100),type='n',axes=F,xlab='',ylab='')
# 
# text(50,50,data_name[i_l],cex=2)
# 
# box()

list_cube<-list()

for (i in c(1:length(db_GDP[,1]))){ # Each country.

  line_chosen<-db_GDP[i,]
  
 line_chosen[-c(1:24, 63)] # 1980:2017. 
 
 plot<-0
 
 if (plot==1){
   
   plot(1980:2017,as.numeric(line_chosen[-c(1:24, 63)]),main=line_chosen[1],
        ylim=c(min(db_GDP[,-c(1:24, 63)],na.rm=t),max(db_GDP[,-c(1:24, 63)],na.rm=t)),pch=21,bg=3)
   
 }
 
 
 
 country_value<-as.numeric(line_chosen[-c(1:24, 63)])
  
 if(length(which(is.na(line_chosen[-c(1:24, 63)])))<35){ # Have at least 3 data points.
 

 
 x<-1980:2017
 
 db_model<-na.omit(as.data.frame(cbind(x,country_value)))
 
 
 GPmodel <- GP_fit(db_model$x,db_model$country_value,maxit = 1000,nug_thres=20) # Default is an exponential correlation, model is year plus GP.
 
 pred_GP<-predict(GPmodel,data.frame(x=1980:2017),interval='predict')
 
 if (plot==1){
 
 lines(1980:2017,pred_GP$Y_hat,ylim=c(0,100))
 
 lines(1980:2017,pred_GP$Y_hat+1.96*sqrt(pred_GP$MSE),col=2)
 
 lines(1980:2017,pred_GP$Y_hat-1.96*sqrt(pred_GP$MSE),col=2)
 
 }
 
 country_cube<-cbind(line_chosen[1],line_chosen[2],as.data.frame(pred_GP$complete_data))
 
 names(country_cube)<-c('country','iso','year','gp_value','mse')
 
 }else{
   
  country_cube<-cbind(line_chosen[1],line_chosen[2],as.data.frame(matrix (NA,ncol=3,nrow=38)))
   
  names(country_cube)<-c('country','iso','year','gp_value','mse')
  
   
 }
 
 list_cube[[length(list_cube)+1]]<-country_cube
 
 
}


combine_gp_results<-do.call(rbind,list_cube)


missing_country<-c(unique(combine_gp_results$iso[which(is.na(combine_gp_results$gp_value))]), 'GUF', 'MYT')

list_missing_GP<-list()


for (i_iso in missing_country){

target_region<-na.omit(config_file3[config_file3$ISO3==i_iso,]$WHO_Subregion)[1]

reg_contries<-config_file3[config_file3$WHO_Subregion==target_region & config_file3$ISO3 %in% WB_country_list,]$ISO3

reg_db<-db_GDP[db_GDP[,2] %in% reg_contries, -c(1:24, 63)]

db_model<-na.omit(as.data.frame(cbind(as.vector(matrix(rep(1980:2017,dim(reg_db)[1]),ncol=38,byrow=T)),as.vector(as.matrix(reg_db)))))

if(sum(as.numeric(duplicated(db_model)))>1){

db_model<-db_model[-c(which(duplicated(db_model)==T)),]

}

plot(db_model$V1,db_model$V2,ylim=c(min(db_GDP[,-c(1:4,41)],na.rm=t),max(db_GDP[,-c(1:4,41)],na.rm=t)),pch=21,bg=3)


list_pred<-list()

for(i in 1:100){

  year_sample<-numeric()
  
  value_sample<-numeric()
  
  for(i_y in sort(sample(unique(db_model$V1),8))){
    
    year_values<-db_model[db_model$V1==i_y,]
    
    row_year<-year_values[sample(c(1:dim(year_values)[1]),1),]
    
    year_sample<-c( year_sample,row_year[1,1])
    
    value_sample<-c(value_sample,row_year[1,2])
  
  }
  
GPmodel <- GP_fit(year_sample,value_sample,nug_thres = 10,maxit=1000)  
  
pred_GP<-predict(GPmodel,data.frame(V1=1980:2017),interval='predict')

list_pred[[i]]<-pred_GP$complete_data

plot(year_sample,value_sample,ylim=c(min(db_GDP[,-c(1:4,41)],na.rm=t),max(db_GDP[,-c(1:4,41)],na.rm=t)),pch=21,bg=3)

lines(1980:2017,pred_GP$Y_hat,ylim=c(0,100))

lines(1980:2017,pred_GP$Y_hat+1.96*sqrt(pred_GP$MSE),col=2)

lines(1980:2017,pred_GP$Y_hat-1.96*sqrt(pred_GP$MSE),col=2)

}


res_reg_GP<-do.call(rbind,list_pred)

#par(mfrow=c(4,2))


year<-numeric()

gp_value<-numeric()

mse<-numeric()

for (i_y2 in 1980:2017){


year_reg_GP<-res_reg_GP[res_reg_GP[,1]==i_y2,]


dist_year<-rnorm(10000,mean=year_reg_GP[,2],sd=sqrt(year_reg_GP[,3])) # How does this work?


year<-c(year,i_y2)

gp_value<-c(gp_value,mean(dist_year))

mse<-c(mse,(sd(dist_year))^2)

}


country_name<-na.omit(config_file3[config_file3$ISO3 ==i_iso,]$MAP_Country_Name)[1]

missing_GP_results<-data.frame(country=country_name,iso=i_iso,year,gp_value,mse)


list_missing_GP[[length(list_missing_GP)+1]]<-missing_GP_results

}

combine_gp_results2<-na.omit(combine_gp_results)



combine_gp_results3<-rbind(combine_gp_results2,do.call(rbind,list_missing_GP))

write.table(combine_gp_results3,file_name[i_l],sep=';',quote=T,row.names=F)

# }

### Plots of gap-filled WB covariates ###

out_of_pocket <- read.csv("J:/Treatment_Seeking/WDI_csv/out_of_pocket.csv", sep = ";")
master.iso <- unique(out_of_pocket$iso)

pdf('out_of_pocket.pdf',width=8.7,height = 11.2)

par(mfrow=c(4,2))

for (i in 1:length(master.iso)){
  
  country_line <- out_of_pocket[out_of_pocket$iso==master.iso[i],]
  
  medianline <- country_line$gp_value
  upperline <- country_line$gp_value + qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  lowerline <- country_line$gp_value - qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  
  plot(country_line$year,upperline, ylim = c(0, 100), col="white",xlab="Year",ylab="",main=unique(country_line$country))
  polygon(c(country_line$year,rev(country_line$year)),c(lowerline,rev(upperline)),bg="yellow",border="yellow",col="yellow",density=-1)
  lines(country_line$year,medianline,col="blue")

}
dev.off()


public_exp <- read.csv("Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/public_exp_jul_2017.csv", sep = ";")
master.iso <- unique(public_exp$iso)

pdf('public_exp.pdf',width=8.7,height = 11.2)

par(mfrow=c(4,2))

for (i in 1:length(master.iso)){
  
  country_line <- public_exp[public_exp$iso==master.iso[i],]
  
  medianline <- country_line$gp_value
  upperline <- country_line$gp_value + qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  lowerline <- country_line$gp_value - qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  
  plot(country_line$year,upperline, ylim = c(0, 100), col="white",xlab="Year",ylab="",main=unique(country_line$country))
  polygon(c(country_line$year,rev(country_line$year)),c(lowerline,rev(upperline)),bg="yellow",border="yellow",col="yellow",density=-1)
  lines(country_line$year,medianline,col="blue")
  
}
dev.off()

pregnant <- read.csv('Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/pregnant_jul_2017.csv', sep = ";")
master.iso <- unique(pregnant$iso)

pdf('pregnant.pdf',width=8.7,height = 11.2)

par(mfrow=c(4,2))

for (i in 1:length(master.iso)){
  
  country_line <- pregnant[pregnant$iso==master.iso[i],]
  
  medianline <- country_line$gp_value
  upperline <- country_line$gp_value + qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  lowerline <- country_line$gp_value - qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  
  plot(country_line$year,upperline, ylim = c(0, 100), col="white",xlab="Year",ylab="",main=unique(country_line$country))
  polygon(c(country_line$year,rev(country_line$year)),c(lowerline,rev(upperline)),bg="yellow",border="yellow",col="yellow",density=-1)
  lines(country_line$year,medianline,col="blue")
  
}
dev.off()

