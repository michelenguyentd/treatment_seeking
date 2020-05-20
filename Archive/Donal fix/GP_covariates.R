rm(list = ls())

library(kernlab)
library("GPfit")
library(VIM)

# Config data to select countries to model:
config_file <- read.csv("Z:/Config_Data/National_Config_Data.csv")

# Count number of countries to be modelled for treatment seeking: 
admin0_units <- config_file[config_file$MAP_Include == "Y", ]
nrow(admin0_units)

# Create list of iso for countries to model:
country_list <- admin0_units$ISO3

setwd("J:/Treatment_Seeking/WDI_csv")
graphics.path <- "J:/Treatment_Seeking/WB covariate plots/"
  
WDI_Data <-read.csv('WDIData.csv',stringsAsFactors=F)

data_name<-c('public_exp','primary_school','DPT','pregnant','GDP_grow','tot_exp', 'out_of_pocket')

file_name<-c('J:/Treatment_Seeking/WDI_csv/public_exp.csv',
             'J:/Treatment_Seeking/WDI_csv/primary_school.csv',
             'J:/Treatment_Seeking/WDI_csv/DPT.csv',
             'J:/Treatment_Seeking/WDI_csv/pregnant.csv',
             'J:/Treatment_Seeking/WDI_csv/GDP_grow.csv',
             'J:/Treatment_Seeking/WDI_csv/tot_ex.csv', 
             'J:/Treatment_Seeking/WDI_csv/out_of_pocket.csv')

Ind_codes<-c('SH.XPD.PUBL', 'SE.PRM.CMPT.ZS', 'SH.IMM.IDPT', 'SH.STA.ANVC.ZS', 'NY.GDP.MKTP.KD.ZG', 'SH.XPD.TOTL.ZS', 'SH.XPD.OOPC.TO.ZS')

# Check that codes match the names:

for (i in 1:length(Ind_codes)){
  print(unique(WDI_Data$Indicator.Name[WDI_Data$Indicator.Code == Ind_codes[i]]))
}

years <- 1980:2017
year_codes <- paste("X", years, sep = '')

# Check sparsity of data per country per indicator:

for (i_l in 1:length(Ind_codes)){
  db_GDP<-WDI_Data[WDI_Data$Indicator.Code == Ind_codes[i_l], ]
  db_GDP$Country.Code[which(db_GDP$Country.Code=='ZAR')]<-'COD'
  db_GDP$Country.Code[which(db_GDP$Country.Code=='TMP')]<-'TLS'
  
  cov_matrix <- rep(NA, length(years))
  
  for (i in 1:length(country_list)){
    cov_row <- as.numeric(db_GDP[db_GDP$Country.Code == country_list[i], year_codes, ])
    cov_matrix <- rbind(cov_matrix, cov_row)
  }
  
  cov_matrix <- cov_matrix[-1, ]
  
  row.names(cov_matrix) <- country_list
  labels <- years
  
  paper_subset <- cov_matrix[, 21:ncol(cov_matrix)]
  y.ticks <- which(apply(paper_subset, MARGIN = 1, FUN = function(x){sum(is.na(x))})/length(21:ncol(cov_matrix)) > 0.6) # Find the countries which have less than 60% of data in between 2000-2017.
  x.ticks <- which(years %in% c(1980, 1995, 2000, 2014, 2017))
  
  pdf(paste(graphics.path, data_name[i_l], "_missingvalues.pdf", sep = ""), height = 10, width = 12)
  par(mar = c(2, 4, 2, 2))
  matrixplot(cov_matrix[, ], main = paste("Missing values for ", data_name[i_l]), xlab = '', ylab = "", axes = FALSE, labels = "")
  axis(side = 1, at = x.ticks, labels = labels[x.ticks], tick = FALSE, las = 1, tck = 0, col = 'white')
  axis(side = 2, at = y.ticks, labels = country_list[y.ticks], tick = FALSE, las = 1, tck = 0, col = 'white')
  dev.off()
}

temp_time <- proc.time()[3]

for (i_l in 1:length(Ind_codes)){

db_GDP<-WDI_Data[WDI_Data$Indicator.Code == Ind_codes[i_l], ]

db_GDP$Country.Code[which(db_GDP$Country.Code=='ZAR')]<-'COD'

db_GDP$Country.Code[which(db_GDP$Country.Code=='TMP')]<-'TLS'

db_GDP <- db_GDP[db_GDP$Country.Code %in% country_list, ]

list_cube<-list() # For storing gapfilled country series.

for (i in 1:length(db_GDP[,1])){ 
  
  line_chosen<-db_GDP[i,]
 
  plot <- 0
 
 country_value<-as.numeric(line_chosen[year_codes])
 
 if (plot==1){
   
   plot(years, country_value,main=line_chosen[1],
        ylim=c(min(country_value,na.rm=t),max(country_value,na.rm=t)),pch=21,bg=3)
   
 }
 
 if(length(which(is.na(country_value)))<length(years)-1){ # If you have at least 1 data point... (too lax?)
 
 db_model<-na.omit(as.data.frame(cbind(x = years,country_value)))
 
 GPmodel <- GP_fit(db_model$x,db_model$country_value,maxit = 1000,nug_thres=20)
 
 pred_GP<-predict(GPmodel,data.frame(x=years),interval='predict')
 
 if (plot==1){
 
 lines(years,pred_GP$Y_hat,ylim=c(0,100))
 
 lines(years,pred_GP$Y_hat+1.96*sqrt(pred_GP$MSE),col=2)
 
 lines(years,pred_GP$Y_hat-1.96*sqrt(pred_GP$MSE),col=2)
 
 }
 
 country_cube<-cbind(line_chosen[1],line_chosen[2],as.data.frame(pred_GP$complete_data))
 
 names(country_cube)<-c('country','iso','year','gp_value','mse')
 
 }else{
   
  country_cube<-cbind(line_chosen[1],line_chosen[2],as.data.frame(matrix (NA,ncol=3,nrow=37)))
   
  names(country_cube)<-c('country','iso','year','gp_value','mse')
  
   
 }
 
 list_cube[[length(list_cube)+1]]<-country_cube
 
 
}


combine_gp_results<-do.call(rbind,list_cube)

missing_country<- c(unique(combine_gp_results$iso[which(is.na(combine_gp_results$gp_value))]), "GUF", "MYT") # GUF and MYT in GBD list but not in WB list.

list_missing_GP<-list()

for (i_iso in missing_country){

  target_region<-na.omit(config_file[config_file$ISO3==i_iso,]$WHO_Subregion)[1]
  
  reg_contries<-config_file[config_file$WHO_Subregion==target_region & config_file$ISO3 %in% country_list,]$ISO3
  
  reg_db<-db_GDP[db_GDP[,2] %in% reg_contries, year_codes]

  db_model<-na.omit(as.data.frame(cbind(as.vector(matrix(rep(years,dim(reg_db)[1]),ncol=length(years),byrow=T)),as.vector(as.matrix(reg_db)))))
  
  if(sum(as.numeric(duplicated(db_model)))>1){

   db_model<-db_model[-c(which(duplicated(db_model)==T)),]

  }

pdf(paste(graphics.path, data_name[i_l], '_', target_region,"_for_", i_iso, ".pdf", sep = ""), height = 10, width = 12)
  
plot(db_model$V1,db_model$V2,ylim=c(min(db_GDP[,year_codes],na.rm=t),max(db_GDP[,year_codes],na.rm=t)), ylab = data_name[i_l], main = paste(target_region,"for", i_iso, sep = " "), pch=21,bg=3)

dev.off()


list_pred<-list()

for(j in 1:100){
  
  year_sample<-numeric()
  
  value_sample<-numeric()
  
  set.seed(j)
  
  random_years <- sort(sample(unique(db_model$V1),8))  # Randomly select 8 unique years. 
  
  for(i_y in random_years){
    
    year_values<-db_model[db_model$V1==i_y,] # Get all data from region for that year.
    
    set.seed(j)
    
    row_year<-year_values[sample(c(1:dim(year_values)[1]),1),] # Randomly select one country to give data for that year.
    
    year_sample<-c( year_sample,row_year[1,1])
    
    value_sample<-c(value_sample,row_year[1,2])
    
  }
  
  GPmodel <- GP_fit(year_sample,value_sample,nug_thres = 10,maxit=1000)  
  
  pred_GP<-predict(GPmodel,data.frame(V1=years),interval='predict')
  
  list_pred[[j]]<-pred_GP$complete_data
  
}


res_reg_GP<-do.call(rbind,list_pred)

#par(mfrow=c(4,2))

year<-numeric()

gp_value<-numeric()

mse<-numeric()

for (i_y2 in years){


year_reg_GP<-res_reg_GP[res_reg_GP[,1]==i_y2,]


dist_year<-rnorm(10000,mean=year_reg_GP[,2],sd=sqrt(year_reg_GP[,3])) # 100 random samples from each of the 100 prediction distributions for each year. 


year<-c(year,i_y2)

gp_value<-c(gp_value,mean(dist_year)) # mean of the 10000 random samples.

mse<-c(mse,(sd(dist_year))^2) # variance of the 1000 random samples.

}


country_name<-na.omit(config_file[config_file$ISO3 ==i_iso,]$MAP_Country_Name)[1]

missing_GP_results<-data.frame(country=country_name,iso=i_iso,year,gp_value,mse)


list_missing_GP[[length(list_missing_GP)+1]]<-missing_GP_results

}

combine_gp_results2<-na.omit(combine_gp_results)



combine_gp_results3<-rbind(combine_gp_results2,do.call(rbind,list_missing_GP))

write.table(combine_gp_results3,file_name[i_l],sep=';',quote=T,row.names=F)

### Plots of gap-filled WB covariates ###

pdf(paste(graphics.path, data_name[i_l], '.pdf', sep = ''), width=8.7,height = 11.2)

par(mfrow=c(4,2))

for (i in 1:length(country_list)){
  
  country_line <- combine_gp_results3[combine_gp_results3$iso==country_list[i],]
  
  medianline <- country_line$gp_value
  upperline <- country_line$gp_value + qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  lowerline <- country_line$gp_value - qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  
  plot(country_line$year,upperline, ylim = c(0, 100), col="white",xlab="Year",ylab="",main=unique(country_line$country))
  polygon(c(country_line$year,rev(country_line$year)),c(lowerline,rev(upperline)),bg="yellow",border="yellow",col="yellow",density=-1)
  lines(country_line$year,medianline,col="blue")
  
}
dev.off()


}

timetaken <- proc.time()[3] - temp_time # Less than 20 minutes.
