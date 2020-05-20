rm(list = ls()) # Clear working environment.
library(TMB) # Load libraries required.
library(sparseMVN)
library(Matrix)
library(gtools)

setwd('J:/Treatment_Seeking/')

graphics.path <- 'J:/Treatment_Seeking/graphics/'
data.path <- 'J:/Treatment_Seeking/Data/'
realizations.path <- 'J:/Treatment_Seeking/realizations/'
output.suffix <- '.Any'

# Years to model:
years <- 1980:2017

# Read in full datasets:

full_TreatSeek <- read.csv(paste(data.path, "full_TreatSeek.csv", sep = ""))
full_TreatSeek_n <- read.csv(paste(data.path, "full_TreatSeek_n.csv", sep = ""))

master.region.list <- unique(full_TreatSeek_n$IHME_Region_Name)
 
# Try for Eastern Sub-Saharan Africa:

# Create input matrices for logit_Any for each of the 12 IHME regions. 
# Do logit_HMISfrac later....

# 1. Eastern Sub-Saharan Africa:

ESSA.data <- full_TreatSeek_n[full_TreatSeek_n$IHME_Region_Name == "Eastern Sub-Saharan Africa", ] 
ESSA.list <- unique(ESSA.data$Admin_Unit_Name)

ESSA.Any <- ESSA.Any.low <- ESSA.Any.high <- matrix(NA,ncol=length(years),nrow=length(ESSA.list))
for (i in 1:length(ESSA.list)) {
  for (j in 1:length(years)) {
    if (length(which(ESSA.data$Admin_Unit_Name==ESSA.list[i] & ESSA.data$Year == years[j])) > 0) {
      ESSA.Any[i,j]      <- ESSA.data$Any_treat[ESSA.data$Admin_Unit_Name== ESSA.list[i] & ESSA.data$Year==years[j]]
      ESSA.Any.low[i,j]  <- ESSA.data$Any_treat_low_SVY[ESSA.data$Admin_Unit_Name== ESSA.list[i] & ESSA.data$Year==years[j]]
      ESSA.Any.high[i,j] <- ESSA.data$Any_treat_high_SVY[ESSA.data$Admin_Unit_Name== ESSA.list[i] & ESSA.data$Year==years[j]]
    }
  }
}


plot(years, ESSA.Any[1, ], type = 'p')

ESSA.cov <- list()

# For checking: Be careful of factor levels!

ANC4 <- DMSP <- matrix(NA,ncol=length(years),nrow=length(ESSA.list))
for (i in 1:length(ESSA.list)) {
  for (j in 1:length(years)) {
    if (length(which(ESSA.data$Admin_Unit_Name==ESSA.list[i] & ESSA.data$Year == years[j])) > 0) {
      ANC4[i,j]      <- ESSA.data$ANC4_coverage_prop[ESSA.data$Admin_Unit_Name== ESSA.list[i] & ESSA.data$Year==years[j]]
      DMSP[i,j]  <- ESSA.data$DMSP_nighttime[ESSA.data$Admin_Unit_Name== ESSA.list[i] & ESSA.data$Year==years[j]]
    }
  }
}

ESSA.cov[[1]] <- ANC4
ESSA.cov[[2]] <- DMSP

# Actually assemble the list of matrices (in this case of length 6 due to the 6 covariates) into a 3-D array
covariate.array <- array(NA,c(dim(ESSA.Any)[2],2,dim(ESSA.Any)[1]))
for (i in 1:2) {covariate.array[,i,] <- t(ESSA.cov[[i]])}
ESSA.covariates <- covariate.array

source("gapfill_Any_timeseries_rt.R")

## Run Any timeseries construction code
temptime <- proc.time()[3]
Any_timeseries_realisations <- gapfill_Any_timeseries_rt(Any_mean=ESSA.Any,Any_low=ESSA.Any.low,Any_high=ESSA.Any.high, covariates = covariates)
timetaken <- proc.time()[3] - temptime

## Visualise Results
N_countries <- dim(Any_timeseries_realisations)[1]
#N_square <- which.min(((1:20)^2-N_countries)^2)
#if (N_square^2 < N_countries) {N_square <- N_square+1}

#X11(width=18,height=9)
#layout(t(matrix(1:(N_square^2),nrow=N_square)))
#par(mai=c(0.0,0.5,0.0,0.0))
for (i in 1:N_countries) {
  #for (i in 1:2) {
  tempfilename <- paste(graphics.path,ESSA.list[i],output.suffix,".png", sep = "")
  png(filename = tempfilename, width=600, height=400, units = "px", pointsize = 10)
  upperline <- lowerline <- medianline <- meanline <- numeric(dim(Any_timeseries_realisations)[2])
  for (j in 1:length(medianline)) {
    medianline[j] <- quantile(Any_timeseries_realisations[i,j,],0.5)
    upperline[j] <- quantile(Any_timeseries_realisations[i,j,],0.975)
    lowerline[j] <- quantile(Any_timeseries_realisations[i,j,],0.025)
    meanline[j] <- mean(Any_timeseries_realisations[i,j,])
  }
  plot(1:(dim(Any_timeseries_realisations)[2]),upperline, ylim = c(0, 1), yaxt='n',xaxt='n',col="white",xlab="Year",ylab="Any Treatment Seeking",main=ESSA.list[i])
  polygon(c(1:(dim(Any_timeseries_realisations)[2]),rev(1:(dim(Any_timeseries_realisations)[2]))),c(lowerline,rev(upperline)),bg="yellow",border="yellow",col="yellow",density=-1)
  lines(1:(dim(Any_timeseries_realisations)[2]),medianline,col="magenta")
  lines(1:(dim(Any_timeseries_realisations)[2]),meanline,col="blue")
  points((1:(dim(Any_timeseries_realisations)[2]))[!is.na(ESSA.Any[i,])],ESSA.Any[i,!is.na(ESSA.Any[i,])],pch=19)
  x <- (1:(dim(Any_timeseries_realisations)[2]))[!is.na(ESSA.Any[i,])]
  y1 <- ESSA.Any.low[i,!is.na(ESSA.Any[i,])]
  y2 <- ESSA.Any.high[i,!is.na(ESSA.Any[i,])]
  for (j in 1:length(x)) {lines(rep(x[j],2),c(y1[j],y2[j]))}
  box()
  axis(2,at=seq(0, 1, by = 0.1),labels=seq(0, 1, by = 0.1),las=2,hadj=0.75)
  axis(1,at=c(1,6,11,16,21,26,31,36),labels=c("1980","1985","1990","1995","2000","2005","2010","2015"),las=0,hadj=0.75)
  dev.off()
}

Any_timeseries_realisations_1 <- Any_timeseries_realisations

