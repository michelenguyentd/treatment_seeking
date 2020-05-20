## R function (running TMB) to perform API gap-filling and timeseries inference procedure

## Supposes that gold and silver classification data are identifiable as those with zero width input error estimates (i.e., those i,j such that API_low[i,j] = API_mean[i,j] = API_high[i,j])
## Supposes missing data indicated by NAs
## Supposes lower and upper bounds are approx equal and have been estimated as approx 95% confidence intervals

## Requires:
# API_mean = an N_national (rows) x N_years (columns) matrix giving estimated mean APIs (units of per 1000 PYO)
# API_low = an N_national (rows) x N_years (columns) matrix giving estimated lower bound on APIs (units of per 1000 PYO)
# API_high = an N_national (rows) x N_years (columns) matrix giving estimated upper bound on APIs (units of per 1000 PYO)
# Covariates = an N_years x N_covariates x N_national array giving covariates for modelling

# For testing function:
# API_mean <- Region.api; API_low <- Region.api.low; API_high <- Region.api.high; covariates <- covariates

gapfill_API_timeseries_pf_ar <- function(API_mean, API_low, API_high, covariates){
  # Note: Need to take 1000 national API realisations and compute API_mean and API_sd to read in beforehand.

  # Catch basic input errors
  if (!(dim(API_mean)==dim(API_low) && dim(API_mean)==dim(API_high))) {stop("API_mean,API_low,_API_high matrices not equal sizes.")}
  if (length(which(API_mean[!is.na(API_mean)]>1000))>0) {stop("API_mean contains entries > 1000 cases per 1000 PYO: unrealistically high.")}
  if (length(which(API_high[!is.na(API_high)]>2000))>0) {stop("API_high contains entries > 2000 cases per 1000 PYO: unrealistically high.")}

  # Prepare TMB input data list
  N_years <- ncol(API_mean)
  N_covariates <- dim(covariates)[2]
  missing_data <- matrix(0,nrow=1,ncol=N_years)
  missing_data[is.na(API_mean)] <- 1

  fixed_data <- matrix(0,nrow=1,ncol=N_years)
  fixed_data[API_high==API_low] <- 1

  API_mean[is.na(API_mean)] <- -99
  API_sd <- matrix(-99,nrow=1,ncol=N_years)
  API_sd[missing_data==0 & fixed_data==0] <- ((API_high-API_mean)/2+(API_mean-API_low)/2)[missing_data==0 & fixed_data==0]/diff(qnorm(c(0.025,0.975)))
  API_sd[API_sd<0.001 & API_sd > -99] <- 0.001

  data <- list()

  data$N_years <- N_years
  data$N_covariates <- N_covariates
  data$API_mean <- API_mean
  data$API_sd <- API_sd
  data$fixed_data <- fixed_data
  data$missing_data <- missing_data
  data$covariates <- covariates

  parameters <- list()

  parameters$mean_log_scalefactor_noise <- 0
  parameters$log_sd_log_scalefactor_noise <- 0
  parameters$log_sub_scalefactor_noise <- 0
  parameters$log_offsets <- 0
  parameters$shortterm_noise <- rep(0, N_years)
  parameters$mean_covariate_slopes <- rep(0, N_covariates)
  parameters$phi <- 0.2

  compile("gapfill_API_timeseries_pf_ar.cpp") # Compile the C++ file.
  dyn.load(dynlib("gapfill_API_timeseries_pf_ar")) # Dynamically link the C++ code.
  # Construct an R object (obj) that represents our C++ function.
  # Treat offset term, short-term and long-term noise as random effects.
  obj <- MakeADFun(data,parameters,random=c('log_offsets', 'shortterm_noise'),DLL="gapfill_API_timeseries_pf_ar")

  system.time(
  opt <- nlminb(obj$par,obj$fn,obj$gr,control=list(iter.max=5000,eval.max=5000))
  )

  # Check for significance of covariates:
  #Test significance of covariates:
  rep <- sdreport(obj,getJointPrecision =T)
  cred.int <- cbind(rep$par.fixed + qnorm(0.025)*diag(rep$cov.fixed), rep$par.fixed + qnorm(0.975)*diag(rep$cov.fixed))


  data <- list()
  
  data$N_years <- N_years
  data$API_mean <- API_mean
  data$API_sd <- API_sd
  data$fixed_data <- fixed_data
  data$missing_data <- missing_data
  data$covariates <- covariates
  data$N_covariates <- N_covariates

  # Posterior modes from previous optimisation (in second run: prior = posterior for these; since working with Gaussians, posteriors are Gaussians and mode = mean.).
  # Related to a better prior/starting value?
  # Slight approximation because joint posterior is multivariate Gaussian whereas using univariate normals.
  data$mean_covariate_slopes <- opt$par[names(opt$par)=="mean_covariate_slopes"]
  data$log_sub_scalefactor_noise <- opt$par[names(opt$par)=="log_sub_scalefactor_noise"]
  data$phi <- opt$par[names(opt$par) == "phi"]
  # Now want to focus on the random effects so as to simulate from their joint posterior later. 
  parameters <- list()
  parameters$dummy <- 0
  parameters$shortterm_noise <- matrix(0, nrow = 1, ncol = N_years)
  parameters$log_offsets <- 0
  
  compile("gapfill_API_timeseries_fixedpar_pf_ar.cpp") # Compile the C++ file.
  dyn.load(dynlib("gapfill_API_timeseries_fixedpar_pf_ar"))
  obj2 <- MakeADFun(data,parameters,random=c('log_offsets', 'shortterm_noise'),DLL="gapfill_API_timeseries_fixedpar_pf_ar")
  # So dummy is treated as a fixed parameter (set to 0 in model).
  system.time(
  opt2 <- nlminb(obj2$par,obj2$fn,obj2$gr,control=list(iter.max=500,eval.max=500))
  )
  
  rep2 <- sdreport(obj2,getJointPrecision =T)
  isNotEmpty <- function(x) { return(length(x)!=0) }

  # 1000 x 7527 matrix where each row corresponds to a simulation of offsets, short-term noise and long-term noise.
  set.seed(1)
  r.draws <- rmvn.sparse(1000,c(rep2$par.fixed,rep2$par.random),Cholesky(rep2$jointPrecision),prec=T)
  
  API_timeseries_realisations <- array(NA,dim=c(1,N_years,1000))
  
  for (i in 1:1000) {
    buffer2 <- exp(obj2$report(r.draws[i,])$log_API_timeseries) # Computes API_timeseries matrix based on the simulations and model (Monte Carlo, prior predictive distribution).
    buffer2[fixed_data==1] <- API_mean[fixed_data==1] # Input fixed data.
    if (isNotEmpty(API_mean[missing_data==0][length(which(missing_data==0))])){ # If we have a last API_mean available, i.e. we have data for this country. 
        if (API_mean[missing_data==0][length(which(missing_data==0))]==0 & max(which(missing_data==0))<N_years) { 
          # If the last API_mean available for country j is 0 and we have missing data after consistently, fill the last few with zeros. 
          buffer2[(max(which(missing_data==0))+1):N_years] <- 0
        }
    } 
    else{buffer2 <- 0} # Have no data for this country.  
    API_timeseries_realisations[,,i] <- buffer2 
    cat(i,"\n")
  }
  
  return(API_timeseries_realisations) # 1000 realisations to compute confidence intervals later.
}
  


  