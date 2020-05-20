### R function (running TMB) to perform TS gap-filling and timeseries inference procedure

## Supposes that gold and silver classification data are identifiable as those with zero width input error estimates (i.e., those i,j such that Any_low[i,j] = Any_mean[i,j] = Any_high[i,j])
## Supposes missing data indicated by NAs
## Supposes lower and upper bounds are approx equal and have been estimated as approx 95% confidence intervals

## Requires:
# Any_mean = an N_countries (rows) x N_years (columns) matrix giving estimated mean Any TS. 
# Any_low = an N_countries (rows) x N_years (columns) matrix giving estimated lower bound on Any TS.
# Any_high = an N_countries (rows) x N_years (columns) matrix giving estimated upper bound on Any TS.
# Covariates = an N_years x N_covariates x N_countries array giving covariates for modelling

# For testing function:
Any_mean <- ESSA.Any; Any_low <- ESSA.Any.low ; Any_high <- ESSA.Any.high; covariates <- ESSA.covariates

# gapfill_Any_timeseries_rt <- function(Any_mean,Any_low,Any_high,covariates) {
  
  # Catch basic input errors
  if (!(dim(Any_mean)==dim(Any_low) && dim(Any_mean)==dim(Any_high))) {stop("Any_mean,Any_low,Any_high matrices not equal sizes.")}
  if (!prod(dim(Any_mean)==dim(covariates)[c(3,1)])) {stop("Covariate array improper size given Any inputs.")}
  if (!prod(!is.na(covariates))) {stop("Covariate array contains NAs.")}
 
  # Prepare TMB input data list
  N_countries <- dim(Any_mean)[1]
  N_years <- dim(Any_mean)[2]
  N_covariates <- dim(covariates)[2]

  missing_data <- matrix(0,nrow=N_countries,ncol=N_years)
  missing_data[is.na(Any_mean)] <- 1
  
  fixed_data <- matrix(0,nrow=N_countries,ncol=N_years)
  fixed_data[Any_high==Any_low] <- 1
  
  Any_mean[is.na(Any_mean)] <- -99
  Any_sd <- matrix(-99,nrow=N_countries,ncol=N_years)
  Any_sd[missing_data==0 & fixed_data==0] <- ((Any_high-Any_mean)/2+(Any_mean-Any_low)/2)[missing_data==0 & fixed_data==0]/diff(qnorm(c(0.025,0.9755)))
  Any_sd[Any_sd<0.001 & Any_sd > -99] <- 0.0001
  
  data <- list()
  
  data$N_countries <- N_countries
  data$N_years <- N_years
  data$N_covariates <- N_covariates
  data$Any_mean <- Any_mean
  data$Any_sd <- Any_sd
  data$fixed_data <- fixed_data
  data$missing_data <- missing_data
  data$covariates <- covariates
  
  parameters <- list()
  
  parameters$mean_covariate_slopes <- rep(0,N_covariates)
  parameters$mean_log_scalefactor_noise <- 0
  parameters$log_sd_log_scalefactor_noise <- 0
  parameters$log_regional_scalefactor_noise <- 0 # Common for region. 
  parameters$log_national_scalefactor_noise <- rep(0, N_countries) # For individual countries. 

  parameters$national_initial_Any_offsets <- rep(0,N_countries)
  parameters$national_shortterm_noise <- matrix(0, N_countries, N_years-1)
  parameters$national_longterm_noise <- rep(0, N_years-1)

  compile("Code/gapfill_TS_rt.cpp") # Compile the C++ file.
  dyn.load(dynlib("Code/gapfill_TS_rt")) # Dynamically link the C++ code.
  # Construct an R object (obj) that represents our C++ function.
  # Treat offset term, short-term and long-term noise as random effects.
  obj <- MakeADFun(data,parameters,random=c('national_initial_Any_offsets','national_shortterm_noise','national_longterm_noise'),DLL="gapfill_TS_rt")
  
  opt <- nlminb(obj$par,obj$fn,obj$gr,control=list(iter.max=5000,eval.max=5000))
  #Test significance of covariates:
  rep <- sdreport(obj,getJointPrecision =T)
  cred.int <- cbind(rep$par.fixed + qnorm(0.025)*diag(rep$cov.fixed), rep$par.fixed + qnorm(0.975)*diag(rep$cov.fixed)) # Both covariates significant.

  data <- list()
  
  data$N_countries <- N_countries
  data$N_years <- N_years
  data$N_covariates <- N_covariates
  data$Any_mean <- Any_mean
  data$Any_sd <- Any_sd
  data$fixed_data <- fixed_data
  data$missing_data <- missing_data
  data$covariates <- covariates
  
  # Posterior modes from previous optimisation (in second run: prior = posterior for these; since working with Gaussians, posteriors are Gaussians and mode = mean.).
  # Related to a better prior/starting value?
  # Slight approximation because joint posterior is multivariate Gaussian whereas using univariate normals.
  data$log_national_scalefactor_noise <- opt$par[names(opt$par)=="log_national_scalefactor_noise"]
  data$log_regional_scalefactor_noise <- opt$par[names(opt$par)=="log_regional_scalefactor_noise"]
  data$mean_covariate_slopes <- opt$par[names(opt$par)=="mean_covariate_slopes"]

  # Now want to focus on the random effects so as to simulate from their joint posterior later. 
  
  parameters <- list()
  
  parameters$dummy <- 0
  parameters$national_initial_Any_offsets <- rep(0,N_countries)
  parameters$national_shortterm_noise <-matrix(0, N_countries, N_years-1)
  parameters$national_longterm_noise <- rep(0, N_years-1)
  
  compile("Code/gapfill_TS_fixedpar_rt2.cpp") # Compile the C++ file.
  dyn.load(dynlib("Code/gapfill_TS_fixedpar_rt2")) 
  
  obj2 <- MakeADFun(data,parameters,random=c('national_initial_Any_offsets','national_shortterm_noise','national_longterm_noise'),DLL="gapfill_TS_fixedpar_rt2")
  
  # So dummy is treated as a fixed parameter (set to 0 in model).
  opt2 <- nlminb(obj2$par,obj2$fn,obj2$gr,control=list(iter.max=500,eval.max=500))
  rep <- sdreport(obj2,getJointPrecision =T)

  isNotEmpty <- function(x) { return(length(x)!=0) }

  # 1000 x 7527 matrix where each row corresponds to a simulation of offsets, short-term noise and long-term noise.
  set.seed(1)
  r.draws <- rmvn.sparse(1000,c(rep$par.fixed,rep$par.random),Cholesky(rep$jointPrecision),prec=T)
  Any_timeseries_realisations <- array(NA,dim=c(N_countries,N_years,1000))
  for (i in 1:1000) {
    buffer <- inv.logit(obj2$report(r.draws[i,])$logit_Any_timeseries) # Computes Any_timeseries matrix based on the simulations and model (Monte Carlo, prior predictive distribution).
    buffer[fixed_data==1] <- Any_mean[fixed_data==1] # Input fixed data.
    for (j in 1:N_countries) {
	  if (isNotEmpty(Any_mean[j,missing_data[j,]==0][length(which(missing_data[j,]==0))])){ # If we have a last Any_mean available, i.e. we have data for this country. 
        if (Any_mean[j,missing_data[j,]==0][length(which(missing_data[j,]==0))]==0 & max(which(missing_data[j,]==0))<N_years) { 
          # If the last Any_mean available for country j is 0 and we have missing data after consistently, fill the last few with zeros. 
        buffer[j,(max(which(missing_data[j,]==0))+1):N_years] <- 0
        }
	    }
	   }
      #{else buffer[j,] <- 0 # Have no data for this country.}
    Any_timeseries_realisations[,,i] <- buffer 
    cat(i,"\n")
  
  }
return(Any_timeseries_realisations) # 1000 realisations to compute confidence intervals later.
}