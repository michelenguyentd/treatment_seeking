#include <TMB.hpp> // Links in the TMB libraries
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  // Input Data from R

  DATA_INTEGER(N_years);
  DATA_VECTOR(API_mean); // N_years
  DATA_VECTOR(API_sd); // N_years
  DATA_IVECTOR(fixed_data); // N_years
  DATA_IVECTOR(missing_data); // N_years
  DATA_INTEGER(N_covariates); 
  DATA_ARRAY(covariates);
  // Parameters from R
  
  PARAMETER(mean_log_scalefactor_noise); // Need hyperpriors for these two.
  PARAMETER(log_sd_log_scalefactor_noise);
  PARAMETER(log_sub_scalefactor_noise); 
  PARAMETER(phi);
  PARAMETER_VECTOR(mean_covariate_slopes);
  
  // Random effects
  PARAMETER(log_offsets);
  PARAMETER_VECTOR(shortterm_noise); // N_years -1  

  Type nll = 0; // Declare the "objective function" (neg. log likelihood)
  
  // Priors
  
  nll -= dnorm(log_offsets,Type(0),Type(10),true); 
  nll -= dnorm(mean_log_scalefactor_noise,Type(0.0),Type(1),true);
  nll -= dnorm(log_sd_log_scalefactor_noise,Type(0.0),Type(1),true);
  nll -= dnorm(log_sub_scalefactor_noise,mean_log_scalefactor_noise, exp(log_sd_log_scalefactor_noise),true);
  // AR(1) noise:
  nll -= dbeta(phi, Type(1), Type(1), true);
  nll += SCALE(AR1(phi), exp(log_sub_scalefactor_noise))(shortterm_noise);
  for (int j = 0; j<N_covariates; j++){ 
     nll -= dnorm(mean_covariate_slopes(j), Type(0.0), Type(1), true);
  }
  
 
  // Relate log API time series to AR(1) processes:

  vector<Type> log_API_timeseries(N_years);
   log_API_timeseries = log_offsets + shortterm_noise;
   for (int j=1; j<N_years; j++) {
     for (int k=0; k<N_covariates; k++){
        log_API_timeseries(j) += mean_covariate_slopes(k)*(covariates(j, k, 0) - covariates(j-1, k, 0));
     }
  }

  // National contributions:
  for (int j=0; j<N_years; j++) {
    if (missing_data(j)==0) {
      if (fixed_data(j)==0) {
        nll -= dnorm(exp(log_API_timeseries(j)), API_mean(j), API_sd(j),true);
      }
      if (fixed_data(j)==1) {
        nll -= dnorm(exp(log_API_timeseries(j)), API_mean(j), API_mean(j)*0.001+0.001,true);
      }
    }
  }
  
  return nll;
}
