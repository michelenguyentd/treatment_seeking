#include <TMB.hpp> // Links in the TMB libraries
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  // Input Data from R
  
  DATA_INTEGER(N_countries);
  DATA_INTEGER(N_years);
  DATA_INTEGER(N_covariates);
  DATA_MATRIX(Any_mean); // N_countries x N_years
  DATA_MATRIX(Any_sd); // N_countries x N_years
  DATA_IMATRIX(fixed_data); // N_countries x N_years
  DATA_IMATRIX(missing_data); // N_countries x N_years
  DATA_ARRAY(covariates); // N_years x N_covariates x N_countries
  
  // Parameters from R
  
  DATA_VECTOR(mean_covariate_slopes); // N_covariates
  DATA_VECTOR(log_national_scalefactor_noise);
  DATA_SCALAR(log_regional_scalefactor_noise)
  
  // Random effects
  PARAMETER(dummy); //
  PARAMETER_VECTOR(national_initial_Any_offsets); // N_countries
  PARAMETER_MATRIX(national_shortterm_noise); // N_countries x N_years-1
  PARAMETER_VECTOR(national_longterm_noise); // N_years-1
  
  Type nll = 0; // Declare the "objective function" (neg. log likelihood)
  
  // Priors
  nll -= dnorm(dummy,Type(0),Type(1),true); // Use R-style call to normal density
  
  for (int i=0; i<N_countries; i++) {nll -= dnorm(national_initial_Any_offsets[i],Type(0),Type(1),true);} // Use R-style call to normal density
 
   for (int i=0; i<N_countries; i++){
    for (int j=0; j<(N_years-1); j++) {
    nll -= dnorm(national_shortterm_noise(i, j),Type(0.0),exp(log_national_scalefactor_noise[i]),true);
    }
  }

    for (int j=0; j<(N_years-1); j++) {
      nll -= dnorm(national_longterm_noise(j),Type(0.0), exp(log_regional_scalefactor_noise), true);
    }

  // Transforms
  
  Type buffer;
  matrix<Type> national_shortterm_noise_smoothed(N_countries, N_years);
  for (int i=0; i<N_countries; i++){
    for (int j=0; j<(N_years-1); j++) {
      national_shortterm_noise_smoothed(i, j) = 0.0;
      buffer = 0.0;
      for (int k=0; k<(N_years-1); k++) {
        national_shortterm_noise_smoothed(i, j) += national_shortterm_noise(i, k)*exp(-(j-k)*(j-k)/(0.5*0.5)); // Smoothing by a Gaussian kernel.
        buffer += exp(-(j-k)*(j-k)/(0.5*0.5));
      }
      national_shortterm_noise_smoothed(i, j) = national_shortterm_noise_smoothed(i, j)/buffer;
    }
  }
  
 vector<Type> national_longterm_noise_smoothed(N_years);
 for (int j=0; j<(N_years-1); j++) {
  national_longterm_noise_smoothed(j) = 0.0;
    buffer = 0.0;
    for (int k=0; k<(N_years-1); k++) {
     national_longterm_noise_smoothed(j) += national_longterm_noise(k)*exp(-(j-k)*(j-k)/(2.5*2.5)); // Smoothing by a flatter Gaussian kernel.
     buffer += exp(-(j-k)*(j-k)/(2.5*2.5));
   }
  national_longterm_noise_smoothed(j) = national_longterm_noise_smoothed(j)/buffer;
 }
  
  matrix<Type> logit_Any_timeseries(N_countries,N_years);
  for (int i=0; i<N_countries; i++) {
    logit_Any_timeseries(i,0) = national_initial_Any_offsets[i];
    for (int j=1; j<N_years; j++) {
      logit_Any_timeseries(i,j) = logit_Any_timeseries(i,j-1) + national_longterm_noise_smoothed(j-1);
        for (int k=0; k<N_covariates; k++) {logit_Any_timeseries(i,j) += mean_covariate_slopes(k)*(covariates(j,k,i)-covariates(j-1,k,i));}
    }
    for (int j=1; j<N_years; j++) {
      logit_Any_timeseries(i,j) += national_shortterm_noise_smoothed(i, j-1);
    }
  }
    
  // Likelihood
  
  for (int i=0; i<N_countries; i++) {
    for (int j=0; j<N_years; j++) {
      if (missing_data(i,j)==0) {
        if (fixed_data(i,j)==0) {
          nll -= dnorm(exp(logit_Any_timeseries(i,j))/(1 + exp(logit_Any_timeseries(i,j))),Any_mean(i,j),Any_sd(i,j),true);
        }
        if (fixed_data(i,j)==1) {
          nll -= dnorm(exp(logit_Any_timeseries(i,j))/(1 + exp(logit_Any_timeseries(i,j))),Any_mean(i,j),Any_mean(i,j)*0.0001+0.0001,true);
        }
      }
    }
  }
  
  REPORT(logit_Any_timeseries); // what will be reported when the MakeADFun is called with inputs and $report later.
  return nll;
}
