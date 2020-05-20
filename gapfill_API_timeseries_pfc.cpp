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
 // DATA_INTEGER(N_covariates);
  DATA_MATRIX(API_mean); // N_countries x N_years
  DATA_MATRIX(API_sd); // N_countries x N_years
  DATA_IMATRIX(fixed_data); // N_countries x N_years
  DATA_IMATRIX(missing_data); // N_countries x N_years
  DATA_ARRAY(covariates); // N_years x N_covariates x N_countries
  
  // Parameters from R
  
 // PARAMETER_VECTOR(mean_covariate_slopes); // N_covariates
  PARAMETER(mean_log_scalefactor_noise); // Need hyperpriors for these two.
  PARAMETER(log_sd_log_scalefactor_noise);
  PARAMETER_VECTOR(log_national_scalefactor_noise); // N_countries

  // Random effects
  PARAMETER_VECTOR(national_log_initial_API_offsets); // N_countries
  PARAMETER_MATRIX(national_shortterm_noise); // N_countries x (N_years-1)

  Type nll = 0; // Declare the "objective function" (neg. log likelihood)
  
  // Priors
  
 for (int i=0; i<N_countries; i++) {nll -= dnorm(national_log_initial_API_offsets[i],Type(-1),Type(1),true);} // Use R-style call to normal density
  //for (int i=0; i<N_covariates; i++) {nll -= dnorm(mean_covariate_slopes[i],Type(1),Type(1),true);}
  nll -= dnorm(mean_log_scalefactor_noise,Type(-4),Type(1),true);
  nll -= dnorm(log_sd_log_scalefactor_noise,Type(-4),Type(1),true);

  
  for (int i=0; i<N_countries; i++) {nll -= dnorm(log_national_scalefactor_noise[i],mean_log_scalefactor_noise, exp(log_sd_log_scalefactor_noise),true);}

  for (int i=0; i<N_countries; i++) {
    for (int j=0; j<(N_years-1); j++) {
      nll -= dnorm(national_shortterm_noise(i,j),Type(0.0),exp(log_national_scalefactor_noise[i]),true);
    }
  }
  

  // Transforms
  
  Type buffer;
  matrix<Type> national_shortterm_noise_smoothed(N_countries,N_years);
  for (int i=0; i<N_countries; i++) {
    for (int j=0; j<(N_years-1); j++) {
      national_shortterm_noise_smoothed(i,j) = 0.0;
      buffer = 0.0;
      for (int k=0; k<(N_years-1); k++) {
        national_shortterm_noise_smoothed(i,j) += national_shortterm_noise(i,k)*exp(-(j-k)*(j-k)/(1.5*1.5)); // Smoothing by a Gaussian kernel.
        buffer += exp(-(j-k)*(j-k)/(1.5*1.5));
      }
      national_shortterm_noise_smoothed(i,j) = national_shortterm_noise_smoothed(i,j)/buffer;
    }
  }
  
  matrix<Type> API_timeseries(N_countries,N_years);
  for (int i=0; i<N_countries; i++) {
   API_timeseries(i,0) = exp(national_log_initial_API_offsets[i]);
    for (int j=1; j<N_years; j++) {
      API_timeseries(i,j) = API_timeseries(i,j-1) + (covariates(j,0,i)-covariates(j-1,0,i)) + national_shortterm_noise_smoothed(i,j);
    }
  }
    
  // Likelihood
  
  for (int i=0; i<N_countries; i++) {
    for (int j=0; j<N_years; j++) {
      if (missing_data(i,j)==0) {
        if (fixed_data(i,j)==0) {
          nll -= dnorm(API_timeseries(i,j),API_mean(i,j),API_sd(i,j),true);
        }
        if (fixed_data(i,j)==1) {
          nll -= dnorm(API_timeseries(i,j),API_mean(i,j),API_mean(i,j)*0.001+0.001,true);
        }
      }
    }
  }
  
  REPORT(API_timeseries); // what will be reported when the MakeADFun is called with inputs and $report later.
  return nll;
}
