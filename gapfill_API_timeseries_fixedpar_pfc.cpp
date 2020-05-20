#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  // Input Data
  
  DATA_INTEGER(N_countries);
  DATA_INTEGER(N_years);
  // DATA_INTEGER(N_covariates);
  DATA_MATRIX(API_mean); // N_countries x N_years
  DATA_MATRIX(API_sd); // N_countries x N_years
  DATA_IMATRIX(fixed_data); // N_countries x N_years
  DATA_IMATRIX(missing_data); // N_countries x N_years
  DATA_ARRAY(covariates); // N_years x N_covariates x N_countries
  
  // Previously parameters, now fixed

  // DATA_VECTOR(mean_covariate_slopes); // N_covariates
  DATA_VECTOR(log_national_scalefactor_noise); // N_countries

  // Parameters (previously random effects)
  
  PARAMETER(dummy);
  PARAMETER_VECTOR(national_log_initial_API_offsets); // N_countries
  PARAMETER_MATRIX(national_shortterm_noise); // N_countries x (N_years-1)

  Type nll = 0;
  
  // Priors (same distributions as before)
  
  nll -= dnorm(dummy,Type(0),Type(1),true);
  for (int i=0; i<N_countries; i++) {nll -= dnorm(national_log_initial_API_offsets[i],Type(0),Type(10),true);}

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
