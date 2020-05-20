library(plyr)
# these next two libraries are required for running copies of the function (one per survey) in parallel
# on windows - there are other choices than doParallel esp on linux 
library(foreach)
library(doParallel)

# function to create 95% confidence intervals for the national proportions based on the raw 
# child level had-treatment data. Input data should contain only children who had fever.
createCIs <- function(dfInput){
  library(survey)
  # create GLMs for the relationship between the response column and the predictor "1"
  pubmod <- glm(public_treat~1, data=dfInput, family=quasibinomial(), weights=young_child_wt)
  anymod <- glm(any_treat~1, data=dfInput, family=quasibinomial(), weights=young_child_wt)
  
  dfOut <- tryCatch({
    meanpub <- plogis(coef(pubmod))
    confpub <- plogis(confint(pubmod))
    meanany <- plogis(coef(anymod))
    confany <- plogis(confint(anymod))
    
    # create a survey design object to use the survey package for estimating CIs
    SVY <- svydesign(id=~1, weights=~young_child_wt, data=dfInput)
    coefpub <- svymean(~public_treat, SVY, deff=FALSE, na.rm=TRUE)
    coefany <- svymean(~any_treat, SVY, deff=FALSE, na.rm=TRUE)
    svyconfpub <- confint(coefpub)
    svyconfany <- confint(coefany)
    
    data.frame(glm_mean_public=meanpub, glm_lci_public=confpub["2.5 %"], glm_uci_public=confpub["97.5 %"], 
               glm_mean_any=meanany, glm_lci_any=confany["2.5 %"], glm_uci_any=confany["97.5 %"],
               svy_lci_public=svyconfpub[1], svy_uci_public=svyconfpub[2],
               svy_lci_any=svyconfany[1], svy_uci_any=svyconfany[2])
  },
  error=function(cond){
    return (data.frame(glm_mean_public=-9999, glm_lci_public=-9999, glm_uci_public=-9999, 
                       glm_mean_any=-9999, glm_lci_any=-9999, glm_uci_any=-9999))
  },
  warning=function(cond){return(NULL)}
  )
  return (dfOut)
}

# how many cores to use - my machine has 12, adjust to suit
workers <- makeCluster(10)
registerDoParallel(workers)

# output a dataframe containing one row (=fme feature) for each input survey, by calling the 
# createCIs function for each survey's data in turn (like GROUP BY)
# Use parallel functionality to ensure it runs in a decent amount of time. On a single core 
# with all 200+ surveys it takes 10 minutes+ and this times out in FME making the translation fail.
fmeOutput <- ddply(childraw, "surveyid", createCIs, .parallel = TRUE)