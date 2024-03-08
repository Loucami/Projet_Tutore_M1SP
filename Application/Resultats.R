
library(glmnet)
library(ncvreg)
library(MASS)
source('Application/Methodes.R')


Resultats <- function(xdata, ydata) {
  
  res <- Methodes(xdata, ydata)
  
  nb_covs <- list(LASSO = sum(res[1,]),
                  SCAD = sum(res[2,]),
                  MCP = sum(res[3,]),
                  STEP = sum(res[4,]))
  
  which_covs <- list(LASSO = which(res[1,]!=0),
                     SCAD = which(res[2,]!=0),
                     MCP = which(res[3,]!=0),
                     STEP = which(res[4,]!=0))
    
  resultats <- list(resultats_tot = res, 
                    resultats_nb = nb_covs, 
                    resultats_covs = which_covs)
  
  return(resultats)
}
