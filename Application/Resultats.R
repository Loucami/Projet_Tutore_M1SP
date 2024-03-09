
library(glmnet)
library(ncvreg)
library(MASS)
source('Application/Methodes.R')


Resultats1 <- function(xdata, ydata) {
  
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


Resultats2 <- function(xdata, ydata) {
  
  res <- numeric(ncol(xdata))
  N <- nrow(xdata)
  for (i in 1:100) {
    id <- sample(1:N, round(N/2))
    x <- xdata[id,]
    y <- ydata[id]
    covs <- Methodes(xdata,ydata)
    res <- rbind(res, covs)
  }
  res <- res[-1,]
  print(res)
  # Sélection unique de covariables pour chaque méthode
  resultat <- list()
  nb_lignes <- nrow(res)
  for (i in 1:4) {
    i_grp <- seq(i, nb_lignes, by = 4)
    moy_covs <- colMeans(res[i_grp,])
    select_covs <- ifelse(moy_covs > 0.8, 1, 0)
    resultat[[i]] <- select_covs
  }
  resultat <- list(LASSO = resultat[[1]],
                   SCAD = resultat[[2]],
                   MCP = resultat[[3]],
                   STEP = resultat[[4]])
  
  # Résumé du nombre de covariables sélectionnées par chaque méthode, pour notre jeu de donnée
  nb_covs <- list(LASSO = sum(resultat[[1]]!=0),
                  SCAD = sum(resultat[[2]]!=0),
                  MCP = sum(resultat[[3]]!=0),
                  STEP = sum(resultat[[4]]!=0))
  
  which_covs <- list(LASSO = which(resultat[[1]]!=0),
                     SCAD = which(resultat[[2]]!=0),
                     MCP = which(resultat[[3]]!=0),
                     STEP = which(resultat[[4]]!=0))
  
  resultats <- list(resultats_tot = resultat, 
                    resultats_nb = nb_covs, 
                    resultats_covs = which_covs)
  
  return(resultats)
}
