


### ------------------------  Méthodes de sélection  ----------------------- ###


Methodes <- function(xdata, ydata, folds = 5) {
  
  xdata_scale <- scale(xdata)
  
  # On récupère d'abord un unique lambda.min pour LASSO, SCAD et MCP
  lambda_lasso <- cv.glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)$lambda.min
  lambda_scad <- cv.ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)$lambda.min
  lambda_mcp <- cv.ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)$lambda.min
  
  # Stability selection sur LASSO, SCAD et MCP
  res <- numeric(ncol(xdata))
  N <- nrow(xdata)
  res = foreach(i = 1:1000,.combine = rbind,.packages=c("glmnet","glmnetUtils","ncvreg")) %dopar%{
    id <- sample(1:N, round(N/2))
    x_scale <- xdata_scale[id,]
    x <- xdata[id,]
    y <- ydata[id]
    
    # LASSO
    fit <- glmnet(x_scale, y, alpha = 1, lambda = lambda_lasso)
    ß_lasso <- coef(fit, s = lambda_lasso)
    covs_lasso <- ifelse(ß_lasso[-1]!=is.na(ß_lasso[-1]) & ß_lasso[-1]!=0, 1, 0)
    
    # SCAD
    cvfit_scad <- ncvreg(x, y, penalty = 'SCAD', nfolds = folds, lambda = lambda_scad )
    ß_scad <- coef(cvfit_scad)
    covs_scad <- ifelse(ß_scad[-1]!=is.na(ß_scad[-1]) & ß_scad[-1]!=0, 1, 0)
    
    # MCP
    cvfit_mcp <- ncvreg(x, y, penalty = 'MCP', nfolds = folds, lambda = lambda_mcp)
    ß_mcp <- coef(cvfit_mcp)
    covs_mcp <- ifelse(ß_mcp[-1]!=is.na(ß_mcp[-1]) & ß_mcp[-1]!=0, 1, 0)
    
    covs <- rbind(covs_lasso, covs_scad, covs_mcp)
    covs
  }

  # Sélection unique de covariables pour LASSO, SCAD, MCP
  res_methodes <- list()
  nb_lignes <- nrow(res)
  for (i in 1:3) {
    i_grp <- seq(i, nb_lignes, by = 3)
    moy_covs <- colMeans(res[i_grp,])
    select_covs <- ifelse(moy_covs > 0.8, 1, 0)
    res_methodes[[i]] <- select_covs
  }
  
  resultat <- rbind(res_methodes[[1]], 
                     res_methodes[[2]], 
                     res_methodes[[3]])
  
  nb_covs <- list(LASSO = sum(resultat[1,]),
                  SCAD = sum(resultat[2,]),
                  MCP = sum(resultat[3,]))
  
  which_covs <- list(LASSO = which(resultat[1,]!=0),
                     SCAD = which(resultat[2,]!=0),
                     MCP = which(resultat[3,]!=0))
  
  resultats <- list(resultats_nb = nb_covs, 
                    resultats_covs = which_covs)
  
  return(resultats)
}

