
library(glmnet)
library(ncvreg)
library(MASS)


### ------------------------  Méthodes de sélection  ----------------------- ###


Methodes1 <- function(xdata, ydata, folds = 5) {
  
  ### ------ LASSO ------ ###
  
  ## Normalisation de X
  xdata_scale <- scale(xdata)
  
  ## Ajustement du modèle LASSO avec validation croisée
  cvfit_lasso <- cv.glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  fit <- glmnet(xdata_scale, ydata, alpha = 1, lambda = cvfit_lasso$lambda.min) 
  # plot(cvfit_lasso)
  
  ## Coefficients sélectionnées
  ß_lasso <- coef(fit, s = cvfit_lasso$lambda.min)
  covs_lasso <- ifelse(ß_lasso[-1]!=is.na(ß_lasso[-1]) & ß_lasso[-1]!=0, 1, 0)
  
  ### ------ SCAD ------ ###
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit_scad <- cv.ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  # plot(cvfit_scad)
  
  # Coefficients sélectionnées
  ß_scad <- coef(cvfit_scad)
  covs_scad <- ifelse(ß_scad[-1]!=is.na(ß_scad[-1]) & ß_scad[-1]!=0, 1, 0)
  
  ### ------  MCP  ------ ###
  
  ## Modèle d'origine et plot
  # fit <- ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)
  # plot(fit)
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit_mcp <- cv.ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)
  # plot(cvfit_mcp)
  
  ## Coefficients sélectionnées
  ß_mcp <- coef(cvfit_mcp)
  covs_mcp <- ifelse(ß_mcp[-1]!=is.na(ß_mcp[-1]) & ß_mcp[-1]!=0, 1, 0)
  
  ### ------  STEPAIC  ------ ###
  
  ## Ajustement du modèle SCAD avec validation croisée
  l_data <- cbind.data.frame(ydata, xdata)
  model.sat <- lm(ydata ~ ., data = l_data)
  model.cst <- lm(ydata ~ 1, data = l_data)
  model_step = stepAIC(model.cst,  scope = list(lower = model.cst, upper = model.sat), trace = FALSE)
  
  ## Coefficients sélectionnées
  cols <- names(l_data[-1])
  cols_select <- which(coef(model_step)[-1] != 0)
  covs_step <- numeric(length(cols))
  covs_step[cols_select] <- 1
  
  return(rbind(covs_step, covs_lasso, covs_scad, covs_mcp))
}


Methodes2 <- function(xdata, ydata, folds = 5) {
  
  xdata_scale <- scale(xdata)
  
  # On récupère d'abord un unique lambda.min pour LASSO, SCAD et MCP
  lambda_lasso <- cv.glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)$lambda.min
  lambda_scad <- cv.ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)$lambda.min
  lambda_mcp <- cv.ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)$lambda.min
  
  res <- numeric(ncol(xdata))
  N <- nrow(xdata)
  for (i in 1:100) {
    id <- sample(1:N, round(N/2))
    x <- xdata[id,]
    y <- ydata[id]
    
    # LASSO
    x_scale <- scale(x)
    fit <- glmnet(xdata_scale, ydata, alpha = 1, lambda = lambda_lasso) 
    ß_lasso <- coef(fit, s = lambda_lasso)
    covs_lasso <- ifelse(ß_lasso[-1]!=is.na(ß_lasso[-1]) & ß_lasso[-1]!=0, 1, 0)
    
    # SCAD
    cvfit_scad <- ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds, lambda = lambda_scad )
    ß_scad <- coef(cvfit_scad)
    covs_scad <- ifelse(ß_scad[-1]!=is.na(ß_scad[-1]) & ß_scad[-1]!=0, 1, 0)
    
    # MCP
    cvfit_mcp <- ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds, lambda = lambda_mcp)
    ß_mcp <- coef(cvfit_mcp)
    covs_mcp <- ifelse(ß_mcp[-1]!=is.na(ß_mcp[-1]) & ß_mcp[-1]!=0, 1, 0)
    
    # STEP
    l_data <- cbind.data.frame(ydata, xdata)
    model.sat <- lm(ydata ~ ., data = l_data)
    model.cst <- lm(ydata ~ 1, data = l_data)
    model_step = stepAIC(model.cst,  scope = list(lower = model.cst, upper = model.sat), trace = FALSE)
    
    cols <- names(l_data[-1])
    cols_select <- which(coef(model_step)[-1] != 0)
    covs_step <- numeric(length(cols))
    covs_step[cols_select] <- 1
    
    covs <- rbind(covs_step, covs_lasso, covs_scad, covs_mcp)
    res <- rbind(res, covs)
  }
  res <- res[-1,]

  # Sélection unique de covariables pour chaque méthode
  resultat <- list()
  nb_lignes <- nrow(res)
  for (i in 1:4) {
    i_grp <- seq(i, nb_lignes, by = 4)
    moy_covs <- colMeans(res[i_grp,])
    select_covs <- ifelse(moy_covs > 0.8, 1, 0)
    resultat[[i]] <- select_covs
  }
  
  resultat <- list(STEPAIC = resultat[[1]],
                   LASSO = resultat[[2]],
                   SCAD = resultat[[3]],
                   MCP = resultat[[4]])
  
  nb_covs <- list(STEPAIC = sum(resultat[[1]]!=0),
                  LASSO = sum(resultat[[2]]!=0),
                  SCAD = sum(resultat[[3]]!=0),
                  MCP = sum(resultat[[4]]!=0))
  
  which_covs <- list(STEPAIC = which(resultat[[1]]!=0),
                     LASSO = which(resultat[[2]]!=0),
                     SCAD = which(resultat[[3]]!=0),
                     MCP = which(resultat[[4]]!=0))
  
  resultats <- list(resultats_tot = resultat, 
                    resultats_nb = nb_covs, 
                    resultats_covs = which_covs)
  
  return(resultats)
}

