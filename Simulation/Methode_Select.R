

library(glmnet)
library(ncvreg)
library(MASS)


### ------------------------  Méthodes de sélection  ----------------------- ###


methode_select <- function(xdata, ydata, folds) {
  
  ### ------ LASSO ------ ###
  
  ## Normalisation de X
  xdata_scale <- scale(xdata)
  
  ## Modèle d'origine et plot
  # fit <- glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  # plot(fit)
  
  ## Ajustement du modèle LASSO avec validation croisée
  cvfit_lasso <- cv.glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  fit <- glmnet(xdata_scale, ydata, alpha = 1, lambda = cvfit_lasso$lambda.min) 
  # plot(cvfit_lasso)
  
  ## Coefficients sélectionnées
  ß_lasso <- coef(fit, s = cvfit_lasso$lambda.min)
  covs_lasso <- ifelse(ß_lasso[-1]!=is.na(ß_lasso[-1]) & ß_lasso[-1]!=0, 1, 0)
  
  ### ------ SCAD ------ ###
  
  ## Modèle d'origine et plot
  # fit <- ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  # plot(fit)
  
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
  f.sat <- as.formula('ydata ~ .')
  model.sat <- lm(f.sat, data = l_data)
  f.cst <- as.formula('ydata ~ 1')
  model.cst <- lm(f.cst, data = l_data)
  model_step = stepAIC(model.cst,  scope = list(upper = model.sat, lower = model.cst), trace = FALSE)
  
  ## Coefficients sélectionnées
  ß_step <- coef(model_step)
  cols_select <- gsub("`", "", names(ß_step[-1]))
  cols <- names(l_data[-1])
  covs_step <- rep(0, length(cols))
  covs_step[match(cols_select, cols)] <- ß_step[-1]
  covs_step <- ifelse(covs_step!=is.na(covs_step) & covs_step!=0, 1, 0)
  
  return(rbind(covs_lasso, covs_scad, covs_mcp, covs_step))
  #return(list(lasso = covs_lasso, scad = covs_scad, mcp = covs_mcp, stepAIC = covs_step))
}
