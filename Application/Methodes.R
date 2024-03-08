
library(glmnet)
library(ncvreg)
library(MASS)


### ------------------------  Méthodes de sélection  ----------------------- ###


Methodes <- function(xdata, ydata, folds = 5) {
  
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
  model.sat <- lm(ydata ~ ., data = l_data)
  model.cst <- lm(ydata ~ 1, data = l_data)
  model_step = stepAIC(model.cst,  scope = list(lower = model.cst, upper = model.sat), trace = FALSE)
  
  ## Coefficients sélectionnées
  cols <- names(l_data[-1])
  cols_select <- which(coef(model_step)[-1] != 0)
  covs_step <- numeric(length(cols))
  covs_step[cols_select] <- 1
  
  return(rbind(covs_lasso, covs_scad, covs_mcp, covs_step))
}
