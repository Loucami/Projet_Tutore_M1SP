
library(glmnet)
library(ncvreg)
library(MASS)


### ------------------------  Méthodes d'évaluation  ----------------------- ###


eval_methode_auto <- function(xdata, ydata, folds) {
  
  info <- paste("Nombre d'enregistrements :", nrow(xdata), "; Nombre de covariables :", ncol(xdata))
  print(info)
  
  ### ------ LASSO ------ ###
  
  print('--------  LASSO  --------')
  
  ## Normalisation de X
  xdata_scale <- scale(xdata)
  
  ## Modèle d'origine et plot
  fit <- glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  plot(fit)
  
  ## Ajustement du modèle LASSO avec validation croisée
  cvfit_lasso <- cv.glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  fit <- glmnet(xdata_scale, ydata, alpha = 1, lambda = cvfit_lasso$lambda.min) 
  plot(cvfit_lasso)
  
  ## Prédiction des résultats
  y_pred_lasso <- predict(fit, newx = xdata_scale, s = cvfit_lasso$lambda.min) 
  ß_pred_lasso <- coef(fit, s = cvfit_lasso$lambda.min)
  
  ## Affichage des résultats
  # Pénalité 
  penalty_lasso <- paste('Pénalité :', round(cvfit_lasso$lambda.min,4))
  print(penalty_lasso)
  
  # Nombre de variables sélectionnées 
  nb_vars_lasso <- sum(ifelse(ß_pred_lasso[-1][ß_pred_lasso[-1]!=0 & ß_pred_lasso[-1]!=is.na(ß_pred_lasso[-1])], 1, 0))  
  number_vars_lasso <- paste('Nombre de covariable sélectionnées :', nb_vars_lasso)
  print(number_vars_lasso)
  
  # Variables sélectionnées 
  vars_lasso <- which(ß_pred_lasso[-1]!=0 & ß_pred_lasso[-1]!=is.na(ß_pred_lasso[-1])) 
  liste_vars_lasso <- paste('Liste des covariables sélectionnées :', paste(vars_lasso, collapse = ", "))
  print(liste_vars_lasso)
  
  ### ------ SCAD ------ ###
  
  print('--------  SCAD  --------')
  
  ## Modèle d'origine et plot
  fit <- ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  plot(fit)
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit_scad <- cv.ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  plot(cvfit_scad)
  
  ## Affichage des résultats
  # Pénalité 
  penalty_scad <- paste('Pénalité :', round(cvfit_scad$lambda.min,4))
  print(penalty_scad)
  
  # Nombre de variables sélectionnées
  nb_vars_scad <- predict(cvfit_scad, type = "nvars")
  number_vars_scad <- paste('Nombre de covariable sélectionnées : ', nb_vars_scad)
  print(number_vars_scad)
  
  # Variables sélectionnées
  vars_scad <- predict(cvfit_scad, type = "vars")
  list_vars_scad <- paste("Variables séletcionnées :", paste(vars_scad, collapse = ", "))
  print(list_vars_scad)
  
  ### ------  MCP  ------ ###
  
  print('--------  MCP  --------')
  
  ## Modèle d'origine et plot
  fit <- ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)
  plot(fit)
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit_mcp <- cv.ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)
  plot(cvfit_mcp)
  
  ## Affichage des résultats
  # Pénalité 
  penalty_mcp <- paste('Pénalité :', round(cvfit_mcp$lambda.min,4))
  print(penalty_mcp)
  
  # Nombre de variables sélectionnées
  nb_vars_mcp <- predict(cvfit_mcp, type = "nvars")
  number_vars_mcp <- paste('Nombre de covariable sélectionnées : ', nb_vars_mcp)
  print(number_vars_mcp)
  
  # Variables sélectionnées
  vars_mcp <- predict(cvfit_mcp, type = "vars")
  list_vars_mcp <- paste("Variables séletcionnées :", paste(vars_mcp, collapse = ", "))
  #print(list_vars_mcp)
  
  # ### ------  STEPAIC  ------ ###
  # 
  # print('--------  STEPAIC  --------')
  # 
  # ## Ajustement du modèle SCAD avec validation croisée
  # data_stepAIC <- as.data.frame(cbind(xdata, ydata))
  # last_col <- colnames(data_stepAIC[ncol(data_stepAIC)])
  # modele <- lm(paste(last_col, " ~ .", sep = ""), data = data_stepAIC)
  # invisible(capture.output(fit_aic <- stepAIC(modele, direction = "both")))
  # 
  # ## Prédiction des coefficients
  # y_pred_aic <- predict(fit_aic) 
  # ß_pred_aic <- coef(fit_aic)
  # 
  # ## Affichage des résultats
  # # Nombre de variables sélectionnées 
  # nb_vars_aic <- length(ß_pred_aic[-1])
  # number_vars_aic <- paste('Nombre de covariable sélectionnées :', nb_vars_aic)
  # print(number_vars_aic)
  # 
  # # Variables sélectionnées 
  # vars_aic <- which(ß_pred_aic[-1]!=0)
  # liste_vars_aic <- paste('Liste des covariables sélectionnées :', paste(vars_aic, collapse = ", "))
  # print(liste_vars_aic)
  
}


### -----------------------------  Simulation  ----------------------------- ###


simulation <- function(N,n,P) {
  
  # On crée X
  mu <- rnorm(n)
  sigma2_X <- rexp(n, 1)
  X <- matrix(data = NA, nrow = N, ncol = n)
  for (i in 1:n) { X[,i] <- rnorm(N, mu[i], sigma2_X[i]) }
  
  # On crée ß
  ß_P <- rnorm(P,0,1)
  ß_N <- numeric(n-P)
  ß <- c(ß_P, ß_N)
  
  # On crée e
  sigma2_e <- 0.10
  e <- rnorm(N, 0, sigma2_e)
  
  # On crée Y
  Y <- X %*% ß + e
  
  resultat <- eval_methode_auto(X, Y, 5)
  return(resultat)
}

simulation(100,50,20)
