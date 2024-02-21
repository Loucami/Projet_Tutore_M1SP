
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
  print(list_vars_mcp)
  
  ### ------  STEPAIC  ------ ###

  print('--------  STEPAIC  --------')

  ## Ajustement du modèle SCAD avec validation croisée
  l_data <- cbind.data.frame(ydata, xdata)
  f.sat <- as.formula('ydata ~ .')
  model.sat <- lm(f.sat, data = l_data)
  f.cst <- as.formula('y ~ 1')
  model.cst <- lm(f.cst, data = l_data)
  model_step = stepAIC(model.cst,  scope = list(upper = model.sat, lower = model.cst), trace = FALSE)

  ## Prédiction des coefficients
  y_pred_aic <- predict(model_step)
  ß_pred_aic <- coef(model_step)

  ## Affichage des résultats
  # Nombre de variables sélectionnées
  nb_vars_aic <- length(ß_pred_aic[-1])
  number_vars_aic <- paste('Nombre de covariable sélectionnées :', nb_vars_aic)
  print(number_vars_aic)

  # Variables sélectionnées
  vars_aic <- which(ß_pred_aic[-1]!=0)
  liste_vars_aic <- paste('Liste des covariables sélectionnées :', paste(vars_aic, collapse = ", "))
  #print(liste_vars_aic)

}



### -----------------------------  Simulation  ----------------------------- ###


simulation <- function(Nb, Cov, Pos_Cov, Rep) {
  
  # Paramètres pour X
  mu <- rnorm(Cov)
  sigma2_X <- rexp(Cov, 2) #Le 2 est arbitraire
  
  # Paramètres pour ß
  ß_P <- rnorm(Pos_Cov, 0, 2)
  ß_N <- numeric(Cov-Pos_Cov)
  ß <- c(ß_P, ß_N)
  
  # Paramètre pour e
  sigma2_e <- 0.10
  
  res = list()
  
  for (r in 1:Rep) {
    # On crée X
    X <- matrix(data = NA, nrow = Nb, ncol = Cov)
    for (i in 1:Cov) { X[,i] <- rnorm(Nb, mu[i], sigma2_X[i]) } 
    
    # On crée e
    e <- rnorm(Nb, 0, sigma2_e)
    
    # On crée Y
    Y <- X %*% ß + e
    
    res[[r]] <- list(X = X, Y = Y)
  }
  
  return(res)
}

#simulation(100,50,20,10)



### -----------------------------  Resultat  ----------------------------- ###


resultat <- function(Nb, Cov, Pos_Cov, Rep) {
  
  # Création des jeux de données 
  données <- simulation(Nb, Cov, Pos_Cov, Rep)
  
  # Évaluation par les quatre méthodes
  for (i in 1:Rep) {
    xdata <- données[[i]]$X
    ydata <- données[[i]]$Y
    covs <- eval_methode_auto(xdata, ydata, 5)
    
    resultat <- rbind(resultat, covs)
  }
  
  return(list(res = resultat, data = données))
}


#Simulation 
# 100 simulation(), où sigma2_e, les paramètres de X, et C, sont choisis en amont.
# Pour chaque simulation, on crée N individus => Xi, ei
# -> Regarder mvtnorm pour tirer directement une matrice gaussienne
# Simulation => Juste simulation
# Resultat() ensuite 
# return(list(blabla=blabla))
# Regarder les scores de tests 
# 100/200, 100/500, 100/1000.
# Idéalement, les 200 premières covariables des 500 (ou des 1000) sont les mêmes que celles présentent dans la simulation de 200.
# Penser à mettre une partie Implémentation (Version de R utilisé, packages particuliers utilisés, github)
# Regarder les templates Overleaf 
