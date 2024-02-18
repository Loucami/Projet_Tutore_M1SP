# Chargement du package glmnet
library(glmnet)


# Génération d'une simulation / Importation d'une simulation
data("QuickStartExample")
X <- QuickStartExample$x
y <- QuickStartExample$y
data <- paste("Nombre d'enregistrement :", nrow(X), "/ Nombre de covariables : ", ncol(X))
print(data)


scad_eval <- function(xdata, ydata, folds) {
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit <- cv.ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  plot(cvfit)
  
  ## Prédiction des résultats
  y_pred <- predict(cvfit, xdata) 
  ß_pred <- coef(cvfit)
  
  ## Affichage des résultats
  # Pénalité 
  penalty <- paste('Pénalité :', round(cvfit$lambda.min,4))
  print(penalty)
  
  # Nombre de variables sélectionnées
  n_vars <- predict(cvfit, type = "nvars")
  number_vars <- paste('Nombre de covariable sélectionnées : ', n_vars)
  print(number_vars)
  
  # Variables sélectionnées
  vars <- predict(cvfit, type = "vars")
  list_vars <- paste("Variables séletcionnées :", paste(vars, collapse = ", "))
  print(list_vars)
  
  # Coefficients des variables sélectionnées
  coefs <- ß_pred[ß_pred!=0]
  print(coefs)
}

scad_eval(X,y,5)
scad_eval(X,y,4)
