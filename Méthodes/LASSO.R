# Chargement du package glmnet
library(glmnet)

# Génération d'une simulation / Importation d'une simulation
data("QuickStartExample")
X <- QuickStartExample$x
y <- QuickStartExample$y
data <- paste("Nombre d'enregistrement :", nrow(X), "/ Nombre de covariables : ", ncol(X))
print(data)

lasso_eval <- function(xdata, ydata, folds) {
  
  ## Ajustement du modèle LASSO avec validation croisée
  cvfit <- cv.glmnet(xdata, ydata, alpha = 1, nfolds = folds)
  plot(cvfit)
  fit <- glmnet(xdata, ydata, alpha = 1, lambda = cvfit$lambda.min) 
  
  ## Prédiction des résultats
  y_pred <- predict(fit, newx = xdata, s = cvfit$lambda.min) 
  ß_pred <- coef(fit, s = cvfit$lambda.min)
  
  ## Affichage des résultats
  # Pénalité 
  penalty <- paste('Pénalité :', round(cvfit$lambda.min,4))
  print(penalty)
  
  # Nombre de variables sélectionnées 
  nb_vars <- sum(ifelse(ß_pred[-1][ß_pred[-1]!=0 & ß_pred[-1]!=is.na(ß_pred[-1])], 1, 0))  
  number_vars <- paste('Nombre de covariable sélectionnées :', nb_vars)
  print(number_vars)
  
  # Variables sélectionnées 
  vars <- which(ß_pred[-1]!=0 & ß_pred[-1]!=is.na(ß_pred[-1])) 
  liste_vars <- paste('Liste des covariables sélectionnées :', paste(vars, collapse = ", "))
  print(liste_vars)
  
  # Coefficients des variables sélectionnées
  coefs <- t(data.frame(coefficients = ß_pred[ß_pred!=0]))
  vars <- c('(Intercept)', vars)
  colnames(coefs) <- vars
  print(coefs)
}

lasso_eval(X,y,5)
lasso_eval(X,y,4)
