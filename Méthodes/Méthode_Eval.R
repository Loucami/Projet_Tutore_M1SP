# Chargement du package glmnet
library(glmnet)
library(ncvreg)
library(MASS)

# Génération d'une simulation / Importation d'une simulation
data("QuickStartExample")
X <- QuickStartExample$x
y <- QuickStartExample$y


eval_methode <- function(xdata, ydata, folds, penalty) {
  
  info <- paste("Nombre d'enregistrements :", nrow(xdata), "; Nombre de covariables :", ncol(xdata))
  print(info)
  
  if (penalty == 'LASSO') {
    
    ## Normalisation de X
    xdata <- scale(xdata)
    
    ## Ajustement du modèle LASSO avec validation croisée
    fit <- glmnet(xdata, ydata, alpha = 1)
    plot(fit)
    cvfit <- cv.glmnet(xdata, ydata, alpha = 1, nfolds = folds)
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
    # coefs <- t(data.frame(Coefficients = ß_pred[ß_pred!=0]))
    # vars <- c('(Intercept)', vars)
    # colnames(coefs) <- vars
    # print(coefs)
  
  } else if (penalty=='MCP' | penalty=='SCAD') {
    
    ## Ajustement du modèle SCAD avec validation croisée
    cvfit <- cv.ncvreg(xdata, ydata, penalty = penalty, nfolds = folds)
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
    # coefs <- ß_pred[ß_pred!=0]
    # print(coefs)
    
  } else if (penalty == 'STEPAIC') {
    
    ## Ajustement du modèle SCAD avec validation croisée
    data_stepAIC <- as.data.frame(cbind(xdata, ydata))
    modele <- lm(V21 ~ ., data = data_stepAIC)
    invisible(capture.output(fit <- stepAIC(modele, direction = "both")))
    
    ## Prédiction des coefficients
    y_pred <- predict(fit) 
    ß_pred <- coef(fit)
    
    ## Affichage des résultats
    # Nombre de variables sélectionnées 
    nb_vars <- length(ß_pred[-1])
    number_vars <- paste('Nombre de covariable sélectionnées :', nb_vars)
    print(number_vars)
    
    # Variables sélectionnées 
    vars <- which(ß_pred[-1]!=0)
    liste_vars <- paste('Liste des covariables sélectionnées :', paste(vars, collapse = ", "))
    print(liste_vars)
  }
}

eval_methode(X, y, 5, 'LASSO')
eval_methode(X, y, 5, 'MCP')
eval_methode(X, y, 5, 'SCAD')
eval_methode(X, y, 5, 'STEPAIC')




### CHANGEMENTS ###

#1 Changer de plot => Comme on prend le meilleur lambda le plot des coefs n'affiche rien, puisque la pénalité est unique

#2 Hypothèse pour le Lasso : Scale(X) (=> Normaliser X) => Rajouté

#3 Peut être lancer les trois plutot que de choisir => Méthode_Eval_Auto 

#4 Regarder d'autres méthodes (Step-AIC package MASS) => Rajouté 

# Regarder pour un dépot github (lien ent)

# Sortie : liste des covariables récupérées (essayer avec 200 covariables)
