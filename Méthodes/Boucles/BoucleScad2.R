# Chargement du package ncvreg
library(ncvreg)

# Création de la fonction
scad_rmse2 <- function(n, p, x, k) {
  
  avg_rmse <- numeric(x)
  avg_r2 <- numeric(x)
  set.seed(666) 
  
  for (i in 1:x) {
    # Génération de données aléatoires
    X <- matrix(rnorm(n * p), nrow = n)
    ß <- c(rnorm(p, mean = 0, sd = 1.5))
    e <- rnorm(n, mean = 0, sd = 0.2)
    y <- X %*% ß + e
    
    # Division des données en k plis pour la validation croisée
    folds <- sample(1:k, n, replace = TRUE)
    rmse <- numeric(k)
    r2 <- numeric(k)
    nb_cov <- numeric(k)
    
    # Validation croisée
    for (j in 1:k) {
      X_train <- X[folds != j, ]
      y_train <- y[folds != j]
      X_test <- X[folds == j, ]
      y_test <- y[folds == j]
      
      # Ajustement du modèle SCAD avec validation croisée
      scad_fit <- cv.ncvreg(X_train, y_train, penalty = "SCAD", nfolds = 5) 
      y_pred <- predict(scad_fit, X_test, s = 'lambda.min') 
      
      # Calcul du MSE pour chaque pli
      rmse[j] <- sqrt(sum((y_pred - y_test)^2) / length(y_test))
      
      # Calcul du R2 pour chaque pli
      sst <- sum((y_test - mean(y_test))^2)
      sse <- sum((y_pred - y_test)^2)
      r2[j] <- 1 - sse/sst
      
      # Nombre de covariables sélectionnées
      ß_pred <- coef(scad_fit, s = "lambda.min")
      nb_cov[j] <- sum(!is.na(ß_pred[-1]))
    }
    
    # Calcul de la moyenne des k MSE, et des k R2
    avg_rmse[i] <- mean(rmse)
    avg_r2[i] <- mean(r2)
  }
  
  # Calcul de la moyenne et de l'écart type des MSE sur l'ensemble des itérations
  average_rmse <- c(mean(avg_rmse), sd(avg_rmse))
  avg_cov <- mean(nb_cov)
  
  # Calcul de la moyenne R2 moyen sur l'ensemble des itérations 
  average_r2 <- mean(r2)
  
  # Retourne une chaîne de caractères décrivant les résultats
  result <- paste('(MSE) mean :', round(average_rmse[1],4), '/ sd :', round(average_rmse[2],4), 
                  '/ Nombre de covariables sélectionnées :', avg_cov, '/ r2 :', round(average_r2, 5))
  return(result)
}

# Exemple d'utilisation de la fonction
scad_rmse2(100, 20, 100, 5)
scad_rmse2(50, 15, 200, 5)
