# Chargement du package ncvreg
library(ncvreg)

# Création de la fonction
scad_mse <- function(n, p, x) {
  
  avg_mse <- numeric(x) 
  set.seed(666) 
  
  for (i in 1:x) {
    # Génération de données aléatoires
    X <- matrix(rnorm(n * p), nrow = n)
    ß <- c(rnorm(p, mean = 1, sd = 0.5))
    e <- rnorm(n, mean = 0, sd = 0.2)
    y <- X %*% ß + e
    
    # Division des données en ensembles d'entraînement et de test
    train_indices <- sample(1:n, size = n * 0.8) # 80% des données pour l'entraînement
    test_indices <- setdiff(1:n, train_indices) # Le reste pour le test
    X_train <- X[train_indices, ]
    y_train <- y[train_indices]
    X_test <- X[test_indices, ]
    y_test <- y[test_indices]
    
    # Ajustement du modèle et prédiction
    scad_fit <- cv.ncvreg(X_train, y_train, penalty = "SCAD", nfolds = 5) 
    y_pred <- predict(scad_fit, X_test)
    mse <- mean((y_pred - y_test)^2) # Calcul de l'erreur quadratique moyenne (RMSE)

    # Calcul de la moyenne et de l'écart type des MSE
    avg_mse[i] <- mse
  }
  
  # Calcul de la moyenne et de l'écart type des MSE sur l'ensemble des itérations
  average_mse <- c(mean(avg_mse), sd(avg_mse))
  
  # Retourner une chaîne de caractères décrivant les résultats
  result <- paste('(MSE) mean :', round(average_mse[1], 4), '/ sd :', round(average_mse[2], 4))
  return(result)
}

# Exemple d'utilisation de la fonction
scad_mse(100, 20, 100)
scad_mse(50, 15, 200)