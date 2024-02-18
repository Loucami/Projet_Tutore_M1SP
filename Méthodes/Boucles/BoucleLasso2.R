# Chargement du package glmnet
library(glmnet)

# Création de la fonction
lasso_eval2 <- function(n, p, x, k) {
  
  # Création des différentes listes de stockages
  avg_rmse <- numeric(x)
  avg_r2 <- numeric(x)
  avg_nbcov <- numeric(x)
  
  set.seed(666) 
  
  for (i in 1:x) {
    # Génération de données aléatoires
    X <- matrix(rnorm(n * p, mean=-50, sd=2), nrow = n)
    ß <- c(rnorm(p, mean = 0, sd = 2.5))
    e <- rnorm(n, mean = 0, sd = 0.2)
    y <- X %*% ß + e
    
    # Division des données en k plis pour une validation croisée
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
      
      # Ajustement du modèle LASSO avec validation croisée
      cvfit <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5) 
      #best_model <- glmnet(X_train, y_train, alpha = 1, lambda = cvfit$lambda.min) ? 
      y_pred <- predict(cvfit, newx = X_test, s = "lambda.min") 
      
      # Calcul du RMSE pour chaque pli
      rmse[j] <- sqrt(sum((y_pred - y_test)^2) / length(y_test))
      
      # Calcul du R2 pour chaque pli
      sst <- sum((y_test - mean(y_test))^2)
      sse <- sum((y_pred - y_test)^2)
      r2[j] <- 1 - sse/sst
      
      # Nombre de covariables sélectionnées pour chaque pli
      ß_pred <- coef(cvfit, s = "lambda.min")
      nb_cov[j] <- sum(ifelse(ß_pred[ß_pred!=0 & ß_pred!=is.na(ß_pred)], 1, 0)) - 1
    }
    
    # Calcul de la moyenne des RMSE, des R2, et des covariables sélectionnées pour l'ensemble du jeu de données 
    avg_rmse[i] <- mean(rmse)
    avg_r2[i] <- mean(r2)
    avg_nbcov[i] <- mean(nb_cov)
  }
  
  # Calcul de la moyenne du RMSE, du R2 et des covariables sélectionnées pour l'ensemble des itérations
  average_rmse <- c(mean(avg_rmse), sd(avg_rmse))
  average_r2 <- mean(r2)
  average_cov <- mean(avg_nbcov)
  
  # Retourne une chaîne de caractères décrivant les résultats
  result <- paste('(RMSE) mean :', round(average_rmse[1],4), '/ sd :', round(average_rmse[2],4), 
                  '/ Nombre de covariables sélectionnées :', average_cov, '/ r2 :', round(average_r2, 5))
  return(result)
}


# Exemple
lasso_eval2(100, 20, 100, 5)
lasso_eval2(50, 15, 200, 5)
lasso_eval2(40, 40, 100, 5)
