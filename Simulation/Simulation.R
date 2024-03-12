

library(mvtnorm)
library(splus2R)


### -----------------------------  Simulation  ----------------------------- ###


Simulation <- function(Nb, Covs, Pos_Covs, Rep, ß = NULL) {
  
  resultats = list()
  if (is.numeric(Covs) && length(Covs) == 1) { Covs <- c(Covs) }
  
  # Création d'une liste pour accueillir les différents jeux de données
  donnees <- list()
  for (Cov in Covs) {
    
    # Paramètres pour X
    mu <- rnorm(Cov)
    sigma2_X <- rexp(Cov, 2) #Le 2 est arbitraire
    
    # Paramètres pour ß
    if (is.null(ß)) {
      ß_P <- rnorm(Pos_Covs, 0, sqrt(2))
      ß_N <- numeric(Cov-Pos_Covs)
      ß <- c(ß_P, ß_N)
    } else {
      ß <- c(ß, rep(0, Cov-length(ß)))
    }
    
    # Paramètre pour e
    sigma2_e <- 0.10
    
    res = list()
    
    for (r in 1:Rep) {
      # On crée X
      X <- rmvnorm(Nb, mean = mu, sd = sqrt(sigma2_X))
      
      # On crée e
      e <- rnorm(Nb, 0, sqrt(sigma2_e))
      
      # On crée Y
      Y <- X %*% ß + e
      
      res[[r]] <- list(X = X, Y = Y, ß = ß)
    }
    
    resultats[[as.character(Cov)]] <- res
  }
  
  return(resultats)
}
