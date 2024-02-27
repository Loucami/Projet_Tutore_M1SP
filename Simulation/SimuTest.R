
source("Simulation/Methodes.R")
library(splus2R)


### -----------------------------  Simulation  ----------------------------- ###


simulation <- function(Nb, Cov, Pos_Cov, Rep, ß = NULL) {
  
  # Paramètres pour X
  mu <- rnorm(Cov)
  sigma2_X <- rexp(Cov, 2) #Le 2 est arbitraire
  
  # Paramètres pour ß
  if (is.null(ß)) {
    ß_P <- rnorm(Pos_Cov, 0, 2)
    ß_N <- numeric(Cov-Pos_Cov)
    ß <- c(ß_P, ß_N)
  }
  
  # Paramètre pour e
  sigma2_e <- 0.10
  
  res = list()
  
  for (r in 1:Rep) {
    # On crée X
    X <- rmvnorm(Nb, mean = mu, sd = sqrt(sigma2_X))
    
    # On crée e
    e <- rnorm(Nb, 0, sigma2_e)
    
    # On crée Y
    Y <- X %*% ß + e
    
    res[[r]] <- list(X = X, Y = Y, ß = ß)
  }
  
  return(res)
}

