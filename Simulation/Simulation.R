

source("Simulation/Methodes.R")
library(splus2R)


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
    X <- rmvnorm(Nb, mean = mu, sd = sqrt(sigma2_X))
    
    # On crée e
    e <- rnorm(Nb, 0, sigma2_e)
    
    # On crée Y
    Y <- X %*% ß + e
    
    res[[r]] <- list(x = X, y = Y)
  }
  
  return(res)
}


### ------------------------------  Résultat  ------------------------------ ###


resultat_simulation <- function(Nb, Cov, Pos_Cov, Rep) {
  
  # Création des données
  donnees <- simulation(Nb, Cov, Pos_Cov, Rep)
  
  # Évaluation par les quatre méthodes sur tout les réplicas
  resultat <- numeric(Cov)
  for (i in 1:Rep) {
    xdata <- donnees[[i]]$x
    ydata <- donnees[[i]]$y
    covs <- methode_select(xdata, ydata, 5)
    resultat <- rbind(resultat, covs)
  }
  resultat <- resultat[-1,] # J'ai crée resultat avec une ligne vide pour pouvoir ajouter ensuite plusieurs lignes à la fois, je l'enlève donc ici
  
  # Résumé des 4 méthodes sur l'ensemble des données 
  resume_grps <- list()
  nb_lignes <- nrow(resultat)
  for (i in 1:4) {
    i_grp <- seq(i, nb_lignes, by = 4)
    somme_lignes <- rowSums(resultat[i_grp, ])
    moy_grp <- mean(somme_lignes)
    resume_grps <- append(resume_grps, moy_grp)
  }
  resume_grps <- list(LASSO = resume_grps[[1]], 
                      SCAD = resume_grps[[2]],
                      MCP = resume_grps[[3]], 
                      STEP = resume_grps[[4]])
  
  return(list(result = resultat, resum = resume_grps, data = donnees))
}


### --------------------------------  Test  -------------------------------- ###


resultat1 <- resultat_simulation(100, 30, 20, 100)
resultat1$resum

resultat2 <- resultat_simulation(100, 50, 20, 100)
resultat2$resum

resultat3 <- resultat_simulation(100, 100, 20, 100)
resultat3$resum

resultat4 <- resultat_simulation(100, 200, 20, 100)
resultat4$resum

resultat5 <- resultat_simulation(100, 500, 20, 100)
resultat5$resum



