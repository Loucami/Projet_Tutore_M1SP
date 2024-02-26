
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


### ------------------------------  Résultat  ------------------------------ ###


resultat_simulation <- function(Nb, Covs, Pos_Cov, Rep) {
  
  donnees <- list()
  new_donnees <- simulation(Nb, Covs[1], Pos_Cov, Rep)
  donnees[[1]] <- new_donnees
  for (c in 2:length(Covs)) {
    old_donnees <- new_donnees
    new_donnees <- simulation(Nb, Covs[c], Pos_Cov, Rep, ß = old_donnees$ß)
    donnees[[c]] <- new_donnees
  }
  
  # Évaluation pour chaque jeu de donnée
  resultats <- list()
  k = 1
  for (d in donnees) {
    
    # Évaluation par les quatre méthodes sur tout les réplicas
    resultat <- numeric(length(d[[1]]$ß))
    for (i in 1:Rep) {
      xdata <- d[[i]]$X
      ydata <- d[[i]]$Y
      covs <- methode_select(xdata, ydata, 5)
      resultat <- rbind(resultat, covs)
    }
    resultat <- resultat[-1,] # J'ai crée resultat avec une ligne vide pour pouvoir ajouter ensuite plusieurs lignes à la fois, je l'enlève donc ici
  
    # Résumé des 4 méthodes sur l'ensemble des données 
    resume_grps <- list()
    nb_lignes <- nrow(resultat)
    for (j in 1:4) {
      j_grp <- seq(j, nb_lignes, by = 4)
      somme_lignes <- rowSums(resultat[j_grp, ])
      moy_grp <- mean(somme_lignes)
      resume_grps <- append(resume_grps, moy_grp)
    }
    resume_grps <- list(LASSO = resume_grps[[1]], 
                        SCAD = resume_grps[[2]],
                        MCP = resume_grps[[3]], 
                        STEP = resume_grps[[4]])
    
    resultats[[k]] <- list(result = resultat, resum = resume_grps, data = d)
    k = k+1
  }
  
  return(resultats)
}


### --------------------------------  Test  -------------------------------- ###

liste_covs <- c(30, 50, 100, 200)
resimu <- resultat_simulation(100, liste_covs, 20, 100)
for (r in 1:length(resimu)) {
  print(paste(liste_covs[r],'covariables'))
  print(resimu[[r]]$resum)
  }
