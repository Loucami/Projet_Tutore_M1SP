
source("Simulation/Methodes.R")
source("Simulation/Simulation.R")

# Package Méthodes
library(glmnet) 
library(ncvreg)
library(MASS)
# Packages Simulation
library(mvtnorm)
library(splus2R)
# Packages Evaluation
library(caret)
library(ROCR)



### -----------------------------  Evaluation  ----------------------------- ###


Evaluation <- function(simulation) {
  
  #1 --- RESULTATS --- #
  
  resultats <- list()
  
  for (s in seq_along(simulation)) {
    
    # Sélection de covariables pour chaque réplica du jeu de données, à l'aide de nos 4 méthodes
    res <- numeric(length(simulation[[s]][[1]]$ß))
    for (r in seq_along(simulation[[s]])) {
      xdata <- simulation[[s]][[r]]$X
      ydata <- simulation[[s]][[r]]$Y
      covs <- methode_select(xdata, ydata, 5)
      res <- rbind(res, covs)
    }
    res <- res[-1,] # Je supprime la première ligne nulle
    
    # Sélection unique de covariables pour chaque méthode
    res_grps <- list()
    nb_lignes <- nrow(res)
    for (i in 1:4) {
      i_grp <- seq(i, nb_lignes, by = 4)
      moy_cols <- colMeans(res[i_grp,])
      res_grps[[i]] <- round(moy_cols)
    }
    res_grps <- list(STEPAIC = res_grps[[1]],
                     LASSO = res_grps[[2]],
                     SCAD = res_grps[[3]],
                     MCP = res_grps[[4]])
    
    # Résumé du nombre de covariables sélectionnées par chaque méthode, pour notre jeu de donnée
    resume_grps <- list(STEPAIC = length(which(res_grps[[1]]!=0)),
                        LASSO = length(which(res_grps[[2]]!=0)),
                        SCAD = length(which(res_grps[[3]]!=0)),
                        MCP = length(which(res_grps[[4]]!=0)))
    
    # Aggrégation des resultats bruts, des résultats uniques pour chaque méthodes, du nombre de covariables séletcionnées
    resultats[[as.character(length(simulation[[s]][[1]]$ß))]] <- list(resultats_tot = res, 
                                                                      resultats_grp = res_grps, 
                                                                      resultats_res = resume_grps,
                                                                      beta = simulation[[s]][[1]]$ß)
  }
  
  #2 --- EVALUATION --- #
  
  evaluations <- list()
  
  # Evaluation des résultats de chaque jeu de données...
  for (res in resultats) {
    covs_og <- factor(ifelse(res$beta != 0, 1, 0))
    covs_pred <- res$resultats_grp
    eval <- list()
    
    # ... Pour chaque méthode
    for (j in 1:4) {
      
      # Matrice de confusion
      conf.matrix <- confusionMatrix(factor(covs_pred[[j]]), covs_og, positive = '1')
      
      # Courbe ROC
      pred <- prediction(covs_pred[[j]], covs_og)
      perf.ROC <- performance(pred, "tpr", "fpr") 
      plot(perf.ROC, main="ROC Curve", col= "blue", lwd = 2)
      lines(0:1, 0:1, col = "black", lty = 2)
      
      # Calcul du FNR / FPR 
      FNR <- conf.matrix$table[2, 1] / sum(conf.matrix$table[2, ], na.rm = TRUE)
      FPR <- conf.matrix$table[1, 2] / sum(conf.matrix$table[1, ], na.rm = TRUE)
      
      # Aggrégation des évaluations pour chaque méthode, de chaque jeu de données
      eval[[j]] <- list(F1score = round(conf.matrix$byClass['F1'], 3),
                        FNR = round(FNR,3),
                        FPR = round(FPR,3),
                        AUC = round(conf.matrix$byClass['Balanced Accuracy'], 3))
    }
    
    # Aggrégation des évaluations des 4 méthodes dans un tableau
    tab_evals <- data.frame(matrix(nrow = 4, ncol = 4))
    rownames(tab_evals) <- c("STEPAIC", "LASSO", "SCAD", "MCP")
    colnames(tab_evals) <- c("F1score", "FNR", "FPR", "AUC")
    for (j in seq_along(eval)) {
      tab_evals[j, ] <- unlist(eval[[j]])
    }
    attr(tab_evals, "nb_variables") <- length(res$beta)
    
    evaluations[[as.character(length(res$beta))]] <- tab_evals
  }
  
  return(list(resultats = resultats, evaluations = evaluations))
}



# Variables 
covariables <- c(100,200,500,1000)
simulation <- Simulation(100, covariables, 20, 100)
evaluation <- Evaluation(simulation)
evaluation$evaluations

# Tableaux des résultats
eval <- evaluation$evaluations
nb_covs <- as.character(attributes(eval)$nb_variables)
tableau <- rbind(eval$'100', eval$'200', eval$'500', eval$'1000')

tableau %>%
  kable('latex', booktabs = T, caption = 'Évaluations des méthodes stepAIC, Lasso, SCAD et MCP pour 100 réplicas') %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  pack_rows("100 covariables", 1, 4) %>%
  pack_rows("200 covariables", 5, 8) %>% 
  pack_rows("500 covariables", 9, 12) %>% 
  pack_rows("1000 covariables", 13, 16) %>% 
  save_kable(file = "tableau_simulations.tex")


