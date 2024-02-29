
source("Simulation/SimuTest.R")
library(glmnet)
library(ncvreg)
library(MASS)
library(splus2R)
library(caret)
library(ROCR)

### ------------------------------  Résultat  ------------------------------ ###


resultat_simulation <- function(Nb, Covs, Pos_Cov, Rep) {
  
  donnees <- list()
  new_donnees <- simulation(Nb, Covs[1], Pos_Cov, Rep)
  donnees[[1]] <- new_donnees
  betas <- list(new_donnees[[1]]$ß)
  for (c in 2:length(Covs)) {
    old_donnees <- new_donnees
    new_donnees <- simulation(Nb, Covs[c], Pos_Cov, Rep, ß = old_donnees[[1]]$ß)
    betas[[c]] <- new_donnees[[1]]$ß
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
    
    # Résultat unique pour chaque méthode sur chaque jeu de données
    resultat_grps <- list()
    nb_lignes <- nrow(resultat)
    for (j in 1:4) {
      j_grp <- seq(j, nb_lignes, by = 4)
      moy_cols <- colMeans(resultat[j_grp,])
      resultat_grps[[j]] <- round(moy_cols)
    }
    resultat_grps <- list(LASSO = resultat_grps[[1]],
                        SCAD = resultat_grps[[2]],
                        MCP = resultat_grps[[3]],
                        STEP = resultat_grps[[4]])
    
    # Résumé des 4 méthodes sur l'ensemble des données 
    # resume_grps <- list()
    # for (j in 1:4) {
    #   j_grp <- seq(j, nb_lignes, by = 4)
    #   somme_lignes <- rowSums(resultat[j_grp, ])
    #   moy_grp <- mean(somme_lignes)
    #   resume_grps <- append(resume_grps, moy_grp)
    # }
    # resume_grps <- list(LASSO = resume_grps[[1]], 
    #                     SCAD = resume_grps[[2]],
    #                     MCP = resume_grps[[3]], 
    #                     STEP = resume_grps[[4]])
    
    resume_grps <- list(LASSO = length(which(resultat_grps[[1]]!=0)),
                        SCAD = length(which(resultat_grps[[2]]!=0)),
                        MCP = length(which(resultat_grps[[3]]!=0)),
                        STEP = length(which(resultat_grps[[4]]!=0)))
    
    resultats[[k]] <- list(resultat_tot = resultat, 
                           resultat_grp = resultat_grps, 
                           resultat_res = resume_grps,
                           beta = betas[[k]])
    k = k+1
  }
  return(resultats)
}


### -----------------------------  Évaluation  ----------------------------- ###

evaluation_simulation <- function(Nb, Covs, Pos_Cov, Rep) {
  
  # Simule des jeux de données et récupère les résultats
  resultats <- resultat_simulation(Nb, Covs, Pos_Cov, Rep)
  
  evaluation <- list()
  for (i in seq_along(resultats)) {
    covs_og <- factor(ifelse(resultats[[i]]$beta != 0, 1, 0))
    covs_pred <- resultats[[i]]$resultat_grp
    eval <- list()
    for (j in 1:4) {
      # Matrice de confusion
      conf.matrix <- confusionMatrix(factor(covs_pred[[j]]), covs_og, positive = '1')
      # Courbe ROC
      pred <- prediction(covs_pred[[j]], covs_og)
      perf.ROC <- performance(pred, "tpr", "fpr")
      plot(perf, main="ROC Curve", col= "blue", lwd = 2)
      # Courbe Precision-Recall
      perf.PR <- performance(pred, "prec", "rec")
      plot(perf, main="Precision-Recall Curve", col= "red", lwd = 2)
      # Evaluation 
      eval[[j]] <- list(Accuracy = round(conf.matrix$overall['Accuracy'], 3),
                        F1score = round(conf.matrix$byClass['F1'], 3),
                        Sensitivity = round(conf.matrix$byClass['Sensitivity'], 3),
                        Specificity = round(conf.matrix$byClass['Specificity'], 3), 
                        AUC = round(conf.matrix$byClass['Balanced Accuracy'], 3))
    }
    method_df <- data.frame(matrix(nrow = 4, ncol = 5))
    rownames(method_df) <- c("LASSO", "SCAD", "MCP", "STEP")
    colnames(method_df) <- c("Accuracy", "F1score", "Sensitivity", "Specificity", "AUC")
    for (k in seq_along(eval)) {method_df[k, ] <- unlist(eval[[k]])}
    attr(method_df, "nb_variables") <- Covs[i]
    evaluation[[i]] <- method_df
  }
  return(evaluation)
}


### --------------------------------  Test  -------------------------------- ###


evalualtion <- evaluation_simulation(100, c(50, 75), 20, 100)
for (eval in evaluation){
  print(paste(attributes(eval)$nb_variables, 'variables'))
  print(eval)
}

# library(dplyr)
# library(knitr)
# library(kableExtra)
# library(webshot2)
# kable(eval[[1]], caption = "Résultats pour Covs = 50", format = "latex") %>%
#   kable_styling() %>%
#   save_kable("tableau1.tex")
