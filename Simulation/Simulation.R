
library(glmnet)
library(ncvreg)
library(MASS)
library(splus2R)


### ------------------------  Méthodes d'évaluation  ----------------------- ###


eval_methode_auto <- function(xdata, ydata, folds) {

  ### ------ LASSO ------ ###
  
  ## Normalisation de X
  xdata_scale <- scale(xdata)
  
  ## Modèle d'origine et plot
  # fit <- glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  # plot(fit)
  
  ## Ajustement du modèle LASSO avec validation croisée
  cvfit_lasso <- cv.glmnet(xdata_scale, ydata, alpha = 1, nfolds = folds)
  fit <- glmnet(xdata_scale, ydata, alpha = 1, lambda = cvfit_lasso$lambda.min) 
  # plot(cvfit_lasso)
  
  ## Coefficients sélectionnées
  ß_lasso <- coef(fit, s = cvfit_lasso$lambda.min)
  covs_lasso <- ifelse(ß_lasso[-1]!=is.na(ß_lasso[-1]) & ß_lasso[-1]!=0, 1, 0)
  
  ### ------ SCAD ------ ###
  
  ## Modèle d'origine et plot
  # fit <- ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  # plot(fit)
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit_scad <- cv.ncvreg(xdata, ydata, penalty = 'SCAD', nfolds = folds)
  # plot(cvfit_scad)
  
  # Coefficients sélectionnées
  ß_scad <- coef(cvfit_scad)
  covs_scad <- ifelse(ß_scad[-1]!=is.na(ß_scad[-1]) & ß_scad[-1]!=0, 1, 0)
  
  ### ------  MCP  ------ ###
  
  ## Modèle d'origine et plot
  # fit <- ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)
  # plot(fit)
  
  ## Ajustement du modèle SCAD avec validation croisée
  cvfit_mcp <- cv.ncvreg(xdata, ydata, penalty = 'MCP', nfolds = folds)
  # plot(cvfit_mcp)
  
  ## Coefficients sélectionnées
  ß_mcp <- coef(cvfit_mcp)
  covs_mcp <- ifelse(ß_mcp[-1]!=is.na(ß_mcp[-1]) & ß_mcp[-1]!=0, 1, 0)
  
  ### ------  STEPAIC  ------ ###
  
  ## Ajustement du modèle SCAD avec validation croisée
  l_data <- cbind.data.frame(ydata, xdata)
  f.sat <- as.formula('ydata ~ .')
  model.sat <- lm(f.sat, data = l_data)
  f.cst <- as.formula('ydata ~ 1')
  model.cst <- lm(f.cst, data = l_data)
  model_step = stepAIC(model.cst,  scope = list(upper = model.sat, lower = model.cst), trace = FALSE)

  ## Coefficients sélectionnées
  ß_step <- coef(model_step)
  cols_select <- gsub("`", "", names(ß_step[-1]))
  cols <- names(l_data[-1])
  covs_step <- rep(0, length(cols))
  covs_step[match(cols_select, cols)] <- ß_step[-1]
  covs_step <- ifelse(covs_step!=is.na(covs_step) & covs_step!=0, 1, 0)

  return(rbind(covs_lasso, covs_scad, covs_mcp, covs_step))
  #return(list(lasso = covs_lasso, scad = covs_scad, mcp = covs_mcp, stepAIC = covs_step))
}


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
  
  # Création des jeux de données 
  données <- simulation(Nb, Cov, Pos_Cov, Rep)
  resultat <- numeric(Cov)
  
  # Évaluation par les quatre méthodes sur tout les réplicas
  for (i in 1:Rep) {
    xdata <- données[[i]]$x
    ydata <- données[[i]]$y
    covs <- eval_methode_auto(xdata, ydata, 5)
    
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
  
  return(list(result = resultat[-1,], resum = resume_grps, data = données))
}

resultat <- resultat_simulation(100, 100, 20, 20)
resultat$resum

