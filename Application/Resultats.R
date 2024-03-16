
load('Application/Donnees.RData')
source('Application/Methodes.R')
library(dplyr)
library(glmnet) 
library(glmnetUtils)
library(ncvreg)
library(MASS)
library(foreach)
doParallel::registerDoParallel(cluster <- parallel::makeCluster(parallel::detectCores()-5))


# NA => 0 NA
sum(which(is.na(gene_expression)))
sum(which(is.na(sp.df)))


# Individus => 71 en commmun 
d1 <- as.data.frame(rownames(gene_expression))
colnames(d1) <- 'id'
d2 <- sp.df['id']
id_commun <- merge(d1, d2, by='id')
nrow(id_commun)


# Création de X et Y 
gene_expression <- gene_expression[order(rownames(gene_expression)), ] #Pour que les individus soient dans le bon ordre
# X
xdata <- gene_expression %>% as.data.frame() %>% filter(rownames(gene_expression) %in% id_commun$id)
xdata <- as.matrix(xdata)
# Y
ydata <- sp.df %>% filter(sp.df$id %in% id_commun$id) 
ydata <- ydata$theta


# Résultats
resultats <- Methodes(xdata,ydata)
resultats$resultats_covs
# => GAPDH

# SHARP : Stability selection pour Lasso
# library(sharp)
# lasso <- VariableSelection(xdata,ydata)
# sum(SelectedVariables(lasso))
