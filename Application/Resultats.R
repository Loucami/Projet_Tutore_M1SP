
load('Application/data.RData')
source('Application/Methodes.R')
library(dplyr)
library(kableExtra)
library(glmnet) 
library(glmnetUtils)
library(ncvreg)
library(MASS)
library(foreach)
doParallel::registerDoParallel(cluster <- parallel::makeCluster(parallel::detectCores()-5))


# NA => 0 NA
sum(which(is.na(gene_expression)))
sum(which(is.na(sp.df)))


# Individus => 96 en commmun 
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
res <- Methodes(xdata,ydata)
res$resultats_covs
res$resultats_frq[,6163]
res$resultats_frq[,4339]
#save(resultats, file = 'Application/resultats.RData')


# Tableaux
load('Application/resultats.RData')
tableau <- cbind(resultats$resultats_frq[,6163], resultats$resultats_frq[,4339])
colnames(tableau) <- c('PAX6', 'JPH3')
rownames(tableau) <- c('LASSO', 'SCAD', 'MCP')

tableau %>% 
  kable('latex', booktabs = T, caption = 'Titre') %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c(" " = 1, "Gènes sélectionnés" = 2)) %>% 
  pack_rows("Méthodes", 1, 3) #%>% save_kable(file = "tableau_simulations.tex")

# SHARP : Stability selection pour Lasso
# library(sharp)
# lasso <- VariableSelection(xdata,ydata)
# sum(SelectedVariables(lasso))
