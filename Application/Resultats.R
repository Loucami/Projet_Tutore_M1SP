
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
# Fréquences 
for (i in 1:3){print(head(sort(res$resultats_frq[i,], decreasing = T)))}
sort_frq <- sort(res$resultats_frq[3,], decreasing = T)
plot(sort_frq, xlim = c(0,100), ylim = c(0,0.3))
# PAX6 et JPH3
frqPAX6 <- res$resultats_frq[,4339]
frqJPH3 <- res$resultats_frq[,6163]
# Moyenne, median et 100ème percentil de sélection 
means <- rowMeans(res$resultats_frq)
Q2 <- rowMedians(res$resultats_frq) 
P100 <- apply(res$resultats_frq, 1, function(x) quantile(x, probs = 0.999))
#save(resultats, file = 'Application/resultats.RData')


# Tableaux 1
load('Application/resultats.RData')
tableau1 <- cbind(frqPAX6,frqJPH3)
colnames(tableau1) <- c('PAX6', 'JPH3')
rownames(tableau1) <- c('LASSO', 'SCAD', 'MCP')
tableau1 %>% 
  kable('latex', booktabs = T, caption = 'Titre') %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  add_header_above(c(" " = 1, "Gènes sélectionnés" = 2)) %>% 
  pack_rows("Méthodes", 1, 3) 

# Tableau 2
tableau2 <- cbind(round(means,4),round(P100,2))
colnames(tableau2) <- c('Moyenne', 'Q100')
rownames(tableau2) <- c('LASSO', 'SCAD', 'MCP')
tableau2 %>% 
  kable('latex', booktabs = T, caption = 'Titre') %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  pack_rows("Méthodes", 1, 3) 

# SHARP : Stability selection pour Lasso
# library(sharp)
# lasso <- VariableSelection(xdata,ydata)
# sum(SelectedVariables(lasso))
