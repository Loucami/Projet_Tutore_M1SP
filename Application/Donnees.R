
library(dplyr)
load('Application/data.RData')
source('Application/Resultats.R')

# NA => 0 NA
sum(which(is.na(gene_expression)))
sum(which(is.na(sp.df)))

# Individus => 71 en commmun 
d1 <- as.data.frame(rownames(gene_expression))
colnames(d1) <- 'id'
d2 <- sp.df['id']
id_commun <- merge(d1, d2, by='id')
nrow(id_commun)

# Test 
gene_expression <- gene_expression[order(rownames(gene_expression)), ] #Pour que les individus soient dans le bon ordre
xdata <- gene_expression %>% as.data.frame() %>% filter(rownames(gene_expression) %in% id_commun$id)
xdata <- as.matrix(xdata)

ydata <- sp.df %>% filter(sp.df$id %in% id_commun$id) 
ydata <- ydata$theta

resultats1 <- Resultats1(xdata[,1:500],ydata)
resultats1$resultats_nb

resultats2 <- Resultats2(xdata[,1:500],ydata)
resultats2$resultats_nb


# Stability selection pour Lasso
library(sharp)
lasso <- VariableSelection(xdata[,1:500],ydata)
sum(SelectedVariables(lasso))



