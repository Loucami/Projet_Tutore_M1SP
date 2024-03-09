
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

resLASSO <- list()
resSCAD <- list()
resMCP <- list()
resSTEP <- list()
for (i in 1:5) {
  resultats <- Resultats(xdata[,1:5000],ydata)
  resLASSO[[i]] <- resultats$resultats_nb$LASSO
  resSCAD[[i]] <- resultats$resultats_nb$SCAD
  resMCP[[i]] <- resultats$resultats_nb$MCP
  resSTEP[[i]] <- resultats$resultats_nb$STEP
}
list(LASSO = mean(unlist(resLASSO)), 
     SCAD = mean(unlist(resSCAD)), 
     MCP = mean(unlist(resMCP)), 
     STEP = mean(unlist(resSTEP)))

