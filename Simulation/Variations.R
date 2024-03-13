
source('Simulation/Methodes.R')
source('Simulation/Simulation.R')

### ------------------------  Méthodes de sélection  ----------------------- ###

Resultats1 <- function(xdata, ydata) {
  
  res <- Methodes1(xdata, ydata)
  
  nb_covs <- list(STEPAIC = sum(res[1,]),
                  LASSO = sum(res[2,]),
                  SCAD = sum(res[3,]),
                  MCP = sum(res[4,]))
  
  which_covs <- list(STEPAIC = which(res[1,]!=0),
                     LASSO = which(res[2,]!=0),
                     SCAD = which(res[3,]!=0),
                     MCP = which(res[4,]!=0))
  
  resultats <- list(resultats_tot = res, 
                    resultats_nb = nb_covs, 
                    resultats_covs = which_covs)
  
  return(resultats)
}

Resultats2 <- function(xdata, ydata) {
  
  res <- Methodes2(xdata, ydata)
  
  nb_covs <- list(STEPAIC = sum(res[1,]),
                  LASSO = sum(res[2,]),
                  SCAD = sum(res[3,]),
                  MCP = sum(res[4,]))
  
  which_covs <- list(STEPAIC = which(res[1,]!=0),
                     LASSO = which(res[2,]!=0),
                     SCAD = which(res[3,]!=0),
                     MCP = which(res[4,]!=0))
  
  resultats <- list(resultats_tot = res, 
                    resultats_nb = nb_covs, 
                    resultats_covs = which_covs)
  
  return(resultats)
}

# Variabilité des résultats
simu <- Simulation(100, 50, 20, 100)
X <- simu$'50'[[1]]$X
Y <- simu$'50'[[1]]$Y
resultats1 <- resultats2 <- numeric(4)
for (i in 1:15) {
  meth1 <- Resultats1(X,Y)$resultats_nb
  resultats1 <- rbind(resultats1, as.numeric(meth1))
  meth2 <- Resultats2(X,Y)$resultats_nb
  resultats2 <- rbind(resultats2, as.numeric(meth2))
}
resultats1 <- resultats1[2:16,]
resultats2 <- resultats2[2:16,]

# Sans stability selection
plot(resultats1[,2], xlab = 'Occurences', ylab = 'Variables sélectionnées', main = 'Sans stability selection', col = 'red', ylim = c(15,50))
abline(h = mean(resultats1[,2]), col = 'red', lty = 'dashed')
points(resultats1[,3], col = 'blue')
abline(h = mean(resultats1[,3]), col = 'blue', lty = 'dashed')
points(resultats1[,4], col = 'darkgreen')
abline(h = mean(resultats1[,4]), col = 'darkgreen', lty = 'dashed')
legend("topright", legend = c("Lasso", "SCAD", "MCP"), col = c("red", "blue", "darkgreen"), pch = 1, lty = 1, cex = 0.8)

# Avec stability selection
plot(resultats1[,2], xlab = 'Occurences', ylab = 'Variables sélectionnées', main = 'Avec stability selection', col = 'red', ylim = c(15,50))
abline(h = mean(resultats1[,2]), col = 'red', lty = 'dashed')
points(resultats1[,3], col = 'blue')
abline(h = mean(resultats1[,3]), col = 'blue', lty = 'dashed')
points(resultats1[,4], col = 'darkgreen')
abline(h = mean(resultats1[,4]), col = 'darkgreen', lty = 'dashed')
legend("topright", legend = c("Lasso", "SCAD", "MCP"), col = c("red", "blue", "darkgreen"), pch = 1, lty = 1, cex = 0.8)
