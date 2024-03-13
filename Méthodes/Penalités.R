

library(dplyr)


# GRAPHIQUES PÉNALITÉS
par(mfrow=c(1,3))
lambda = 1
alpha = 3

# Graphique 1 : Pénalité appliquée à ß, en fonction de la valeur initialie de ß
lasso_penalty <- function(beta, lambda) {
  abs(beta) * lambda
}
mcp_penalty <- function(beta, lambda, alpha) {
  ifelse(abs(beta) <= alpha*lambda, lambda * abs(beta) - (beta^2 / (2 * alpha)), alpha*(lambda^2)/2)
}
scad_penalty <- function(beta, lambda, alpha) {
  case_when(
    abs(beta) <= lambda ~ lambda * abs(beta),
    (abs(beta) > lambda) & (abs(beta) <= (alpha * lambda)) ~ (2*alpha*lambda*abs(beta) - (beta^2 + lambda^2)) / (2*(alpha-1)),
    TRUE ~ (lambda^2*(alpha+1))/2
  )
} 
curve(lasso_penalty(x,lambda), from = -4, to = 4, col = 'red', xlab = 'ß', ylab = 'p(ß)')
curve(scad_penalty(x, lambda, alpha), from = -4, to = 4, col = 'blue', add = TRUE)
curve(mcp_penalty(x,lambda, alpha), from = -4, to = 4, col = 'darkgreen', add = TRUE)
legend("topright", legend = c("Lasso", "SCAD", "MCP"), col = c("red", "blue", "darkgreen"), lty = 1)

# Graphique 2 : Dérivée de la pénalité (= évolution de la pénalité)
lasso_derivative <- function(beta, lambda) {
  lambda * sign(beta)
}
scad_derivative <- function(beta, lambda, alpha) {
  case_when(
    abs(beta) <= lambda ~ lambda,
    abs(beta) > lambda & abs(beta) <= alpha * lambda ~ (2*alpha*lambda - 2*beta)/(2*(alpha-1)),
    TRUE ~ 0
  )
}
mcp_derivative <- function(beta, lambda, alpha) {
  ifelse(abs(beta) <= alpha * lambda, lambda*sign(beta) - (2*beta)/(2*alpha), 0)
}
curve(lasso_derivative(x,lambda), from = 0.0001, to = 4, col = 'red', xlab = 'ß', ylab = "p'(ß)", ylim = c(0:1))
curve(scad_derivative(x, lambda, alpha), from = 0.0001, to = 4, col = 'blue', add = TRUE)
curve(mcp_derivative(x,lambda, alpha), from = 0.0001, to = 4, col = 'darkgreen', add = TRUE)
legend("topright", legend = c("Lasso", "SCAD", "MCP"), col = c("red", "blue", "darkgreen"), lty = 1)

# Graphique 3 : ß ajustés, en fonction du ß initiale (= fonction inverse de la pénalité)
lasso_seuil <- function(beta, lambda) {
  case_when(
    beta < -lambda ~ beta + lambda, 
    beta > lambda ~ beta - lambda, 
    TRUE ~ 0
  )
}
scad_seuil <- function(x) {
  case_when(
    abs(x) < lambda ~ 0,
    lambda <= abs(x) & abs(x) < alpha-lambda ~ sign(x) * (abs(x) - 1),
    alpha-lambda <= abs(x) & abs(x) < alpha ~ sign(x) * ((alpha-lambda) * abs(x) - alpha),
    TRUE ~ x
  )
}
mcp_seuil <- function(x) {
  case_when(
    abs(x) < lambda ~ 0, 
    abs(x) > alpha ~ x,
    -alpha < x & x < -lambda ~ alpha * (x + lambda) / 2,
    lambda < x & x < alpha ~ alpha * (x - lambda) / 2
  )
}
values <- seq(-4,4,length.out=100)
plot(values, values, type = 'n', col = "black", xlab = "x", ylab = "ß")
lines(values, values, col = "black", lty = 2)  # Ajouter la ligne en pointillés
curve(lasso_seuil(x, lambda), col = 'red', add = TRUE)
curve(scad_seuil(x), col = 'blue', add = TRUE)
curve(mcp_seuil(x), col = 'darkgreen', add = TRUE)
legend("bottomright", legend = c("Lasso", "SCAD", "MCP"), col = c("red", "blue", "darkgreen"), lty = 1)

