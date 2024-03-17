
# Application

Dans ce dossier se trouve les codes utilisés afin d'appliquer les méthodes LASSO, SCAD et MCP à des données réelles relatives au vaccin de la fièvre jaune. 

## Donnes.RData

Fichier contenant les données relatives au vaccin contre la fièvre jaune. Il comprend, pour 71 individus, le taux d'anticorps produit en réponse au vaccin ainsi que l'expression de 10086 gènes. Plus de détails dans *Explication.Rmd*.

## Explication.Rmd

Explique en détail la composition des données réelles *(Donnees.RData)*. 

## Methodes.R

Reprend la Methode2() (avec *stability selection*) utilisée sur les simulations, en ne gardant que les méthodes LASSO, SCAD et MCP. 

## Resultats.R

Applique les 3 méthodes de sélections de covariables aux données réelles.