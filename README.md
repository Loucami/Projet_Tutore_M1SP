# Projet_Tutore_M1SP

*Projet Tutoré - Master 1 Santé Publique 2023/2024*

## Sujet 11 : Evaluation de la pertinence de méthodes de sélection de covariables.

L'objectif de ce projet est d'identifier les gènes influençant la réponse au sein d'un ensemble de données revient à découvrir des paramètres clés pour mieux comprendre le mécanisme sous-jacent. En effet, en médecine personnalisée par exemple, cette approche est cruciale pour cibler les gènes responsables de maladies spécifiques. En comprenant ces gènes, les chercheurs peuvent développer des traitements plus ciblés, offrant ainsi une approche plus précise pour traiter les maladies à l'échelle individuelle. Dans le domaine de la génétique, où les études explorent des milliers de gènes simultanément, nous sommes confrontés à une situation de grande dimension. Cela signifie que le nombre de variables à considérer est extrêmement élevé, introduisant des défis statistiques. Dans ce contexte, les modèles linéaires, tels que le **lasso**, l'**elastic net**, le **SCAD** et le **MCP**, deviennent des outils essentiels pour sélectionner les gènes pertinents et comprendre les mécanismes génétiques sous-jacents aux maladies. Le projet consistera à étudier plusieurs méthodes de sélection de gènes dans le cadre d'un modèle linéaire. Le lasso, l'elastic net, le SCAD et le MCP pourront être utilisés algorithme permettant la sélection des gènes pertinents. Pour mesurer la pertinence de chaque modèle, des outils tels que le Mean Squared Error (MSE) , la Validation Croisée (CV), le Bayesian Information Criterion (BIC) et le Akaike Information Criterion (AIC) pourront être utilisés. Cette évaluation approfondie permettra de déterminer quelle méthode offre la meilleure méthode. Les méthodes seront appliquées à des données simulées issues de l’essai clinique Prevac-UP.

## Méthodes
Contient tout les codes utilisés pour mettre en place les 4 méthodes de sélections de covariables de ce projet : Lasso, SCAD, MCP, stepAIC. 

## Simulation
Contient des codes de simulations de données fictives, sur lesquels sont ensuite appliquées nos quatre méthodes de sélections. 

## Application 
Contient les données réelles utilisées dans ce projet (Fièvre Jaune), ainsi que les codes utilisés afin d'appliquer les méthodes de sélections de covariables. 
