# Simulation

Ici sont présents les codes permettant la simluation de jeux de données fictifs, sur lesquels des méthodes de régréssions sont ensuite appliquées, puis évaluer.


## Methodes.R

Reprend la fonction de sélection de covariables (*Methodes.R*) présente dans la section **Méthodes** et la modifie légèrement, de façon à ressortir une matrice binaire avec en ligne les quatre méthodes de sélection (LASSO, SCAD, MCP, STEPAIC), en colonne les covariables, et des 1 au niveau des covariables sélectionnées. 


## Simulation.R

Simule la création d'un jeu de données en fonction de paramètres prédéfinis. 

À titre d'exemple, un jeu de données contenant *N* enregistrements, *C* covariables, *P* covariables d'intérêts, et *R* réplicats, est simulée comme suit : 
1. Des vecteurs *mu* (rnorm) et *sigma2* (rexp) de taille *C* sont fixés (-> **X**).
2. Le vecteur **ß** (rnorm) des coefficients de taille *C* est fixé.
3. Un deuxième *sigma2* est fixé à 0.10 (-> **e**).
4. Ensuite, pour *R* réplicats : 
- **X** est déterminé grâce à *mu* et *sigma2*, qui attribue une distribution différente à chaque covariable. 
- **e** (rnorm) est déterminé à partir du *sigma2* fixé à 0.10.
- **Y** est déterminé à partir **ß**, ainsi que des nouveaux **X** et **e**. 

Le nombre de covariables peut également être un vecteur contenant plusieurs valeurs, afin de créer plusieurs jeux de données disposant d'un nombre différents de covariables. Dans une telle situation, le vecteur **ß** sera conservé d'un jeu de données à l'autre, et des variables non pertinente (=0) lui seront simplement ajouter au fur et à mesure. L'idée est de gardé les mêmes covariables d'intérêts. 


## Evaluation.R

À partir d'une simulation, la fonction applique à chaque réplica de chaque jeu de données les quatre méthodes de sélections de covariables utilisées dans ce projet. Elle calcule ensuite pour chaque jeu de données et pour chaque méthode un vecteur moyen des covariables sélectionnées, et le nombre moyen de covariables sélectionnées. Enfin, à partir de ces informations elle évalue les quatre méthodes sur chaque jeu de données, en utilisant notamment l'*Accuracy*, le *F1Score*, le *FNR*, le *FPR*, le *AUC*, et le *RMSE*. 

