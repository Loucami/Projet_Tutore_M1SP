# Simulation.R

Ici sont présents les codes permettant la simluation de jeux de données fictifs, sur lesquels des méthodes de régréssions sont ensuite appliquées *(cf. Méthodes)*.


### Simulation()

Pour *N* enregistrements, *C* covariables, *P* covariables d'intérêts, et *R* réplicats : 
1. Des vecteurs *mu* (rnorm) et *sigma2* (rexp) de taille *C* sont fixés (-> **X**).
2. Le vecteur **ß** (rnorm) des coefficients de taille *C* est fixé.
3. Un deuxième *sigma2* est fixé à 0.10 (-> **e**).
4. Ensuite, pour *R* réplicats : 
- **X** est déterminé grâce à *mu* et *sigma2*, qui attribue une distribution différente à chaque covariable. 
- **e** (rnorm) est déterminé à partir du *sigma2* fixé à 0.10.
- **Y** est déterminé à partir **ß**, ainsi que des nouveaux **X** et **e**. 

### Resultat()

Pour chacun des *R* réplicats, les quatre méthodes de sélections de covariables sont appliquées : **LASSO**, **SCAD**, **MCP**, **STEPAIC**. Les covariables sélectionnées sont stockées dans une matrice binaire, qui est finalement renvoyer. Une matrice finale de taille *4xR*. 