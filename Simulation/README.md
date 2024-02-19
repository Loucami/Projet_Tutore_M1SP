# Simulation

Ici sont présents les différents codes permettant la simluation de jeux de données fictifs, sur lesquels des méthodes de régréssions sont ensuite appliquées *(cf. Méthodes)*.


## Changements au 20 février 2024

#### Changer de plot ?
On prend le meilleur lambda donc le plot des coefs n'affiche rien, puisque la pénalité est unique. 

#### Dimension des données dans le cas du Lasso 
On a scale les données pour le Lasso.

#### Peut être plus pratique d'avoir une fonction qui fasse tout les tests en même temps ? 
On a crée Méthode_Eval_Auto() qui lancent automatiquement tout les tests.

#### Regarder d'autres méthodes 
On a rajouté la méthode de StepAIC (package MASS).

#### Regarder pour un dépot github
On a créé le repository complet. 

#### Sortie : liste des covariables récupérées (essayer avec 200 covariables)
Pour des questions de lisibilités nous avons gardé la version complète pour le moment. À rajouter dans la fonction finale !