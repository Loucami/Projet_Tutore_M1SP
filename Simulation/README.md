# Simulation

Ici sont présents les différents codes permettant la simluation de jeux de données fictifs, sur lesquels des méthodes de régréssions sont ensuite appliquées *(cf. Méthodes)*.


## Changements au 20 février 2024

#### 1. Regarder pour un dépot github
Nous avons créé le repository complet. 

#### 2. Dimension des données dans le cas du Lasso 
Nous avons *scale* les données pour le Lasso.

#### 3. Fonction automatique 
Nous avons crée *Méthode_Eval_Auto()* qui lance automatiquement tout les tests.

#### 4. Meilleur plot ? 
Nous avons ajouté le plot illustrant l'évolution des coefficients en fonction de la pénalité. 
**Comment l'interpréter ?**

#### 5. Regarder d'autres méthodes 
Nous avons rajouté la méthode de StepAIC (package MASS). **Malheureusement elle ne fonctionne pas dans *Simulation()***

#### 6. Sortie : liste des covariables récupérées (essayer avec 200 covariables)
Pour des questions de lisibilités nous avons gardé la version complète pour le moment. À rajouter dans la fonction finale !