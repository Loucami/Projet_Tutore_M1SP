# Simulation

Ici sont présents les différents codes permettant la simluation de jeux de données fictifs, sur lesquels des méthodes de régréssions sont ensuite appliquées *(cf. Méthodes)*.


# Notes réunion 20/02 

### Pour Simulation()
- 100 simulation(), où sigma2_e, les paramètres de X, et C, sont choisis en amont.
- Pour chaque simulation, on crée N individus => Xi, ei
- Regarder mvtnorm pour tirer directement une matrice gaussienne
- Simulation => Juste simulation. Créer ensuite Résultat() qui combien Eval_method() + Simulation()

### Autre
- Regarder les scores de tests 
- Regarder pour : 100/200, 100/500, 100/1000.
- Idéalement, les 200 premières covariables des 500 (ou des 1000) sont les mêmes que celles présentent dans la simulation de 200.
- Penser à mettre une partie Implémentation (Version de R utilisé, packages particuliers utilisés, github)
- Regarder les templates Overleaf 
