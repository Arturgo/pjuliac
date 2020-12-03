## Gestion des types :

Nous avons choisi de suivre les recommandations de l’énoncé. Nous faisons deux passes distinctes pour construire le contexte puis pour vérifier les types. 
Nous avons choisi d’écrire une fonction pour trouver les variable (relativement) globales d’une expression, une pour le typage des expressions et une pour gérer les Lvalues.
Nous avons opté pour construire un arbre le plus proche possible de celui donné en entrée. Nous n’utilisons pas les inférences de types pour la production de code ; vu que le typage dynamique doit être codé, nous avons préféré ne pas ajouter de typage statique.
Les variables locales sont renumérotées pour éviter d’avoir à le faire ultérieurement. A ceci près, l’arbre renvoyé est identique à celui reçu.

Nous avons eu des difficultés sur le fonctionnement du Julia. En particulier, pour la redirection des fonctions, nous n’avions pas compris qu’il y avait une priorité ( favoriser une fonction dont les arguments sont plus restrictifs), ni qu’il était interdit d’avoir des fonctions avec même nom et mêmes arguments.
L’autre difficulté a été sur les variables globales et locales.  Nous pensions initialement simplement différentier les variables globales et locales. En effet, une variable locale n’en écrase pas une autre. Mais cela n’a pas été possible à cause de l’itérateur des boucles for qui se comporte différemment. Nous avons finalement associé un nombre aléatoire en plus au type Variable.
