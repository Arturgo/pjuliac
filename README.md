Ce dépôt recueille le compilateur de petit-julia réalisé par Étienne Rossignol et Arthur Léonard pour le cours de compilation de l'École Normale Supérieure.

Pour compiler le projet, utilisez :

```shell
make
```

Cela va créer un exécutable nommé pjuliac qui est le compilateur.

# Story Time

## Lexer et Parseur

Nous avons utilisé ocamllex et menhir comme recommandé.
Il n'y a rien de particulier à dire sur le lexeur (lexer.mll), nous nous sommes contentés de suivre au mieux les léxèmes du sujet.
De même, pour le parseur, notre grammaire menhir (parser.mly) ressemble le plus possible celle du sujet.
La seule exception notable est la gestion du '-' unaire que nous gérons en dehors des priorités, cela nous a donné du fil à retordre.

Nous générons un arbre de syntaxe abstraite qui essaie de générer le moins de constructions différentes, les opérateurs sont par exemple vus comme des fonctions particulières.

## Gestion des types :

Nous avons choisi de suivre les recommandations de l’énoncé. Nous faisons deux passes distinctes pour construire le contexte puis pour vérifier les types. 
Nous avons choisi d’écrire une fonction pour trouver les variable (relativement) globales d’une expression, une pour le typage des expressions et une pour gérer les Lvalues.
Nous avons opté pour construire un arbre le plus proche possible de celui donné en entrée. Nous n’utilisons pas les inférences de types pour la production de code ; vu que le typage dynamique doit être codé, nous avons préféré ne pas ajouter de typage statique.
Les variables locales sont renumérotées pour éviter d’avoir à le faire ultérieurement. A ceci près, l’arbre renvoyé est identique à celui reçu.

Nous n'avons pas traité la localisation précise des erreurs.

Nous avons eu des difficultés sur le fonctionnement du Julia. En particulier, pour la redirection des fonctions, nous n’avions pas compris qu’il y avait une priorité ( favoriser une fonction dont les arguments sont plus restrictifs), ni qu’il était interdit d’avoir des fonctions avec même nom et mêmes arguments.
L’autre difficulté a été sur les variables globales et locales.  Nous pensions initialement simplement différentier les variables globales et locales. En effet, une variable locale n’en écrase pas une autre. Mais cela n’a pas été possible à cause de l’itérateur des boucles for qui se comporte différemment. Nous avons finalement associé un nombre aléatoire en plus au type Variable.

## Production du code :

C'est en bonne voie, mais pas tout à fait terminé, il reste à gérer la plupart des erreurs (notamment pour les variables non-définies). Nous passons tous les tests d'exécutions valides sauf un.
Comme le typage est par essence incalculable, toutes les vérifications sur les types doivent être refaites lors de l'exécution, c'est pourquoi nous avons décidé de ne pas nous servir de l'information générée lors du typage à l'exception du calcul de la portée des variables.
Nous essayons d'écrire le moins de code possible en assembleur, et le plus possible en Julia, c'est pourquoi nous avons développé un système d'arithmétique de pointeurs pour la gestion des objets, des fonctions variadiques, et du dispatch multiple.

La suite de l'histoire au prochain rendu !
