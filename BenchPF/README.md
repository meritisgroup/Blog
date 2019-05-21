# BenchPF
Petit projet permettant de lancer les tests présentés dans les articles sur la programmation fonctionnelle. Les exemple produit ici le sont en F#.
La plupard des tests sont automatisé dans des boucles ne récupérant aucun résultat. Il faut donc prendre soin à ne pas laisser les optimisation du compilateur just in time qui pourrait ne pas jouer ces boucles.

# Recommandation d'utilisation
Même s'il se présente sous la forme d'un petit test en mode console, il est plutot recommandé de le lancer en mode débug. Cela permet de garantir que les optimisations de la compilation Just In Time ne sont pas actives.
Par ailleurs cela permet également de poser des breakpoint afin d'observé le comportement du programme. Notamment dans les fonctions récursives qui sont "tail recusive" on peut observer de cette manière que la stack disparait. Il faut pour cela vérifier que dans les options du projet la "tail recursion optimization" est activée.
Noter que la "tail recursion optimization" n'est pas active par défaut en mode debug. La raison est simple, ca permet de garder l'historique de la stack quand on debug une fonction bénéficiant normalement de cette optimisation.

Have fun ;)

# Test primitive obsession
Ce test montre qu'il n'est pas couteux d'utiliser des type record en capsulant les types primitifs (int, double etc ...).
Cette pratique permet de donner un sens au variable utilisée et permet d'éviter la confusion entre les variables manipulées dans un programme.

# Test copy on write
Montre une optimisation dans l'update fonctionnel des record. Seul les champs modifiés lors de l'update ont une nouvelle référence.
Ce genre d'optimisation intervient afin de minimiser le coût de l'immutabilité.
