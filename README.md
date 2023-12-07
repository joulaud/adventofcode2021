This repository is obsolete. See https://git.sr.ht/~avalenn/adventofcode.git/tree/main/item/README.md for more up-to-date information.

# Advent Of Code 2021

Ce repo contient ma tentative de résoudre les problèmes posés sur https://adventofcode.com/2021

Cette année on va tenter de le faire en Scheme avec Guile.

## Day 1

```
cat day01/inputs/day01.01.txt| guile ./day01/first.scm
cat day01/inputs/day01.01.txt| guile ./day01/second.scm
```

Le premier jour est vraiment pour se dérouiller mais j'ai déjà
l'impression de faire des bidouilles pas très clean.

### deuxième passe

J'ai découvert les Streams ([srfi-41]) en Scheme et ça me parait
intéressant comme façon de faire les choses.

[srfi-41]: https://srfi.schemers.org/srfi-41/srfi-41.html

En les utilisant j'ai l'impression de faire un code plus propre et
plus lisible et surtout plus facilement adaptable.


```
cat day01/inputs/day01.01.txt| guile ./day01/secondwithstreams.scm
```
## Day 2

Aujourd'hui les Streams ne me servent quasiment à rien.

J'ai passé du temps à voir comment je pouvais définir un type
spécifique pour gérer les commandes du sous-marin et sa position
courante. Pour le moment les commandes sont juste des fonctions.

```
cat day02/inputs/example.txt| guile ./day02/first.scm
cat day02/inputs/day02.01.txt| guile ./day02/first.scm
```

```
cat day02/inputs/example.txt| guile ./day02/second.scm
cat day02/inputs/day02.01.txt| guile ./day02/second.scm
```
## Day 3

J'ai passé beaucoup beaucoup de temps à essayer de trouver comment faire
des tests unitaires correctement avec Guile. Ma problématique étant
de pouvoir faire des tests sur des fonctions internes (non-exportées)
d'un module, depuis un deuxième fichier de test.

J'ai essayé deux approches:
- `set-current-module`
- `@@`

J'ai eu des problèmes avec l'interaction entre `set-current-module`
et les `define-immutable-record-type` qui me généraient des erreurs
`Wrong type to apply: #<syntax-transformer count0>` sans doute à cause
de modifications dans l'ordre d'évaluation.

Les `@@` m'obligent à redéclarer chaque définition que je veux utilise
dans mes tests donc c'est très verbeux mais ça a l'air de fonctionner.

Le fait de vouloir faire des tests unitaires m'a aussi poussé à faire
des fonctions petites, et parfois trop petites et du coup j'ai perdu pas
mal de temps.

Il faut aussi que j'arrive à être plus pragmatique et à résoudre la
tâche demandée et ne pas trop anticiper les besoins futurs. Si mes tests
sont bien faits, la refacto sera possible en cas de besoin d'évolution.

Je fais aussi une fonction `main` appelable grâce à l'option `-e`
de Guile. Ça me permet de charger le module sans aucun effet de bord.

```
export GUILE_LOAD_PATH=day03:$GUILE_LOAD_PATH
# Tests unitaires
guile day03/tests.scm
guile day03/tests-second.scm
# Tests complets
guile -e '(first)' -s day03/first.scm <day03/inputs/example.txt
guile -e '(second)' -s day03/second.scm <day03/inputs/example.txt
# Résultats
guile -e '(first)' -s day03/first.scm <day03/inputs/day03.txt
guile -e '(second)' -s day03/second.scm <day03/inputs/day03.txt
```

## Day 4

Pour cet exercice je me suis pris la tête avec la structure de données
à utiliser. Je me suis donc familiarisé avec les Vector de Scheme puis
les Arrays quand j'ai voulu représenter les grilles de Bingo dans un
tableau à deux dimensions. Sauf que cette représentation n'était pas
du tout adaptée au problème et que j'ai perdu du temps pour rien.

J'ai eu du mal aussi avec tout ce qui était itérations et ai appris à
me servir des let nommés (`(let loop ((arg1 init1) (result initresult))
(if mybool result (loop arg1 result)))`)

```
echo
export GUILE_LOAD_PATH=day04:$GUILE_LOAD_PATH
# Tests unitaires
guile day04/tests.scm
# Résultats
guile -e '(first)' -s day04/first.scm <day04/inputs/day04.txt
```

## Day 5

J'ai l'impression de me dérouiller. J'y ai encore passé beaucoup de
temps mais en vrai ça va quand même mieux. Les structures de données
internes viennent plus facilement.

Mon petit souci est sur ma capacité à tester certaines fonctions
intermédiaires et du coup je n'ai pas tout de suite vu le bug avec les
lignes diagonales du type `(0,5) -> (5,0)`. Mais le fix a été rapide.

```
echo
export GUILE_LOAD_PATH=day05:$GUILE_LOAD_PATH
# Tests unitaires
guile day05/tests.scm
# Résultats
guile -e '(first)' -s day05/first.scm <day05/inputs/day05.txt
```

## Day 6

Facile. Comme souvent la difficulté vient de la modélisation dans les
structures de données. Là par exemple il suffit de suivre le nombre
de poissons à chaque étape du cycle et de ne pas se laisser distraire
par la liste de chaque poisson.

J'ai quand même mis trop de temps, mais bon je me bagarre avec des
erreurs de syntaxe et surtout des erreurs d'offset (il faut 9 slots pour
aller de 0 à 8)

```
echo
export GUILE_LOAD_PATH=day06:$GUILE_LOAD_PATH
# Tests unitaires
guile day06/tests.scm
# Résultats
guile -e '(first)' -s day06/first.scm <day06/inputs/day06.txt
```

## Day 7

Encore un assez facile. D'ailleurs je n'ai pas pris le temps d'écrire
des tests unitaires, le code se décrit lui-même. Pour la partie 2
j'ai juste rendu paramétable la fonction qui transforme la distance en
consommation d'essence.

```
echo
guile -e '(first)' -s day07/first.scm <day07/inputs/day07.txt
```

## Day 8

J'ai plus galéré avec celui-là.

Déjà il y a du "code métier" avec des règles bizarres qui me font
écrire du code qui ressemble à
```scheme
(let (
         (two-three-five (filter (size-is 5) sets))
         (zero-six-nine (filter (size-is 6) sets))
         ;; NEUF et QUATRE ont quatre segments communs
         (nine  (car (filter (size-of-intersection-is four 4) zero-six-nine)))
         ;; seul F est commun entre SIX et UN
         (six   (car (filter (size-of-intersection-is one 1) zero-six-nine)))
         (F     (char-set-intersection one six))
         ;; ZERO n'est ni NEUF ni SIX
         (zero  (car (filter (lambda (x) (and (not (equal? x nine)) (not (equal? x six)))) zero-six-nine)))
```

J'ai triché en utilisant le type `char-set` qui vient nativement et
qui permet de faire des intersections facilement.

J'ai aussi perdu du temps en essayant d'aller vite et d'utiliser des
paires pour tout représenter au lieu de faire des
`define-record-type`. Du coup j'ai du déboguer plusieurs fois des
erreurs du type `In procedure cdr: Wrong type argument in position 1
(expecting pair):`.

Bref, c'est fait et ça fonctionne.

```
echo
export GUILE_LOAD_PATH=day08:$GUILE_LOAD_PATH
# Tests unitaires
guile day08/tests.scm
# Résultats
guile -e '(first)' -s day08/first.scm <day08/inputs/day08.txt
```

## Day 9

Le plus frustrant jusqu'ici. L'exercice n'est pas compliqué pourtant.

J'ai recouru au brute-force en parcourant de façon brutale le tableau
en deux dimensions jusqu'à ce que j'atteigne un point de convergence
sur la taille d'un bassin. Et je répète ce brute-force autant de fois
qu'il y a de bassins. Première frustration.

Mon code est un mix de fonctionnel et d'impératif et est à mon avis
illisible. Deuxième frustration.

J'ai passé beaucoup de temps à essayer d'éviter le brute-force et le
code impératif. Manque de pragmatisme de ma part. Troisième frustration.

```
echo
export GUILE_LOAD_PATH=day09:$GUILE_LOAD_PATH
# Tests unitaires
guile day09/tests.scm
# Résultats
guile -e '(first)' -s day09/first.scm <day09/inputs/day09.txt
```

```
%%%% Starting test parsing input  (Writing full log to "parsing input.log")
# of expected passes      2
%%%% Starting test neighbours list  (Writing full log to "neighbours list.log")
# of expected passes      3
%%%% Starting test low points  (Writing full log to "low points.log")
# of expected passes      4
%%%% Starting test risk-level  (Writing full log to "risk-level.log")
# of expected passes      1
%%%% Starting test fill-basin  (Writing full log to "fill-basin.log")
# of expected passes      6
result1: 577
result2: 1069200
```

## Day 10

Beaucoup plus facile.

Deux notes:

Il faut absolument que j'utilise les record à la place des paires. Par
exemple les paires de listes à chaque fois c'est compliqué à comprendre
et surtout c'est impossible à déboguer. Et pourtant je l'ai déjà
dit le Day 8.

J'ai perdu du temps en oubliant d'exclure les lignes en erreur du calcul
du score d'autocomplete. Le bogue n'était pas évident à comprendre.

## Day 11

Après le Day9 je n'ai pas répété les mêmes erreurs, du coup c'est
beaucoup plus facile, même si mes algos ne scalent très probablemet pas.

Et puis je suis clairement en mode impératif dès que je manipule des
structures 2D, il faudrait que je suffixe la plupart de mes procédures
par un `!`. Ce ne sont pas des fonctions, elles modifient des états.

# Day 12

Un parcours de graphe profondeur somme toute classique.

Et cette fois-ci j'ai commencé par créer des structures de données
beaucoup trop compliquées par rapport au besoin alors qu'il suffit
d'une simple liste d'adjacence.  Même après avoir simplifié j'ai une
structure de données `<graph>` avec une liste de noeuds qui n'est jamais
utilisée et une liste `visited` qui n'est jamais ne serait-ce que créée
(puisque la liste des noeuds visitées est locale à l'algorithme mais
pas liée au graphe).

Donc encore une fois manque de pragmatisme, et mon focus sur les
structures de données doit savoir laisser place à un "les algos d'abord"
surtout dans ce genre de cas où c'est self-evident et qu'on sait qu'il
n'y aura pas d'évolution à faire.

## Day 13

Algoritmique simple encore, à condition de ne pas confondre x et y.

L'affichage est nécessaire pour résoudre le problème cette fois-ci
et pas seulement pour le debug. Je n'ai pas mis en place l'OCR pour
sortir la réponse au format texte.

J'en ai profité pour refacto un peu la façon dont je structure mes
fichiers pour avoir une lib `utils.scm` réutilisable et j'ai joué avec
un `def-syntax` dans mes tests pour éviter les répétitions.

## Day 14

Très similaire à l'exercice 6. Il faut se rendre compte qu'on peut
modéliser l'exercice par un compteur de chaque paire existant dans
la chaine.

La partie 1 est ultra-facile, si on implémente l'algo tel que décrit
dans l'exercice et les exemples et ça tient. La partie 2 te met juste
face à l'explosion exponentielle de la chaine de caractère et t'oblige
à revoir ta structure de données.

Le truc c'est que chaque paire de caractère présente dans les règles
d'insertion est remplacée par deux nouvelles paires de caractère. Et
que toutes les paires de caractères identiques sont remplacées de
façon identique. Donc on a juste à faire un compteur de chaque paire
(par ex. sous forme d'une table d'association entre la paire et son
nombre d'occurence) pour incrémenter les compteurs facilement.

Je me suis perdu pour plusieurs raisons.

1. J'ai essayé de tout implémenter sous forme de listes et de
paires. Les structures de données paraissaient s'adapter comme ça
mais l'absence de type et l'imbrication des listes de paires de paires
m'a fait écrit tout un tas de bogues et à un moment tu ne sais plus
différencier un `cadar` de l'autre). Donc ce que j'ai dit sur le Day8
est encore valable, parfois il vaut mieux faire un `(define-record-type
<mypair>` à utiliser à la place d'une paire classique, juste pour
avoir des erreurs plus explicites.

2. J'ai utilisé du `assoc-set!` pour faire mine d'optimiser, de façon
très prématurée car ça m'a fait partager des structures de données
modifiables alors qu'elles étaient partagées. D'autant plus que

3. Comme l'algo me paraissait facile j'ai tardé avant de savoir comment
visualiser ce qui se passait. D'ailleurs mes visualisations ne sont
pas top. Ce qui a fini par me débloquer c'est de trouver un moyen de
comparer l'algo initial avec l'algo "optimisé" pour voir où étaient
les différences.

4. Pas de réflexion propre pour la deuxième partie. J'ai voulu enchainer
comme j'étais chaud sur la première partie alors que j'aurais dû
revenir au papier crayon pour prendre du recul.

5. Du coup mon plus gros bogue était un bogue de logique lié à la
façon dont je faisais évoluer mes compteurs. Le reste ne faisait que
rendre ce bogue plus compliqué à comprendre.

J'ai quand même trouvé une solution. Le code n'est pas très élégant
et mériterait une v2 mais il fonctionne.

```
export GUILE_LOAD_PATH=..:$GUILE_LOAD_PATH
# Tests unitaires (très partiels
guile day14/tests.scm
# Tests d'exemples
guile -e '(adventofcode2021 day14 polymers)' -s day14/polymers.scm <day14/inputs/example.txt
# Résultats
guile -e '(adventofcode2021 day14 polymers)' -s day14/polymers.scm <day14/inputs/input

# Résultats (deuxième version un peu nettoyée mais pas trop)
export GUILE_LOAD_PATH=..:$GUILE_LOAD_PATH
guile -e '(adventofcode2021 day14 polymers2)' -s day14/polymers2.scm <day14/inputs/input
```

## Day 15

De l'algorithmique brutale aujourd'hui.

J'ai commencé par un parcours en profondeur qui avait une explosion
combinatoire énorme et qui n'aurait jamais fini avant la fin des
temps. Mais ça m'avait quand même permis de passer le premier exercice
sur l'exemple, mais pas sur le véritable input qui était un peu
plus gros.

Alors j'ai essayé de recomprendre comment fonctionnait l'algorithme
de Dijkstra mais mes souvenirs de Fac sont très loin. Alors j'ai
implémenté ma version sous-optimale de cet algo et c'est ça qui tourne
en ce moment. Sauf bogue je devrais avoir un résultat dans deux heures
et demi si mon PC continue à tourner comme ça.

C'est très clairement sous-optimal mais bon on s'en contentera.

Après presque un peu plus d'une heure d'exécution mon résultat était
faux, ce qui m'a fortement déçu. Je ne comprenais car tout fonctionnait
sur les exemples. Et puis en relisant mon code je me suis rendu compte que
la génération de la carte five-time-as-large était boguée. J'avais mis
en dur dans le code la taille "10x10" de la carte exemple initiale. J'ai
corrigé et relancé. Reparti pour un peu plus d'une heure.

Et cette fois-ci j'ai le bon résultat.

En parallèle de l'exécution du code je me suis demandé comment je
pouvais optimiser. Alors j'ai fait quelques stats (avec `statprof`)
et comme attendu c'est la recherche de la case minimale à traiter qui
pose problème et même le nombre d'accès aux tableaux qui contiennent
les valeurs de "visited" et "current value".

### quelques amélioration de perfs

À prendre avec des pincettes, il peut y avoir des variations qui ne
sont pas dûes à l'algorithme mais simplement au fait que d'autres trucs
tournaient sur la machine. Pour voir les différentes versions de l'algo
il faut regarder l'historique git de ce repo.

Mon algo initial "Dijkstra sans PriorityQueue" où on parcours
l'intégralité de la liste des cases pour savoir laquelle est la case
minimale. (`commit b6a77f3870347c8fdd8bfde53a9518a41ea2a79a`)
```
3734,70s user 0,18s system 99% cpu 1:02:16,43 total | Mem: 26 kb max
```

Le même algo mais avec une optimisation pour limiter l'impact des
vérifications des noeuds déjà visités. On gratte 30% de conso CPU.
Le principe est de garder la trace sur chaque colonne du dernier noeud
non-visité. Ça permet de limiter le nombre de comparaisons à faire.
```
2639,97s user 0,14s system 99% cpu 44:04,63 total | Mem: 24 kb max
```

La deuxième optimisation consiste à faire un "early-exit" quand on
trouve la case `(0,0)` qui contient la valeur recherchée. Ça évite
de chercher les paths minimaux pour rien. Mais dans les faits ça ne me
fait rien gagner car on doit quasiment tout calculer quand même.

```
2770,94s user 0,17s system 99% cpu 46:15,23 total | Mem: 52 kb max
```

Quelques heures après je me suis décidé à remplacer le parcours de
mon tableau de distances par une liste dans laquelle j'ai uniquement les
cases pour lesquelles j'ai déjà calculé une distance. Cette liste de
priorité, bien que complètement sous-optimale est déjà tellement plus
efficace que le parcours intégral de toutes les cases possibles. On a
un rapport entre 200 et 300 par rapport au code précédant.  (`commit
6c8f62a6ba564e9b60e208eca475a96dabb9a740 day15: a lot more speed`)

```
9,04s user 0,06s system 126% cpu 7,214 total | Mem: 52 kb max
```

À ce stade j'ai bien envie de virer complètement la table des
distances calculées qui ne me sert à rien puisque à la fin
une seule valeur m'intéresse. Par contre en termes de perfs
c'est un peu moins bien. L'accès aléatoire à la distance d'un
voisin que permettait cette table était bien pratique.  (`commit
69055b836694d9c3541b594750e8b7138b057d98 day15: get rid of redundant
DISTANCES table`)

```
14,91s user 0,05s system 111% cpu 13,468 total | Mem: 54 kb max
```

### Pour aller plus loin

Les collègues ont galéré eux-aussi. Et le vrai gain en perf est
clairement lié à l'utilisation d'une PriorityQueue pour avoir accès
rapidement à la case minimale suivante sans avoir à la chercher partout
dans la liste des cases (ou pire dans le tableau de cases dans mon cas).

Quand elle est déjà implémentée nativement comme en
[Kotlin][Kotlin-PriorityQueue] ou dans un exemple de la doc comme en
[Go][Go-PriorityQueue] c'est déjà un peu plus facile, mais il faut
quand même comprendre l'[algorithmique][redblob-astar] en jeu.

[Kotlin-Implementation]: https://todd.ginsberg.com/post/advent-of-code/2021/day15/#d15p1
[Go-Implementation]: https://skarlso.github.io/2021/12/15/aoc-day15/
[redblob-astar]: https://www.redblobgames.com/pathfinding/a-star/introduction.html

Pour ma part j'ai souhaité me montrer prétentieux en n'utilisant pas
les implémentations [existant][jaymccarty-dijkstra] en Scheme.

[jaymccarty-dijkstra]: https://planet.racket-lang.org/package-source/jaymccarthy/dijkstra.plt/1/2/planet-docs/dijkstra/index.html

Le plus sympa dans tout ça c'est d'échanger avec les collègues et de
se rendre compte des différences d'approche entre les personnes et selon
les technos utilisées. Et puis de refaire un peu d'algorithmique une
fois de temps en temps ça fait du bien, car en vrai on empile souvent
des couches sans plus rien comprendre. Ce type d'exercice nous remet face
à des concepts de base qu'on a trop souvent laissés dans un tiroir en
sortant de l'école.

## Day 16

Après le retour à la science informatique hier on a aujourd'hui un
simple exercice de parsing.

Scheme est vraiment pratique pour ce genre d'exercice avec des fonctions
comme `apply` pour appliquer une opération à une liste d'éléments.

Bon côté perfs mon implémentation c'est complètement zéro. J'utilise
des listes d'entiers pour représenter des listes bits donc en termes
mémoire c'est cata et en terme CPU je fais des multiplications là où
je pourrais avoir des simples décalages et autres joyeusetés.

Mais c'était une journée pour se reposer je crois donc on verra un autre
jour pour réellement apprendre à gérer des données binaires en scheme.

## Day 17

J'ai tout d'abord abandonné en pensant au vu de la présentation de
la première partie que c'était un problème d'optimisation que je
n'arriverai pas à gérer.

En fait c'est un simple problème d'énumération. D'où la sensation de
"brute force", c'est parce-que dans ce cas je ne vois pas bien ce qu'on
peut faire d'autre que de la force brute. On peut réduire l'espace
à explorer mais pas complètement éliminer le besoin de simuler le
parcours du sous-marin pour plusieurs vitesses initiales.

Pour réduire l'espace à explorer j'ai pris en compte les éléments
suivant.

D'abord, les coordonnées en x et y sont complètement indépendantes
donc on peut calculer d'abord les vitesses initiales verticales (vy)
qui fonctionnent puis vérifier pour les vitesses initiales de vx.

L'aire ciblée est toujours devant nous (xmin et xmax positifs) et
en-dessous de nous (ymin et ymax négatifs).

Si vy est positif (vers le haut) la sonde finit toujours par retomber,
et quand elle repasse au point y=0 sa vitesse est l'opposé de sa
vitesse initiale.

Au-delà d'une certaine vitesse initiale, on "saute" systématiquement
par-dessus l'aire ciblée.

La sonde continue de tomber indéfiniment donc elle ne reste qu'un
certain temps dans la zone ciblée.

Du coup la stratégie est de trouver parmi les vy-init qui ne font
pas directement sauter par-dessus la zone cible ceux qui permettent
d'atteindre la cible et de noter à quelles étapes pour ces vy on est
dans la cible.

Une fois qu'on a les vy initiales possibles avec les étapes t
correspondant, on peut chercher parmi les vitesses horizontales initiales
vx celles-qui permettent d'être dans la zone cible aux étapes t ci-dessus.

Du coup ce n'est pas si compliqué mais j'ai fait beaucoup de maths pour
pas grand chose.

Nota: si pour y je simule chaque étape, pour x je me contente de
vérifier les coordonnées aux temps t déjà trouvé. La position x en
fonction du temps pour une vitesse initiale donnée est mathématiquement
facile à définir. Pour y aussi d'ailleurs mais à un moment j'ai besoin
d'énumérer toutes les étapes donc de toute façon je fais le calcul.

## Day 18

Quand j'ai vu l'énoncé, je me suis dit « les doigts dans le nez ». La
structure de données était idéalement représentée par des paires
`cons` en Scheme. Donc j'ai écrit `(define snailfish-add cons)` et j'ai
cru que ce serait bon.

Mais il y a cette histoire d'explosion et de "first regular number to the
left" et respectivement "to the right". Or ma structure à base de paire
se parcourt comme un arbre et en utilisant la récursion je ne sais pas
simplement ce qu'il y a sur les autres branches de l'arbre (à la limite
je peux tracker les parents, mais pas traverser horizontalement).

Je suis revenu plus tard en faisant une représentation linéaire
correspondant exactement à la représentation écrite, i.e. une liste
de marques d'ouverture (`[` que j'ai noté`'OPEN`) et de fermeture (`]`
que j'ai noté`'CLOSE`) et de nombres. Je ne suis pas sûr du tout que
ce soit le plus efficient mais ça a fonctionné.

Le truc dans pas mal de fonctions est de parcourir la liste d'élément en
étant capable de la modifier à droite et à gauche. Pour ça j'utilise
une double pile. Dans la pile `head` j'empile les éléments déjà
traités à gauche, si j'ai besoin de revenir en arrière je peux dépiler
un peu. Dans la pile `tail` j'ai le reste des éléments à traiter que
je dépile au fur et à mesure. Pour savoir si j'ai affaire à une paire
simple je peux faire un peak sur les quatre éléments au dessus de la
pile `tail` (en cas de paire j'ai `('OPEN NUM1 NUM2 'CLOSE ...)`).

Beaucoup de fonctions. Beaucoup de bogues aussi, dont pas mal
d'inattentions où j'utilise la mauvaise variable. Heureusement le jeu
d'exemples fourni par Eric Wastl se traduit très bien en tests ce qui
permet de détecter ces bogues assez vite.

## Day 19

Je me suis vraiment vraiment perdu sur celui-là. J'ai fin par trouver
la solution mais mon code est parti dans tous les sens et je ne suis pas
sûr d'avoir bien compris ce que j'ai fait. Pour m'en sortir j'ai fini
par poser comme postulat qu'il n'y avait jamais deux paires de beacons à
la même position relative l'un par rapport à l'autre. Statistiquement
ça doit être juste et dans mon cas en tout cas ça s'est avéré
fonctionner.

Je fais aussi deux fois chaque rotation car je n'ai pas réussi à
déterminer lesquelles étaient redondantes.

# Notes et liens
## Des adventofcode2021 intéressant

https://www.draketo.de/software/advent-of-wisp-code-2021.html

https://todd.ginsberg.com/post/advent-of-code/2021/

## Les collègues

https://github.com/julienvey/adventofcode2021/

https://github.com/NinoDLC/AdventOfCode/

https://github.com/rajivjhoomuck/aoc-2021

https://github.com/PierreFourreau/AdventOfCode2021

https://github.com/lhoang/AdventOfCode2021
