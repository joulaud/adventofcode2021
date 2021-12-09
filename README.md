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

