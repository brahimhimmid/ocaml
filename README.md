

# Devoir de programmation Ocaml

## Mode d'emploi de l'outil développé

Le but de ce projet est de concevoir une version simplifiée d’un automate d’états finis (AEF) connu pour être l’outil de reconnaissance des langages réguliers. Le projet est organisé en trois parties (Quelques fonctions sur les AEF,Reconnaissance d’un mot par un AEF déterministe et Opérations sur les AEF), chacune comprenant un ensemble de fonctions et des tests associés en dessous de chaque partie. Le fichier `projet.ml` contient le code source de l'outil.

## Utilisation de l'outil

Pour utiliser cet outil, ouvrez le fichier `projet.ml` dans un interpréteur OCaml ou un environnement de développement OCaml et exécutez les fonctions disponibles pour manipuler les automates.Vous pouvez definir votre propre automate d’états finis en utilisant la structure de donneé  de type `aef` est composé des éléments suivants :

- `alphabets` : une liste de caractères représentant l'alphabet de l'automate (de type `char list`)
- `etats` : une liste d'états représentant tous les états possibles de l'automate (de type `etat list`)
- `q0` : un état représentant l'état initial de l'automate (de type `etat`)
- `fetats` : une liste d'états représentant les états finaux de l'automate (de type `etat list`)
- `delta` : une liste de transitions représentant les règles de transition entre les états de l'automate (de type `ftransition`)

## Utilisation du type `aef`

Pour utiliser le type `aef`, vous pouvez créer des instances d'automates finis en définissant les éléments mentionnés ci-dessus. Par exemple, pour créer un automate fini simple :

let aef_exemple = {
  alphabets = ['a'; 'b'];
  etats = [0; 1; 2];
  q0 = 0;
  fetats = [2];
  delta = [(0, 'a', 1); (1, 'b', 2)]
}


## Répartition du travail

Le travail est réparti comme suit :

La première question (définition d’un AEF), la sixième question (module
Deterministe) et la deuxième partie (Reconnaissance d’un mot par un AEF
déterministe) du projet ont été gérées par `DIAO Mouhamadou` et le reste du travail à
savoir les première et troisième parties (questions 2 à 5 et 10 à 12) a été réalisé par
`HIMMID Brahim`.




## Fonctions et modules

Les descriptions des types de données, des structures et des fonctions sont fournies au format ocamldoc dans les fichiers sources. Vous pouvez générer la documentation en utilisant la commande `ocamldoc` avec l'option `-html` ou `-latex`, selon le format souhaité. Les descriptions détaillées de chaque type et fonction sont disponibles dans la documentation générée.

### Des descriptions brèves de modules
(Les fonctions clés de chaque module sont mentionnées entre parenthèses pour donner un aperçu de ce que chaque module propose, pour une définition plus détaillée.Vous pouvez générer la documentation `ocamldoc`.)

- Le module  `Correct` :contient les fonctions relatives pour vérifier si un automate est correcte (est_vide , est_present, est_inclus , est_etat_utilise,tous_etats_utilises, est_correct).

- Le module  `Complet` : possède les fonctions relatives pour vérifier si un AEF est complet (transition_definie , toutes_transition_definies , est_complet).

- Le module  `Completer_aef` : contient les fonctions qui complètent un automate (etat_maximal, generer_transitions_manquantes,
   completer).  
- Le module  `Deterministe` : regroupe les fonctions permettant de déterminer si un automate est déterministe
          (compter_transitions , est_deterministe ).
     
- Le module  `ReconnaissanceMotAef` : contient les fonctions qui manipulent la reconnaissance d’un mot par un AEF déterministe
          ( lecture_car , lecture_mot, accepter_mot).
- Le module  `OperationAef` : englobe les fonctions qui effectuent les opérations sur les AEF
          (  union_aef , concatener_aef,  afficher).
      



```python

```
