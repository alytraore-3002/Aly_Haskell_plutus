HC12T1 : Afficher un message de bienvenue

```haskell
module Main where

main :: IO ()
main = putStrLn "Bienvenue dans la programmation Haskell !"
```

### Explications détaillées

#### 1. Définition du module
```haskell
module Main where
```
- **Description** : Tout programme Haskell exécutable doit commencer par une déclaration de module. Ici, `Main` est le nom du module par défaut pour les programmes principaux, car c'est le point d'entrée que GHC recherche lors de la compilation.
- **Mot-clé `where`** : Indique le début du corps du module, où les définitions (fonctions, types, etc.) sont placées.
- **Rôle** : Organise le code et permet l'importation dans d'autres modules si nécessaire. Pour un programme simple, `Main` est suffisant et obligatoire pour l'exécution.

#### 2. Fonction `main`
```haskell
main :: IO ()
main = putStrLn "Bienvenue dans la programmation Haskell !"
```
- **Description** : La fonction `main` est le point d'entrée obligatoire de tout programme Haskell exécutable. Elle est de type `IO ()`, signifiant qu'elle effectue des opérations d'entrée/sortie et ne retourne aucune valeur utile (le `()` représente le type "unit", similaire à `void` dans d'autres langages).
- **`putStrLn`** : Fonction de la bibliothèque standard (Prelude) qui prend une chaîne de caractères (`String`) et l'affiche sur la sortie standard (terminal) suivie d'un retour à la ligne (`\n`).
- **Chaîne de caractères** : La littérale `"Bienvenue dans la programmation Haskell !"` est une `String` qui sera imprimée telle quelle.
- **Rôle** : Exécute l'action IO pour afficher le message demandé, respectant l'objectif de l'exercice (afficher un message de bienvenue dans le terminal).

### Résumé
- **Module `Main`** : Définit le module principal pour l'exécution du programme.
- **Fonction `main`** : Point d'entrée qui utilise `putStrLn` pour imprimer le message "Bienvenue dans la programmation Haskell !" avec un retour à la ligne.
- **Code** : Simple et complet pour un programme de base, compilable avec `ghc` et exécutable directement.
