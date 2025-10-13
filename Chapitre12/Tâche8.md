### HC12T8 : Fusionner deux listes triées

```haskell
module Main where

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

main :: IO ()
main = do
  let list1 = [1, 3, 5] :: [Int]
      list2 = [2, 4, 6] :: [Int]
      merged = mergeLists list1 list2
  putStrLn "Première liste triée :"
  print list1
  putStrLn "Deuxième liste triée :"
  print list2
  putStrLn "Liste fusionnée et triée :"
  print merged
```

### Explications détaillées

#### 1. Définition du module
```haskell
module Main where
```
- **Description** : Déclare le module `Main`, requis pour un programme exécutable en Haskell, servant de point d'entrée lors de la compilation avec GHC.
- **Mot-clé `where`** : Ouvre le corps du module pour y placer les définitions de fonctions et la fonction `main`.
- **Rôle** : Structure le code et permet son exécution comme programme autonome.

#### 2. Définition de la fonction `mergeLists`
```haskell
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys
```
- **Signature** : `mergeLists :: Ord a => [a] -> [a] -> [a]` indique que la fonction prend deux listes d'éléments de type `a` (où `a` doit être ordonnable via la contrainte `Ord a`) et retourne une liste triée de `a`.
- **Cas de base** :
  - `mergeLists xs [] = xs` : Si la deuxième liste est vide, retourne la première liste.
  - `mergeLists [] ys = ys` : Si la première liste est vide, retourne la deuxième liste.
- **Cas récursif** :
  - `(x:xs) (y:ys)` : Décompose les têtes (`x`, `y`) et les queues (`xs`, `ys`) des deux listes.
  - `| x <= y = x : mergeLists xs (y:ys)` : Si `x` est inférieur ou égal à `y`, prend `x` comme premier élément et fusionne le reste.
  - `| otherwise = y : mergeLists (x:xs) ys` : Sinon, prend `y` comme premier élément et continue avec `x:xs`.
- **Exemple de fonctionnement** :
  - `mergeLists [1, 3, 5] [2, 4, 6]` :
    - `1 <= 2`, prend `1`, appelle `mergeLists [3, 5] [2, 4, 6]`.
    - `3 > 2`, prend `2`, appelle `mergeLists [1, 3, 5] [4, 6]`.
    - `1 <= 4`, prend `1`, appelle `mergeLists [3, 5] [4, 6]`.
    - Continue jusqu'à `[1, 2, 3, 4, 5, 6]`.
- **Rôle** : Fusionne deux listes triées en une seule liste triée de manière efficace via récursion, répondant à l'objectif de l'exercice.

#### 3. Fonction `main`
```haskell
main :: IO ()
main = do
  let list1 = [1, 3, 5] :: [Int]
      list2 = [2, 4, 6] :: [Int]
      merged = mergeLists list1 list2
  putStrLn "Première liste triée :"
  print list1
  putStrLn "Deuxième liste triée :"
  print list2
  putStrLn "Liste fusionnée et triée :"
  print merged
```
- **Description** : Point d'entrée du programme, utilisant `do` pour séquencer les actions d'affichage.
- **Variables locales avec `let`** :
  - `list1 = [1, 3, 5] :: [Int]` : Définit une liste triée d'entiers comme exemple.
  - `list2 = [2, 4, 6] :: [Int]` : Définit une autre liste triée d'entiers.
  - `merged = mergeLists list1 list2` : Fusionne les deux listes en une seule liste triée.
- **`putStrLn`** : Affiche des messages descriptifs avec un retour à la ligne.
- **`print`** : Affiche les listes directement, convertissant automatiquement les `Int` en chaînes via `Show`.
- **Sortie attendue** :
  ```
  Première liste triée :
  [1,3,5]
  Deuxième liste triée :
  [2,4,6]
  Liste fusionnée et triée :
  [1,2,3,4,5,6]
  ```
- **Rôle** : Teste la fonction `mergeLists` avec des exemples fixes et affiche les résultats, répondant à l'exigence de démonstration via `main`.

### Résumé
- **Module `Main`** : Fournit le module principal pour l'exécution.
- **Fonction `mergeLists`** : Fusionne deux listes triées de type `a` (avec contrainte `Ord a`) en une liste triée via récursion, gérant les cas vides et comparant les éléments.
- **Fonction `main`** : Définit deux listes triées `[1, 3, 5]` et `[2, 4, 6]`, les fusionne avec `mergeLists`, et affiche les listes avec `putStrLn` et `print`.
- **Code** : Complet, compilable avec `ghc`, et exécutable pour démontrer la fusion de listes triées.
