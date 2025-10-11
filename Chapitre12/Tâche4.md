### HC12T4 : Premiers 10 nombres de Fibonacci

```haskell
module Main where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  let fibonacciList = map fib [0..9] :: [Integer]
  putStrLn "Les 10 premiers nombres de Fibonacci sont :"
  print fibonacciList
```

### Explications détaillées

#### 1. Définition du module
```haskell
module Main where
```
- **Description** : Déclare le module `Main`, essentiel pour tout programme Haskell exécutable, servant de point d'entrée lors de la compilation avec GHC.
- **Mot-clé `where`** : Ouvre le corps du module où les définitions (fonctions et `main`) sont placées.
- **Rôle** : Structure le code et permet son exécution comme programme autonome.

#### 2. Définition de la fonction `fib`
```haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```
- **Signature** : `fib :: Integer -> Integer` indique que la fonction prend un index de type `Integer` et retourne un nombre de Fibonacci de type `Integer`.
- **Cas de base** :
  - `fib 0 = 0` : Le premier nombre de Fibonacci est 0.
  - `fib 1 = 1` : Le deuxième nombre de Fibonacci est 1.
- **Cas récursif** : `fib n = fib (n - 1) + fib (n - 2)` calcule chaque nombre de Fibonacci comme la somme des deux précédents, suivant la définition mathématique de la séquence (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...).
- **Exemple de calcul** :
  - `fib 2` → `fib 1 + fib 0` → `1 + 0` = `1`.
  - `fib 3` → `fib 2 + fib 1` → `1 + 1` = `2`.
  - `fib 4` → `fib 3 + fib 2` → `2 + 1` = `3`.
  - Jusqu'à `fib 9` → `34`.
- **Rôle** : Implémente la séquence de Fibonacci de manière récursive, répondant à l'objectif de calculer les 10 premiers nombres.

#### 3. Fonction `main`
```haskell
main :: IO ()
main = do
  let fibonacciList = map fib [0..9] :: [Integer]
  putStrLn "Les 10 premiers nombres de Fibonacci sont :"
  print fibonacciList
```
- **Description** : Point d'entrée du programme, utilisant `do` pour séquencer les actions d'affichage.
- **Variables locales avec `let`** :
  - `fibonacciList = map fib [0..9] :: [Integer]` : Utilise `map` pour appliquer `fib` à la liste `[0..9]` (indices de 0 à 9), générant les 10 premiers nombres de Fibonacci. Le type `Integer` est explicitement indiqué pour éviter des ambiguïtés.
- **`putStrLn`** : Affiche un message descriptif suivi d'un retour à la ligne.
- **`print`** : Affiche la liste `fibonacciList` directement, convertissant automatiquement les `Integer` en chaînes via `Show`.
- **Sortie attendue** : 
  ```
  Les 10 premiers nombres de Fibonacci sont :
  [0,1,1,2,3,5,8,13,21,34]
  ```
- **Rôle** : Teste la fonction `fib` pour les 10 premiers nombres et les affiche, répondant à l'exigence de l'exercice.

### Résumé
- **Module `Main`** : Fournit le module principal pour l'exécution.
- **Fonction `fib`** : Calcule le n-ième nombre de Fibonacci via récursion, avec `fib 0 = 0`, `fib 1 = 1`, et `fib n = fib (n-1) + fib (n-2)` pour `n > 1`.
- **Fonction `main`** : Utilise `map` avec `[0..9]` pour générer les 10 premiers nombres via `fib`, et les affiche avec `putStrLn` et `print`.
- **Code** : Complet, compilable avec `ghc`, et exécutable pour démontrer la séquence de Fibonacci.
