### HC12T3 : Fonction factorielle

```haskell
module Main where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  let num = 5 :: Integer
      result = factorial num
  putStrLn $ "La factorielle de " ++ show num ++ " est : " ++ show result
```

### Explications détaillées

#### 1. Définition du module
```haskell
module Main where
```
- **Description** : Déclare le module `Main`, requis pour un programme exécutable en Haskell, servant de point d'entrée lors de la compilation avec GHC.
- **Mot-clé `where`** : Ouvre le corps du module pour y placer les définitions de fonctions et de la fonction `main`.
- **Rôle** : Organise le code et facilite son exécution comme programme autonome.

#### 2. Définition de la fonction `factorial`
```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```
- **Signature** : `factorial :: Integer -> Integer` indique que la fonction prend un argument de type `Integer` (entier avec une plage large) et retourne un `Integer`.
- **Cas de base** : `factorial 0 = 1` définit la factorielle de 0 comme 1, conformément à la définition mathématique (0! = 1).
- **Cas récursif** : `factorial n = n * factorial (n - 1)` calcule la factorielle en multipliant `n` par la factorielle de `n-1`, s'appliquant aux entiers positifs.
- **Exemple de calcul** :
  - `factorial 5` → `5 * factorial 4`
  - → `5 * 4 * factorial 3`
  - → `5 * 4 * 3 * factorial 2`
  - → `5 * 4 * 3 * 2 * factorial 1`
  - → `5 * 4 * 3 * 2 * 1 * factorial 0`
  - → `5 * 4 * 3 * 2 * 1 * 1` = `120`.
- **Rôle** : Implémente la factorielle récursive, répondant à l'objectif de calculer la factorielle d'un entier positif donné.

#### 3. Fonction `main`
```haskell
main :: IO ()
main = do
  let num = 5 :: Integer
      result = factorial num
  putStrLn $ "La factorielle de " ++ show num ++ " est : " ++ show result
```
- **Description** : Point d'entrée du programme, utilisant `do` pour séquencer les actions.
- **Variables locales avec `let`** :
  - `num = 5 :: Integer` : Définit un entier positif à tester, typé explicitement comme `Integer` pour éviter les débordements avec de grandes valeurs.
  - `result = factorial num` : Calcule la factorielle de `num` en appelant la fonction définie.
- **`putStrLn`** : Affiche une chaîne formatée avec le résultat.
  - **`show`** : Convertit `num` et `result` (types `Integer`) en `String` pour l'affichage.
  - **Message** : Construit une phrase comme "La factorielle de 5 est : 120".
- **Rôle** : Teste la fonction `factorial` avec une valeur fixe et affiche le résultat, respectant l'exigence de démonstration via `main`.

### Résumé
- **Module `Main`** : Fournit le module principal pour l'exécution du programme.
- **Fonction `factorial`** : Calcule la factorielle d'un `Integer` via récursion, avec `0! = 1` et `n! = n * (n-1)!` pour `n > 0`.
- **Fonction `main`** : Utilise `do` et `let` pour tester `factorial` avec `5`, affichant le résultat via `putStrLn` avec `show` pour la conversion.
- **Code** : Complet, compilable avec `ghc`, et exécutable pour démontrer le calcul de la factorielle.
