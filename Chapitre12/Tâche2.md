### HC12T2 : Additionner deux nombres

```haskell
module Main where

main :: IO ()
main = do
  let num1 = 5 :: Int
      num2 = 3 :: Int
      sumResult = num1 + num2
  putStrLn $ "La somme de " ++ show num1 ++ " et " ++ show num2 ++ " est : " ++ show sumResult
```

### Explications détaillées

#### 1. Définition du module
```haskell
module Main where
```
- **Description** : Déclare le module `Main`, requis pour tout programme Haskell exécutable, servant de point d'entrée lors de la compilation avec GHC.
- **Mot-clé `where`** : Ouvre le corps du module où les définitions sont placées.
- **Rôle** : Structure le code et permet une organisation claire, même pour un programme simple.

#### 2. Fonction `main`
```haskell
main :: IO ()
main = do
  let num1 = 5 :: Int
      num2 = 3 :: Int
      sumResult = num1 + num2
  putStrLn $ "La somme de " ++ show num1 ++ " et " ++ show num2 ++ " est : " ++ show sumResult
```
- **Description** : La fonction `main` est l'entrée du programme, de type `IO ()`, indiquant une action d'entrée/sortie. Elle utilise la notation `do` pour séquencer les opérations.
- **Variables locales avec `let`** :
  - `num1 = 5 :: Int` : Définit un entier `num1` avec la valeur 5, typé explicitement comme `Int` pour clarifier.
  - `num2 = 3 :: Int` : Définit un entier `num2` avec la valeur 3, également typé comme `Int`.
  - `sumResult = num1 + num2` : Calcule la somme des deux nombres en utilisant l'opérateur `+`, qui est défini pour les types numériques comme `Int`.
- **`putStrLn`** : Affiche une chaîne formatée sur le terminal avec un retour à la ligne.
  - **Concaténation avec `++`** : Combine des chaînes et des nombres convertis.
  - **`show`** : Convertit les valeurs `Int` (e.g., `num1`, `sumResult`) en `String` pour pouvoir les inclure dans la chaîne affichée.
  - **Message** : Construit dynamiquement une phrase comme "La somme de 5 et 3 est : 8".
- **Rôle** : Répond à l'objectif de l'exercice en additionnant deux nombres (5 et 3) et en affichant le résultat (8) de manière lisible.

### Résumé
- **Module `Main`** : Fournit le module principal pour l'exécution.
- **Fonction `main`** : Utilise `do` et `let` pour définir `num1` et `num2`, calcule leur somme avec `+`, et affiche le résultat via `putStrLn` avec `show` pour la conversion en chaîne.
- **Code** : Complet, compilable avec `ghc`, et exécutable pour démontrer l'addition de deux nombres.
