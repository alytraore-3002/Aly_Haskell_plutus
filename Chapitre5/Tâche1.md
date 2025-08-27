HC5T1 : Utiliser applyTwice
### Code Haskell

```haskell
-- Fonction donnée applyTwice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Fonction applyThreeTimes utilisant applyTwice
applyThreeTimes :: (Int -> Int) -> Int -> Int
applyThreeTimes f x = f (applyTwice f x)

-- Fonction main pour tester applyThreeTimes
main :: IO ()
main = do
  let double x = x * 2  -- Fonction pour doubler
  let inc x = x + 1     -- Fonction pour incrémenter
  let square x = x * x  -- Fonction pour mettre au carré
  print $ applyThreeTimes double 3   -- Résultat attendu : 24
  print $ applyThreeTimes inc 5      -- Résultat attendu : 8
  print $ applyThreeTimes square 2   -- Résultat attendu : 256
```

### Explications détaillées

1. **Rôle de `applyTwice`** :
   - La fonction donnée `applyTwice :: (a -> a) -> a -> a` prend une fonction `f :: a -> a` et une valeur `x :: a`, et applique `f` deux fois à `x`, c’est-à-dire `f (f x)`.
   - Par exemple, si `f` est `double x = x * 2` et `x = 3` :
     - `applyTwice double 3 = double (double 3) = double 6 = 12`.
   - Cette fonction est polymorphe (type `a`), mais dans notre cas, nous l’utiliserons avec des entiers (`a = Int`).

2. **Définition de `applyThreeTimes`** :
   - La fonction `applyThreeTimes :: (Int -> Int) -> Int -> Int` prend :
     - Une fonction `f :: Int -> Int` qui transforme un entier en un autre entier.
     - Un entier `x :: Int` auquel appliquer `f` trois fois.
     - Retourne un entier, le résultat de `f (f (f x))`.
   - L’implémentation `applyThreeTimes f x = f (applyTwice f x)` utilise `applyTwice` :
     - `applyTwice f x` applique `f` deux fois, produisant `f (f x)`.
     - On applique ensuite `f` une troisième fois en passant `applyTwice f x` à `f`, soit `f (f (f x))`.
   - Exemple avec `f = double` et `x = 3` :
     - `applyTwice double 3 = double (double 3) = double 6 = 12`.
     - `applyThreeTimes double 3 = double (applyTwice double 3) = double 12 = 24`.

3. **Pourquoi utiliser `applyTwice`** :
   - En utilisant `applyTwice`, on réutilise une fonction existante qui applique déjà `f` deux fois, réduisant le code nécessaire pour appliquer `f` trois fois.
   - Cela rend l’implémentation plus modulaire et concise par rapport à une définition directe comme `f (f (f x))`.

4. **Fonction `main`** :
   - La fonction `main :: IO ()` teste `applyThreeTimes` avec trois fonctions différentes :
     - `double x = x * 2` :
       - `applyThreeTimes double 3` :
         - `applyTwice double 3 = double (double 3) = double 6 = 12`.
         - `double 12 = 24`.
         - Résultat : `24`.
     - `inc x = x + 1` :
       - `applyThreeTimes inc 5` :
         - `applyTwice inc 5 = inc (inc 5) = inc 6 = 7`.
         - `inc 7 = 8`.
         - Résultat : `8`.
     - `square x = x * x` :
       - `applyThreeTimes square 2` :
         - `applyTwice square 2 = square (square 2) = square 4 = 16`.
         - `square 16 = 256`.
         - Résultat : `256`.
   - Les résultats sont affichés avec `print`.

5. **Types et polymorphisme** :
   - Bien que `applyTwice` soit polymorphe (`a -> a`), `applyThreeTimes` est spécialisée pour `Int -> Int` pour respecter la contrainte de travailler avec un entier.
   - La composition de `applyTwice` et `f` est cohérente, car `applyTwice f x :: Int` (après deux applications de `f`) est un argument valide pour une troisième application de `f :: Int -> Int`.

6. **Approche fonctionnelle** :
   - L’utilisation de `applyTwice` illustre le style fonctionnel de Haskell, où les fonctions sont composées pour construire des comportements complexes à partir de blocs simples.
   - L’évaluation paresseuse garantit que les applications de `f` ne sont calculées que lorsque nécessaires (pour l’affichage via `print`).
   - La fonction est déclarative, exprimant l’intention d’appliquer `f` trois fois sans boucles explicites.

7. **Avantages de l’implémentation** :
   - En réutilisant `applyTwice`, le code est plus modulaire et évite de répéter la logique d’application multiple.
   - La fonction est flexible, car elle peut travailler avec n’importe quelle fonction `f :: Int -> Int`, comme `double`, `inc`, ou `square`.

### Résumé des explications

La fonction `applyThreeTimes :: (Int -> Int) -> Int -> Int` applique une fonction `f` trois fois à un entier `x` en utilisant `applyTwice f x = f (f x)` pour les deux premières applications, puis applique `f` une troisième fois, soit `f (applyTwice f x)`. Dans `main`, elle est testée avec `double x = x * 2` (résultat `24` pour `x=3`), `inc x = x + 1` (résultat `8` pour `x=5`), et `square x = x * x` (résultat `256` pour `x=2`), affichés avec `print`. Cette approche fonctionnelle est concise, modulaire grâce à `applyTwice`, et exploite la curryfication et l’évaluation paresseuse pour une solution claire et efficace.
