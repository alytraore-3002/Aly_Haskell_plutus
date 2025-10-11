HC11T9 : Type Longueur avec unités

```haskell
module Main where

-- Définition du nouveau type Length
data Length = Met Double | Km Double deriving (Show)

-- Dérivation de l'instance Eq pour Length
instance Eq Length where
  (Met m1) == (Met m2) = m1 == m2
  (Km k1) == (Km k2) = k1 == k2
  (Met m1) == (Km k1) = m1 == k1 * 1000  -- Conversion : 1 km = 1000 m
  (Km k1) == (Met m1) = k1 * 1000 == m1  -- Conversion inverse

-- Fonction main pour tester
main :: IO ()
main = do
  let len1 = Met 1000.0
      len2 = Km 1.0
      len3 = Met 500.0
      len4 = Km 2.0

  putStrLn "Test de == (égalité) :"
  print $ len1 == len2  -- True (1000 m = 1 km)
  print $ len1 == len3  -- False (1000 m != 500 m)
  print $ len2 == len4  -- False (1 km != 2 km)
  print $ len3 == len4  -- False (500 m != 2000 m, car 2 km = 2000 m)
```

### Explications détaillées

#### 1. Définition du type `Length`
```haskell
data Length = Met Double | Km Double deriving (Show)
```
- **Description** : `Length` est un type algébrique avec deux constructeurs :
  - `Met Double` : Représente une longueur en mètres (e.g., `Met 1000.0`).
  - `Km Double` : Représente une longueur en kilomètres (e.g., `Km 1.0`).
- **Dérivation** :
  - `Show` : Permet d'afficher les valeurs (e.g., `Met 1000.0`, `Km 1.0`).
- **Rôle** : Modélise des longueurs avec deux unités différentes pour tester les comparaisons personnalisées dans les instances `Eq`.

#### 2. Instance de `Eq` pour `Length`
```haskell
instance Eq Length where
  (Met m1) == (Met m2) = m1 == m2
  (Km k1) == (Km k2) = k1 == k2
  (Met m1) == (Km k1) = m1 == k1 * 1000  -- Conversion : 1 km = 1000 m
  (Km k1) == (Met m1) = k1 * 1000 == m1  -- Conversion inverse
```
- **Description** : Définit l'égalité entre deux `Length` en tenant compte de la conversion entre mètres et kilomètres. Comme `Eq` ne peut être dérivé automatiquement pour gérer des unités différentes, nous corrigeons manuellement les comparaisons.
- **Fonctionnement** :
  - **Cas intra-unité** :
    - `(Met m1) == (Met m2)` : Compare directement les valeurs en mètres (`m1 == m2`).
    - `(Km k1) == (Km k2)` : Compare directement les valeurs en kilomètres (`k1 == k2`).
  - **Cas inter-unités** :
    - `(Met m1) == (Km k1)` : Convertit `k1` en mètres (`k1 * 1000`) et compare avec `m1`.
    - `(Km k1) == (Met m1)` : Convertit `k1` en mètres (`k1 * 1000`) et compare avec `m1`.
  - Les conversions sont basées sur le facteur 1000 (1 km = 1000 m), une approximation standard pour simplifier.
- **Exemples** :
  - `Met 1000.0 == Km 1.0` → `True` (car `1000.0 == 1.0 * 1000`).
  - `Met 500.0 == Km 1.0` → `False` (car `500.0 != 1.0 * 1000`).
  - `Km 2.0 == Met 2000.0` → `True` (car `2.0 * 1000 == 2000.0`).
- **Note** : Cette implémentation suppose une précision exacte des `Double`. En pratique, des erreurs d'arrondi pourraient nécessiter une tolérance (e.g., `abs (m1 - k1 * 1000) < epsilon`), mais ici, on suit une égalité stricte pour respecter l'exercice.

### Résumé
- **Type `Length`** : `data Length = Met Double | Km Double`, représente des longueurs en mètres ou kilomètres.
- **Instance `Eq` pour `Length`** : Implémentée manuellement pour comparer :
  - Les valeurs directement si les unités sont identiques.
  - Les valeurs converties (1 km = 1000 m) si les unités diffèrent.
- **Fonction `main`** : Teste `==` sur des `Length` avec des cas d'égalité et d'inégalité (mètres vs kilomètres), affichant les résultats via IO.
- **Code** : Complet, testable, et respecte les concepts du cours (pureté, typeclasses, IO).
