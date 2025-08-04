HC2T5 - Tâche 5 : définir et utiliser des fonctions

### Fonction `circleArea`

- **Description** : Calcule l'aire d'un cercle à partir de son rayon (de type `Float`). La formule de l'aire est \( \pi \times r^2 \).
- **Signature de type** : `circleArea :: Float -> Float`
  - Prend un `Float` (le rayon) et retourne un `Float` (l'aire).
- **Implémentation** :
  ```haskell
  circleArea :: Float -> Float
  circleArea radius = pi * radius * radius
  ```
  - Explication : Utilise la constante `pi` (disponible dans le module de base de Haskell) et multiplie par le carré du rayon.

### Fonction `maxOfThree`

- **Description** : Prend trois `Int` et retourne le plus grand des trois.
- **Signature de type** : `maxOfThree :: Int -> Int -> Int -> Int`
  - Prend trois arguments de type `Int` et retourne un `Int`.
- **Implémentation** :
  ```haskell
  maxOfThree :: Int -> Int -> Int -> Int
  maxOfThree x y z = max x (max y z)
  ```
  - Explication : Utilise la fonction `max` (définie dans le module de base) pour comparer deux nombres à la fois. D'abord, `max y z` trouve le maximum entre `y` et `z`, puis `max x ...` compare ce résultat avec `x`.

### Code complet avec tests

Voici le code Haskell complet, incluant une fonction `main` qui teste `circleArea` et `maxOfThree` avec différentes entrées :

```haskell
circleArea :: Float -> Float
circleArea radius = pi * radius * radius

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

main :: IO ()
main = do
    putStrLn "Tests de la fonction circleArea :"
    print (circleArea 1.0)  -- Aire pour rayon = 1.0
    print (circleArea 2.5)  -- Aire pour rayon = 2.5
    print (circleArea 0.0)  -- Aire pour rayon = 0.0
    putStrLn "Tests de la fonction maxOfThree :"
    print (maxOfThree 1 2 3)    -- Maximum de 1, 2, 3
    print (maxOfThree 5 5 2)    -- Maximum de 5, 5, 2
    print (maxOfThree 10 3 7)   -- Maximum de 10, 3, 7
    print (maxOfThree (-1) 0 1) -- Maximum de -1, 0, 1
```

### Explications des tests

- **Tests pour `circleArea`** :
  - `circleArea 1.0` : Rayon = 1.0, aire = \( \pi \times 1^2 = \pi \approx 3.14159 \).
  - `circleArea 2.5` : Rayon = 2.5, aire = \( \pi \times 2.5^2 = \pi \times 6.25 \approx 19.63495 \).
  - `circleArea 0.0` : Rayon = 0.0, aire = \( \pi \times 0^2 = 0 \).

- **Tests pour `maxOfThree`** :
  - `maxOfThree 1 2 3` : Retourne `3` (le plus grand).
  - `maxOfThree 5 5 2` : Retourne `5` (le plus grand, même si répété).
  - `maxOfThree 10 3 7` : Retourne `10` (le plus grand).
  - `maxOfThree (-1) 0 1` : Retourne `1` (le plus grand, incluant des nombres négatifs).

### Résultats attendus

```
Tests de la fonction circleArea :
3.1415927
19.634954
0.0
Tests de la fonction maxOfThree :
3
5
10
1
```
