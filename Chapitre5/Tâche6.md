HC5T6 : Composition de fonctions

### Code Haskell

```haskell
-- Fonction pour vérifier si un nombre est pair
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- Fonction pour calculer le carré d'un nombre
square :: Int -> Int
square x = x * x

-- Fonction main pour tester la composition
main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5]  -- Liste de test
  let result = map (isEven . square) numbers  -- Composition pour filtrer les carrés pairs
  print result  -- Afficher les résultats booléens
  let evenSquares = filter isEven (map square numbers)  -- Résultat des carrés pairs
  print evenSquares  -- Afficher les carrés pairs
```

### Explications détaillées

1. **Rôle de l’opérateur de composition `(.)`** :
   - L’opérateur `(.)` en Haskell compose deux fonctions : `(f . g) x = f (g x)`. Cela signifie que la fonction `g` est appliquée à l’entrée `x`, puis la fonction `f` est appliquée au résultat de `g`.
   - Sa signature est `(.) :: (b -> c) -> (a -> b) -> a -> c`, où `f :: b -> c` et `g :: a -> b` produisent une nouvelle fonction de type `a -> c`.
   - Dans ce contexte, nous allons utiliser `(.)` pour composer des fonctions qui transforment une liste en ses carrés, puis filtrent les nombres pairs.

2. **Fonctions auxiliaires** :
   - `isEven :: Int -> Bool` vérifie si un nombre est pair en testant si son reste modulo 2 est 0 (`x `mod` 2 == 0`).
     - Exemple : `isEven 4 = True`, `isEven 9 = False`.
   - `square :: Int -> Int` calcule le carré d’un nombre (`x * x`).
     - Exemple : `square 3 = 9`, `square 4 = 16`.

3. **Composition avec `(.)`** :
   - Pour chaque élément `x` d’une liste, nous voulons calculer son carré (`square x`) et vérifier si ce résultat est pair (`isEven (square x)`).
   - La composition `isEven . square` crée une fonction de type `Int -> Bool` :
     - `square :: Int -> Int` prend un entier et retourne son carré.
     - `isEven :: Int -> Bool` prend un entier et retourne un booléen.
     - `(isEven . square) x = isEven (square x)` vérifie si le carré de `x` est pair.
     - Exemple : `(isEven . square) 3 = isEven (square 3) = isEven 9 = False`, `(isEven . square) 4 = isEven (square 4) = isEven 16 = True`.

4. **Application à une liste** :
   - Pour appliquer `isEven . square` à une liste, nous utilisons `map :: (a -> b) -> [a] -> [b]` pour appliquer la fonction composée à chaque élément.
   - Dans `map (isEven . square) [1, 2, 3, 4, 5]` :
     - `1`: `square 1 = 1`, `isEven 1 = False`.
     - `2`: `square 2 = 4`, `isEven 4 = True`.
     - `3`: `square 3 = 9`, `isEven 9 = False`.
     - `4`: `square 4 = 16`, `isEven 16 = True`.
     - `5`: `square 5 = 25`, `isEven 25 = False`.
     - Résultat : `[False, True, False, True, False]`.

5. **Obtention des carrés pairs** :
   - Pour obtenir les carrés pairs eux-mêmes (et non un résultat booléen), nous devons d’abord calculer les carrés avec `map square`, puis filtrer les résultats pairs avec `filter isEven`.
   - Dans `filter isEven (map square numbers)` :
     - `map square [1, 2, 3, 4, 5] = [1, 4, 9, 16, 25]`.
     - `filter isEven [1, 4, 9, 16, 25] = [4, 16]`, car seuls `4` et `16` sont pairs.
   - Cela donne les carrés des nombres de la liste qui sont pairs.

6. **Fonction `main`** :
   - La fonction `main :: IO ()` teste la composition sur la liste `[1, 2, 3, 4, 5]`.
   - `map (isEven . square) numbers` produit une liste de booléens `[False, True, False, True, False]`, indiquant quels carrés sont pairs.
   - `filter isEven (map square numbers)` produit la liste des carrés pairs `[4, 16]`.
   - Les deux résultats sont affichés avec `print` pour montrer à la fois la composition et le résultat final.

7. **Approche fonctionnelle** :
   - L’utilisation de `(.)` illustre la composition fonctionnelle, une caractéristique clé de Haskell, permettant de combiner des fonctions de manière concise.
   - `map` et `filter` sont des fonctions d’ordre supérieur qui appliquent des transformations et des filtres de manière déclarative, sans boucles explicites.
   - L’évaluation paresseuse de Haskell garantit que les calculs ne sont effectués que lorsque nécessaire (ici, pour l’affichage avec `print`).

### Résumé des explications

La fonction composée `isEven . square` utilise l’opérateur `(.)` pour vérifier si le carré d’un nombre est pair. Appliquée via `map (isEven . square)` à la liste `[1, 2, 3, 4, 5]`, elle produit `[False, True, False, True, False]`, indiquant quels carrés sont pairs. Pour obtenir les carrés pairs eux-mêmes, `filter isEven (map square numbers)` donne `[4, 16]`. Dans `main`, ces résultats sont affichés avec `print`. Cette approche fonctionnelle combine composition, `map`, et `filter` pour transformer et filtrer la liste de manière concise et déclarative, respectant la demande de retourner les carrés pairs.
