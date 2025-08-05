HC3T9 - Tâche avancée 9 : Trouver le maximum de trois nombres avec let

### Code Haskell
```haskell
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = if a > b then a else b  -- Maximum entre a et b
        maxABC = if maxAB > c then maxAB else c  -- Maximum entre maxAB et c
    in maxABC

-- Tests
main :: IO ()
main = do
    putStrLn $ "maxOfThree 10 20 15: " ++ show (maxOfThree 10 20 15)  -- Affiche "maxOfThree 10 20 15: 20"
    putStrLn $ "maxOfThree 5 25 10: " ++ show (maxOfThree 5 25 10)   -- Affiche "maxOfThree 5 25 10: 25"
```

### Explications
- **Définition de la fonction** : La fonction `maxOfThree` a la signature de type `Int -> Int -> Int -> Int`, prenant trois entiers comme arguments et retournant le plus grand d'entre eux.
- **Logique avec `let`** :
  - Une clause `let` définit deux valeurs intermédiaires :
    - `maxAB` : Calcule le maximum entre `a` et `b` en utilisant une expression `if-then-else`. Si `a > b`, alors `maxAB = a`, sinon `maxAB = b`.
    - `maxABC` : Compare `maxAB` avec `c`. Si `maxAB > c`, alors `maxABC = maxAB`, sinon `maxABC = c`.
  - La fonction retourne `maxABC`, qui est le maximum des trois nombres.
- **Tests** : La fonction `main` teste `maxOfThree` avec les triplets `(10, 20, 15)` et `(5, 25, 10)`. L'utilisation de `show` convertit les résultats entiers en chaînes pour l'affichage. Les sorties attendues sont :
  - Pour `(10, 20, 15)` : 
    - `maxAB = if 10 > 20 then 10 else 20`, donc `maxAB = 20`.
    - `maxABC = if 20 > 15 then 20 else 15`, donc `maxABC = 20`.
    - Résultat : `20`.
  - Pour `(5, 25, 10)` : 
    - `maxAB = if 5 > 25 then 5 else 25`, donc `maxAB = 25`.
    - `maxABC = if 25 > 10 then 25 else 10`, donc `maxABC = 25`.
    - Résultat : `25`.
- **Résultats des tests** :
  - `maxOfThree 10 20 15: 20`
  - `maxOfThree 5 25 10: 25`

### Remarque
- La fonction utilise une approche explicite avec des comparaisons intermédiaires via `let`, comme demandé. Une alternative plus concise en Haskell serait d'utiliser la fonction intégrée `maximum`, mais ici, nous respectons l'exigence d'utiliser `let` et des comparaisons manuelles.
  
