HC5T7 : L'opérateur $

### Code Haskell

```haskell
-- Fonction result utilisant l'opérateur $
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- Fonction main pour tester result
main :: IO ()
main = do
  print result  -- Résultat attendu : 98
```

### Explications détaillées

1. **Rôle de l’opérateur `$`** :
   - L’opérateur `$` en Haskell réduit l’usage des parenthèses en appliquant une fonction à un argument. Sa signature est `(a -> b) -> a -> b`, avec une très basse précédence, ce qui signifie qu’il évalue tout ce qui est à sa droite avant d’appliquer la fonction à gauche.
   - L’expression `f $ x` équivaut à `f x`, mais permet d’éviter des parenthèses dans des expressions imbriquées comme `f (g (h x))`, qui devient `f $ g $ h x`.
   - Ici, `$` remplace les parenthèses dans `sum (map (*2) (filter (>3) [1..10]))` pour simplifier l’expression.

2. **Analyse de l’expression originale** :
   - L’expression `sum (map (*2) (filter (>3) [1..10]))` effectue les étapes suivantes :
     - `filter (>3) [1..10]` : Filtre les nombres de `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]` pour ne garder que ceux supérieurs à 3, soit `[4, 5, 6, 7, 8, 9, 10]`.
     - `map (*2) [4, 5, 6, 7, 8, 9, 10]` : Multiplie chaque élément par 2, donnant `[8, 10, 12, 14, 16, 18, 20]`.
     - `sum [8, 10, 12, 14, 16, 18, 20]` : Calcule la somme, soit `8 + 10 + 12 + 14 + 16 + 18 + 20 = 98`.
   - Les parenthèses assurent l’ordre d’application : `filter` d’abord, puis `map`, et enfin `sum`.

3. **Réécriture avec `$`** :
   - Dans `result = sum $ map (*2) $ filter (>3) [1..10]` :
     - Le premier `$` remplace les parenthèses autour de `map (*2) (filter (>3) [1..10])`, équivalant à `sum (map (*2) (filter (>3) [1..10]))`.
     - Le second `$` remplace les parenthèses autour de `filter (>3) [1..10]`, équivalant à `map (*2) (filter (>3) [1..10])`.
   - L’évaluation se fait de droite à gauche en raison de la basse précédence de `$` :
     - `filter (>3) [1..10]` donne `[4, 5, 6, 7, 8, 9, 10]`.
     - `map (*2) [4, 5, 6, 7, 8, 9, 10]` donne `[8, 10, 12, 14, 16, 18, 20]`.
     - `sum [8, 10, 12, 14, 16, 18, 20]` donne `98`.

4. **Signature de `result`** :
   - La fonction `result :: Int` est une constante, car elle calcule un résultat fixe à partir de `[1..10]`.
   - Types intermédiaires :
     - `filter (>3) :: [Int] -> [Int]`, donne `[4, 5, 6, 7, 8, 9, 10]`.
     - `map (*2) :: [Int] -> [Int]`, donne `[8, 10, 12, 14, 16, 18, 20]`.
     - `sum :: [Int] -> Int`, donne `98`.

5. **Fonction `main`** :
   - La fonction `main :: IO ()` teste `result` en affichant sa valeur avec `print`.
   - Le résultat affiché est `98`, correspondant à la somme des nombres supérieurs à 3, multipliés par 2, dans `[1..10]`.

6. **Avantages de l’opérateur `$`** :
   - `$` rend l’expression plus concise en éliminant les parenthèses, améliorant la lisibilité pour les pipelines fonctionnels.
   - Il est idéal pour enchaîner des fonctions comme `filter`, `map`, et `sum`, où l’ordre d’application est linéaire.
   - L’expression reste sémantiquement identique, mais plus claire et moins encombrée.

7. **Approche fonctionnelle** :
   - L’expression utilise des fonctions d’ordre supérieur (`filter`, `map`, `sum`) dans un style fonctionnel, transformant la liste de manière déclarative.
   - `$` s’intègre naturellement dans ce style, facilitant la lecture des enchaînements de fonctions.
   - L’évaluation paresseuse de Haskell garantit que les calculs (filtrage, transformation, somme) ne sont effectués qu’au moment de l’affichage.

### Résumé des explications

La fonction `result = sum (map (*2) (filter (>3) [1..10]))` est réécrite comme `result = sum $ map (*2) $ filter (>3) [1..10]` en utilisant l’opérateur `$` pour remplacer les parenthèses. Elle filtre les nombres supérieurs à 3 dans `[1..10]` (`[4, 5, 6, 7, 8, 9, 10]`), multiplie chaque élément par 2 (`[8, 10, 12, 14, 16, 18, 20]`), et calcule leur somme (`98`). Dans `main`, le résultat `98` est affiché avec `print`. L’opérateur `$` simplifie l’expression en éliminant les parenthèses, tout en maintenant une approche fonctionnelle claire et concise avec `filter`, `map`, et `sum`.
