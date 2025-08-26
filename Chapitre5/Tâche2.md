HC5T2 : Filtrer les nombres détériore

### Code Haskell

```haskell
-- Définition d'une fonction pour vérifier si un nombre est impair
isOdd :: Int -> Bool
isOdd n = n `mod` 2 /= 0

-- Fonction main pour tester le filtrage des nombres impairs
main :: IO ()
main = do
  let numbers = [1..30]  -- Liste d'entiers de 1 à 30
  let oddNumbers = filter isOdd numbers  -- Filtrer les nombres impairs
  print oddNumbers  -- Afficher la liste des nombres impairs
```

### Explications détaillées

1. **Signature de type de `filter`** :
   - La signature `filter :: forall a. (a -> Bool) -> [a] -> [a]` indique que `filter` est une fonction polymorphique :
     - Le premier argument `(a -> Bool)` est un **prédicat**, une fonction qui prend un élément de type `a` et retourne un `Bool` (vrai ou faux).
     - Le deuxième argument `[a]` est une liste d'éléments de type `a`.
     - Le résultat `[a]` est une liste contenant uniquement les éléments de la liste d'entrée pour lesquels le prédicat retourne `True`.
   - Dans notre cas, nous utilisons `filter` avec des entiers (`a = Int`), donc le prédicat sera de type `Int -> Bool`, et la liste d'entrée et de sortie sera de type `[Int]`.

2. **Fonction `isOdd`** :
   - Nous définissons une fonction `isOdd :: Int -> Bool` pour servir de prédicat à `filter`.
   - La fonction `isOdd n = n `mod` 2 /= 0` vérifie si un nombre `n` est impair :
     - `n `mod` 2` calcule le reste de la division de `n` par 2.
     - Si `n `mod` 2 /= 0` (le reste est différent de 0), alors `n` est impair, et `isOdd n` retourne `True`.
     - Sinon, `n` est pair, et `isOdd n` retourne `False`.
   - Par exemple :
     - `isOdd 3 = 3 `mod` 2 /= 0 = 1 /= 0 = True`.
     - `isOdd 4 = 4 `mod` 2 /= 0 = 0 /= 0 = False`.

3. **Génération de la liste `[1..30]`** :
   - L'expression `[1..30]` crée une liste d'entiers de 1 à 30 inclus, en utilisant la syntaxe de plage de Haskell.
   - Cette liste est définie dans `main` avec `let numbers = [1..30]`.
   - Résultat : `[1, 2, 3, ..., 29, 30]`.

4. **Application de `filter`** :
   - L'expression `filter isOdd numbers` applique la fonction `filter` à la liste `numbers` avec le prédicat `isOdd`.
   - Pour chaque élément `n` de la liste `numbers`, `filter` vérifie si `isOdd n` est `True` :
     - Si `True`, l'élément est inclus dans la liste résultat.
     - Si `False`, l'élément est exclu.
   - Les nombres impairs de 1 à 30 sont : `1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29`.
   - Ainsi, `filter isOdd [1..30]` retourne `[1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]`.

5. **Affichage du résultat** :
   - Dans `main`, nous utilisons `print oddNumbers` pour afficher la liste des nombres impairs.
   - La fonction `print` convertit la liste `[Int]` en une chaîne de caractères et l'affiche dans la console.
   - La sortie sera : `[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29]`.

6. **Approche fonctionnelle** :
   - L'utilisation de `filter` illustre le style fonctionnel de Haskell : au lieu de boucler explicitement sur la liste et de vérifier chaque élément, `filter` abstrait cette logique.
   - Cela rend le code concis et déclarative, en exprimant **quoi** faire (filtrer les éléments impairs) plutôt que **comment** le faire (parcourir la liste manuellement).

7. **Utilisation de `let` dans `main`** :
   - Les définitions `let numbers = [1..30]` et `let oddNumbers = filter isOdd numbers` sont locales à la fonction `main`.
   - Cela permet de structurer le code de manière claire et de limiter la portée des variables.

### Résumé des explications

La fonction `filter` est utilisée pour extraire les nombres impairs d'une liste d'entiers de 1 à 30. Une fonction prédicat `isOdd :: Int -> Bool` vérifie si un nombre est impair en utilisant l'opération `mod`. Dans `main`, la liste `[1..30]` est générée, puis `filter isOdd numbers` produit la liste des nombres impairs `[1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]`, qui est affichée avec `print`. Le code illustre le style fonctionnel de Haskell, avec une approche concise et déclarative grâce à `filter`.
