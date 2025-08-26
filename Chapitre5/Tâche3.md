HC5T3 : Vérifier la présence de majuscules

### Code Haskell

```haskell
import Data.Char (isUpper)  -- Pour vérifier si un caractère est une majuscule

-- Fonction prédicat pour vérifier si un mot commence par une majuscule
startsWithUpper :: String -> Bool
startsWithUpper "" = False  -- Cas d'une chaîne vide
startsWithUpper (x:_) = isUpper x  -- Vérifie si le premier caractère est une majuscule

-- Fonction main pour tester la fonction any
main :: IO ()
main = do
  let words1 = ["hello", "World", "code"]  -- Liste avec un mot commençant par une majuscule
  let words2 = ["hello", "world", "code"]  -- Liste sans majuscule
  let words3 = ["Hello", "World"]          -- Liste avec plusieurs majuscules
  let words4 = []                          -- Liste vide
  print $ any startsWithUpper words1       -- Résultat attendu : True
  print $ any startsWithUpper words2       -- Résultat attendu : False
  print $ any startsWithUpper words3       -- Résultat attendu : True
  print $ any startsWithUpper words4       -- Résultat attendu : False
```

### Explications détaillées

1. **Signature et rôle de la fonction `any`** :
   - La signature `any :: (a -> Bool) -> [a] -> Bool` indique que :
     - Le premier argument `(a -> Bool)` est un prédicat qui prend un élément de type `a` et retourne un `Bool`.
     - Le deuxième argument `[a]` est une liste d’éléments de type `a`.
     - Le résultat `Bool` est `True` si au moins un élément de la liste satisfait le prédicat, `False` sinon.
   - Par exemple, pour `any biggerThan4 [1,2,3,4]` avec `biggerThan4 x = x > 4` :
     - Aucun élément (1, 2, 3, 4) ne satisfait `x > 4`, donc `any` retourne `False`.

2. **Fonction prédicat `startsWithUpper`** :
   - La fonction `startsWithUpper :: String -> Bool` vérifie si un mot (une chaîne de caractères) commence par une majuscule.
   - Définie comme :
     - `startsWithUpper "" = False` : Si la chaîne est vide, elle ne commence pas par une majuscule, donc retourne `False`.
     - `startsWithUpper (x:_) = isUpper x` : Pour une chaîne non vide, décompose la chaîne en sa tête `x` (premier caractère) et sa queue `_` (le reste, ignoré). La fonction `isUpper` (du module `Data.Char`) vérifie si `x` est une lettre majuscule.
   - Exemples :
     - `startsWithUpper "Hello" = isUpper 'H' = True`.
     - `startsWithUpper "world" = isUpper 'w' = False`.
     - `startsWithUpper "" = False`.

3. **Utilisation de `any`** :
   - La fonction `any startsWithUpper xs` applique le prédicat `startsWithUpper` à chaque élément de la liste `xs` et retourne `True` dès qu’un élément satisfait le prédicat, ou `False` si aucun ne le satisfait.
   - Dans `main`, nous testons quatre cas :
     - `words1 = ["hello", "World", "code"]` :
       - `startsWithUpper "hello" = False`, `startsWithUpper "World" = True`, `startsWithUpper "code" = False`.
       - Puisque `"World"` commence par une majuscule, `any startsWithUpper words1 = True`.
     - `words2 = ["hello", "world", "code"]` :
       - Tous les mots donnent `False` (`startsWithUpper` retourne `False` pour chaque mot).
       - Résultat : `any startsWithUpper words2 = False`.
     - `words3 = ["Hello", "World"]` :
       - `startsWithUpper "Hello" = True`, `startsWithUpper "World" = True`.
       - Résultat : `any startsWithUpper words3 = True` (dès le premier `True`).
     - `words4 = []` :
       - Pour une liste vide, `any` retourne `False` par définition, car aucun élément ne satisfait le prédicat.
       - Résultat : `any startsWithUpper words4 = False`.

4. **Importation de `Data.Char`** :
   - Le module `Data.Char` fournit la fonction `isUpper :: Char -> Bool`, qui teste si un caractère est une lettre majuscule (par exemple, 'A' à 'Z').
   - Cette fonction est essentielle pour vérifier la majuscule sans avoir à écrire une logique manuelle (par exemple, vérifier si le caractère est dans `['A'..'Z']`).

5. **Fonction `main`** :
   - La fonction `main :: IO ()` définit quatre listes de test :
     - `words1` contient un mot avec une majuscule.
     - `words2` n’en contient aucun.
     - `words3` contient plusieurs mots avec majuscules.
     - `words4` est vide.
   - Chaque appel à `print $ any startsWithUpper wordsX` affiche le résultat (`True` ou `False`) pour chaque liste.
   - La fonction `print` convertit le résultat `Bool` en une chaîne et l’affiche dans la console.

6. **Approche fonctionnelle** :
   - L’utilisation de `any` illustre le style fonctionnel de Haskell : elle abstrait la logique de parcours de la liste et d’évaluation du prédicat.
   - Cela évite d’écrire une boucle explicite ou une récursion manuelle, rendant le code concis et déclarative.
   - La fonction `startsWithUpper` est réutilisable et peut être appliquée à d’autres contextes si nécessaire.

7. **Gestion des cas particuliers** :
   - La fonction `startsWithUpper` gère explicitement le cas de la chaîne vide (`""`), ce qui est important pour éviter des erreurs sur des entrées inattendues.
   - La fonction `any` gère automatiquement le cas de la liste vide, retournant `False` sans évaluer le prédicat.

### Résumé des explications

La fonction `startsWithUpper :: String -> Bool` vérifie si un mot commence par une majuscule en utilisant `isUpper` sur le premier caractère, avec un cas particulier pour la chaîne vide. La fonction `any :: (a -> Bool) -> [a] -> Bool` est utilisée pour vérifier si au moins un mot d’une liste satisfait `startsWithUpper`. Dans `main`, quatre listes de test (`["hello", "World", "code"]`, `["hello", "world", "code"]`, `["Hello", "World"]`, `[]`) sont évaluées, produisant respectivement `True`, `False`, `True`, et `False`, affichés avec `print`. Le code utilise une approche fonctionnelle concise, avec `any` pour abstraire le parcours de la liste et `isUpper` pour tester les majuscules.
