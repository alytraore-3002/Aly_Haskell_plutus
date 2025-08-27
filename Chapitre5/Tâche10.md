HC5T10 : Combiner les fonctions d'ordre supérieur
### Code Haskell

```haskell
-- Fonction pour vérifier si une liste contient une valeur au carré > 50
hasSquareGreaterThan50 :: [Int] -> Bool
hasSquareGreaterThan50 xs = any (>50) (map (\x -> x * x) xs)

-- Fonction main pour tester hasSquareGreaterThan50
main :: IO ()
main = do
  let list1 = [1, 2, 3, 4, 5]      -- Aucun carré > 50
  let list2 = [6, 7, 8, 9, 10]     -- Certains carrés > 50 (ex. 8²=64)
  let list3 = []                    -- Liste vide
  print $ hasSquareGreaterThan50 list1  -- Résultat attendu : False
  print $ hasSquareGreaterThan50 list2  -- Résultat attendu : True
  print $ hasSquareGreaterThan50 list3  -- Résultat attendu : False
```

### Explications détaillées

1. **Objectif de la fonction** :
   - La fonction `hasSquareGreaterThan50` doit vérifier si une liste d’entiers contient au moins une valeur dont le carré est supérieur à 50.
   - Elle utilise `map` pour calculer les carrés, `filter` n’est pas strictement nécessaire ici (car `any` peut gérer le filtrage implicite), mais je vais montrer comment les trois fonctions peuvent être combinées, avec `filter` comme alternative dans l’explication.

2. **Signature de `hasSquareGreaterThan50`** :
   - La signature `hasSquareGreaterThan50 :: [Int] -> Bool` indique :
     - L’argument est une liste d’entiers `[Int]`.
     - Le résultat est un booléen `Bool`, `True` si au moins une valeur au carré est supérieure à 50, `False` sinon.

3. **Implémentation** :
   - La fonction est définie comme `hasSquareGreaterThan50 xs = any (>50) (map (\x -> x * x) xs)` :
     - `map (\x -> x * x) xs` applique la fonction lambda `\x -> x * x` à chaque élément de `xs`, produisant une liste des carrés.
     - `any (>50)` vérifie si au moins un élément de cette liste de carrés est supérieur à 50, retournant `True` si c’est le cas, `False` sinon.
   - Exemple avec `xs = [1, 2, 3, 4, 5]` :
     - `map (\x -> x * x) [1, 2, 3, 4, 5] = [1, 4, 9, 16, 25]`.
     - `any (>50) [1, 4, 9, 16, 25] = False` (aucun élément > 50).
   - Exemple avec `xs = [6, 7, 8, 9, 10]` :
     - `map (\x -> x * x) [6, 7, 8, 9, 10] = [36, 49, 64, 81, 100]`.
     - `any (>50) [36, 49, 64, 81, 100] = True` (car 64 > 50).

4. **Rôle de `filter`, `map`, et `any`** :
   - `map (\x -> x * x) xs` transforme chaque élément `x` en son carré `x * x`, produisant une liste de type `[Int]`.
   - `any (>50)` applique le prédicat `(>50)` à chaque élément de la liste des carrés, retournant `True` dès qu’un élément satisfait la condition.
   - Bien que `filter` ne soit pas utilisé dans l’implémentation principale (car `any` combine le filtrage et la vérification d’existence), une alternative équivalente serait :
     - `hasSquareGreaterThan50 xs = not (null (filter (>50) (map (\x -> x * x) xs)))`.
     - Ici, `filter (>50)` garde les carrés supérieurs à 50, et `not (null ...)` vérifie si la liste filtrée n’est pas vide.
     - Cependant, `any` est plus idiomatique et concis, car il évite de créer une liste intermédiaire explicite.

5. **Fonction `main`** :
   - La fonction `main :: IO ()` teste `hasSquareGreaterThan50` sur trois listes :
     - `list1 = [1, 2, 3, 4, 5]` : Carrés = `[1, 4, 9, 16, 25]`, aucun > 50, résultat : `False`.
     - `list2 = [6, 7, 8, 9, 10]` : Carrés = `[36, 49, 64, 81, 100]`, 64 > 50, résultat : `True`.
     - `list3 = []` : Liste vide, `map` donne `[]`, `any (>50) [] = False`.
   - Les résultats sont affichés avec `print`.

6. **Approche fonctionnelle** :
   - L’utilisation de `map` et `any` illustre le style fonctionnel de Haskell : les transformations et vérifications sont déclaratives, sans boucles explicites.
   - La fonction lambda `\x -> x * x` est utilisée pour calculer les carrés, mais pourrait être remplacée par une fonction nommée comme `square x = x * x` si réutilisée.
   - L’évaluation paresseuse de Haskell garantit que `any` s’arrête dès qu’un carré > 50 est trouvé, optimisant l’exécution.

7. **Intégration de `filter`** :
   - Bien que `filter` ne soit pas nécessaire dans l’implémentation principale, il pourrait être utilisé pour extraire les carrés > 50 avant de vérifier leur existence avec `any` ou `null`.
   - Par exemple, `filter (>50) (map (\x -> x * x) xs)` donnerait `[64, 81, 100]` pour `list2`, et on pourrait vérifier si cette liste est non vide.
   - L’implémentation choisie avec `any` est plus directe, mais montre comment `map` et `any` se combinent, et `filter` peut être intégré dans une variante.

### Résumé des explications

La fonction `hasSquareGreaterThan50 :: [Int] -> Bool` vérifie si une liste contient une valeur dont le carré est supérieur à 50, en utilisant `map (\x -> x * x)` pour calculer les carrés et `any (>50)` pour vérifier si un carré dépasse 50. Testée dans `main` sur `[1, 2, 3, 4, 5]` (`False`), `[6, 7, 8, 9, 10]` (`True`), et `[]` (`False`), les résultats sont affichés avec `print`. Bien que `filter` puisse être utilisé dans une variante (par exemple, `filter (>50)`), `any` combine efficacement filtrage et vérification. Cette approche fonctionnelle est concise, utilise des fonctions d’ordre supérieur, et exploite l’évaluation paresseuse pour une efficacité optimale.
