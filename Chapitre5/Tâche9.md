HC5T9 : Fonction d'ordre supérieur pour transformer une liste
### Code Haskell

```haskell
-- Fonction d'ordre supérieur transformList
transformList :: (a -> a) -> [a] -> [a]
transformList f xs = map (f . f) xs

-- Fonction main pour tester transformList
main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5]  -- Liste de test
  let double x = x * 2            -- Fonction pour doubler
  let inc x = x + 1               -- Fonction pour incrémenter
  print $ transformList double numbers  -- Résultat attendu : [4, 8, 12, 16, 20]
  print $ transformList inc numbers     -- Résultat attendu : [3, 4, 5, 6, 7]
```

### Explications détaillées

1. **Définition d’une fonction d’ordre supérieur** :
   - Une fonction d’ordre supérieur prend une ou plusieurs fonctions comme paramètres ou retourne une fonction comme résultat. Ici, `transformList` prend une fonction `f :: a -> a` comme paramètre, ce qui en fait une fonction d’ordre supérieur.
   - La fonction `transformList` doit appliquer la fonction donnée `f` deux fois à chaque élément d’une liste, c’est-à-dire, pour chaque élément `x`, calculer `f (f x)`.

2. **Signature de `transformList`** :
   - La signature `transformList :: (a -> a) -> [a] -> [a]` indique :
     - Le premier argument `(a -> a)` est une fonction qui prend un élément de type `a` et retourne un élément du même type `a`.
     - Le deuxième argument `[a]` est une liste d’éléments de type `a`.
     - Le résultat `[a]` est une liste contenant les éléments transformés après deux applications de `f`.
   - Le type polymorphe `a` permet à `transformList` de travailler avec des listes de n’importe quel type, tant que la fonction `f` préserve ce type.

3. **Implémentation de `transformList`** :
   - La définition `transformList f xs = map (f . f) xs` utilise :
     - L’opérateur de composition `(.)` pour créer une fonction qui applique `f` deux fois : `(f . f) x = f (f x)`.
     - La fonction `map :: (a -> b) -> [a] -> [b]` pour appliquer `f . f` à chaque élément de la liste `xs`.
   - Par exemple, pour `f = double` (où `double x = x * 2`) et `xs = [1, 2, 3]` :
     - `(double . double) 1 = double (double 1) = double 2 = 4`.
     - `(double . double) 2 = double (double 2) = double 4 = 8`.
     - `(double . double) 3 = double (double 3) = double 6 = 12`.
     - `map (double . double) [1, 2, 3] = [4, 8, 12]`.

4. **Fonction `main`** :
   - La fonction `main :: IO ()` teste `transformList` avec deux fonctions différentes sur la liste `[1, 2, 3, 4, 5]` :
     - `double x = x * 2` :
       - `transformList double [1, 2, 3, 4, 5]` applique `double` deux fois :
         - `1`: `double (double 1) = double 2 = 4`.
         - `2`: `double (double 2) = double 4 = 8`.
         - `3`: `double (double 3) = double 6 = 12`.
         - `4`: `double (double 4) = double 8 = 16`.
         - `5`: `double (double 5) = double 10 = 20`.
       - Résultat : `[4, 8, 12, 16, 20]`.
     - `inc x = x + 1` :
       - `transformList inc [1, 2, 3, 4, 5]` applique `inc` deux fois :
         - `1`: `inc (inc 1) = inc 2 = 3`.
         - `2`: `inc (inc 2) = inc 3 = 4`.
         - `3`: `inc (inc 3) = inc 4 = 5`.
         - `4`: `inc (inc 4) = inc 5 = 6`.
         - `5`: `inc (inc 5) = inc 6 = 7`.
       - Résultat : `[3, 4, 5, 6, 7]`.
   - Les résultats sont affichés avec `print`.

5. **Utilisation de la composition `(.)`** :
   - La composition `f . f` est essentielle pour appliquer `f` deux fois. Elle crée une nouvelle fonction qui combine deux applications de `f`.
   - Cela évite d’écrire explicitement une lambda comme `\x -> f (f x)` ou une définition récursive, rendant le code plus concise et élégant.

6. **Avantages de l’approche** :
   - L’utilisation de `map (f . f)` est concise et déclarative, exprimant directement l’intention d’appliquer `f` deux fois à chaque élément.
   - La fonction est polymorphe, donc réutilisable avec n’importe quelle fonction `f :: a -> a` et n’importe quelle liste `[a]`.
   - Cela illustre le pouvoir des fonctions d’ordre supérieur en Haskell, permettant de manipuler des fonctions comme des valeurs.

7. **Approche fonctionnelle** :
   - `transformList` est une fonction d’ordre supérieur typique, prenant une fonction comme paramètre et utilisant la composition pour construire la logique.
   - L’évaluation paresseuse de Haskell garantit que les transformations ne sont calculées que lorsque nécessaire (ici, pour l’affichage).
   - Le style fonctionnel évite les boucles explicites, rendant le code clair et maintenable.

### Résumé des explications

La fonction d’ordre supérieur `transformList :: (a -> a) -> [a] -> [a]` applique une fonction `f` deux fois à chaque élément d’une liste en utilisant `map (f . f) xs`, où `(f . f)` compose `f` avec elle-même. Dans `main`, elle est testée sur `[1, 2, 3, 4, 5]` avec `double x = x * 2` (résultat `[4, 8, 12, 16, 20]`) et `inc x = x + 1` (résultat `[3, 4, 5, 6, 7]`), affichés avec `print`. Cette approche fonctionnelle, utilisant composition et `map`, est concise, polymorphe, et illustre l’usage des fonctions d’ordre supérieur pour transformer des listes de manière déclarative.
