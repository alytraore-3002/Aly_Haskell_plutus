HC5T1 : Utiliser applyTwice

### Code Haskell

```haskell
-- Définition de la fonction applyThreeTimes
applyThreeTimes :: (Int -> Int) -> Int -> Int
applyThreeTimes f x = f (f (f x))

-- Fonction main pour tester applyThreeTimes
main :: IO ()
main = do
  -- Exemple 1 : Appliquer la fonction double (x * 2) à 3
  let double x = x * 2
  print $ applyThreeTimes double 3  -- Résultat attendu : 24
  
  -- Exemple 2 : Appliquer la fonction incrément (x + 1) à 5
  let inc x = x + 1
  print $ applyThreeTimes inc 5    -- Résultat attendu : 8
  
  -- Exemple 3 : Appliquer la fonction carré (x * x) à 2
  let square x = x * x
  print $ applyThreeTimes square 2 -- Résultat attendu : 256
```

### Explications détaillées

1. **Définition de la fonction `applyThreeTimes`** :
   - La signature de type est `applyThreeTimes :: (Int -> Int) -> Int -> Int`.
     - Le premier argument `(Int -> Int)` est une fonction qui prend un entier et retourne un entier.
     - Le deuxième argument `Int` est l'entier auquel la fonction sera appliquée.
     - Le type de retour `Int` est le résultat après avoir appliqué la fonction trois fois.
   - La fonction est définie comme `f (f (f x))`, ce qui signifie :
     - Appliquer `f` à `x` pour obtenir un premier résultat.
     - Appliquer `f` à ce résultat pour obtenir un deuxième résultat.
     - Appliquer `f` une troisième fois à ce deuxième résultat pour obtenir le résultat final.
   - Par exemple, si `f` est une fonction qui double un nombre (comme `double x = x * 2`) et `x = 3` :
     - Première application : `f 3 = 6`.
     - Deuxième application : `f 6 = 12`.
     - Troisième application : `f 12 = 24`.
     - Résultat final : `24`.

2. **Polymorphisme fonctionnel** :
   - La fonction `applyThreeTimes` est générique dans le sens où elle peut travailler avec n'importe quelle fonction de type `Int -> Int`. Cela illustre le polymorphisme de Haskell : on peut passer différentes fonctions (comme doubler, incrémenter, ou mettre au carré) tant qu'elles respectent la signature de type.
   - Cela rend la fonction réutilisable et flexible.

3. **Composition fonctionnelle** :
   - L'expression `f (f (f x))` est un exemple de composition fonctionnelle. En Haskell, la composition de fonctions est souvent utilisée pour construire des transformations complexes à partir de fonctions simples.
   - Une alternative serait d'utiliser l'opérateur de composition `(.)`, mais ici, comme nous appliquons exactement trois fois, écrire `f (f (f x))` est plus clair et direct.

4. **Fonction `main`** :
   - La fonction `main` est de type `IO ()`, ce qui indique qu'elle effectue des actions d'entrée/sortie (ici, afficher des résultats).
   - Nous définissons trois fonctions de test :
     - `double x = x * 2` : multiplie un nombre par 2.
     - `inc x = x + 1` : incrémente un nombre de 1.
     - `square x = x * x` : met un nombre au carré.
   - Nous utilisons `print` pour afficher les résultats de `applyThreeTimes` avec ces fonctions et des valeurs initiales différentes :
     - `applyThreeTimes double 3` :
       - `double 3 = 6`, `double 6 = 12`, `double 12 = 24`. Résultat : `24`.
     - `applyThreeTimes inc 5` :
       - `inc 5 = 6`, `inc 6 = 7`, `inc 7 = 8`. Résultat : `8`.
     - `applyThreeTimes square 2` :
       - `square 2 = 4`, `square 4 = 16`, `square 16 = 256`. Résultat : `256`.

5. **Utilisation de `let` dans `main`** :
   - Les définitions comme `let double x = x * 2` sont locales à la fonction `main`. Elles créent des fonctions temporaires pour les tests.
   - Cela permet de garder le code clair et d'éviter de définir ces fonctions au niveau global si elles ne sont utilisées que dans `main`.

6. **Sortie avec `print`** :
   - La fonction `print` convertit les résultats (de type `Int`) en chaînes de caractères et les affiche dans la console.
   - Chaque appel à `print` affiche une ligne dans la sortie, correspondant aux résultats `24`, `8`, et `256`.

### Résumé des explications

La fonction `applyThreeTimes` prend une fonction `f :: Int -> Int` et un entier `x`, et applique `f` trois fois à `x` (i.e., `f (f (f x))`). Sa signature de type `(Int -> Int) -> Int -> Int` reflète sa capacité à travailler avec n'importe quelle fonction qui transforme un entier en un autre. Dans la fonction `main`, nous testons `applyThreeTimes` avec trois fonctions différentes (`double`, `inc`, `square`) sur des valeurs initiales (3, 5, 2), produisant respectivement 24, 8, et 256. Le code illustre la composition fonctionnelle, le polymorphisme, et l'utilisation d'actions IO pour afficher les résultats.
