HC5T5 : Application partielle

### Code Haskell

```haskell
-- Fonction de multiplication à deux arguments
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Application partielle pour multiplier par 5
multiplierByFive :: Int -> Int
multiplierByFive = multiply 5

-- Fonction main pour tester multiplierByFive
main :: IO ()
main = do
  print $ multiplierByFive 3   -- Résultat attendu : 15
  print $ multiplierByFive 10  -- Résultat attendu : 50
  print $ multiplierByFive 0   -- Résultat attendu : 0
```

### Explications détaillées

1. **Concept d’application partielle** :
   - En Haskell, l’application partielle consiste à fournir moins d’arguments qu’une fonction n’en attend, ce qui crée une nouvelle fonction attendant les arguments restants.
   - Dans l’exemple donné, `createEmail :: String -> String -> String -> String` prend trois arguments (`domain`, `name`, `lastName`). En fixant `domain`, comme dans `createEmail "teckel-owners.com"`, on obtient une nouvelle fonction `createEmailTeckel :: String -> String -> String` qui attend les deux arguments restants (`name` et `lastName`).
   - Ici, nous appliquons le même principe pour créer `multiplierByFive` en fixant un argument d’une fonction de multiplication.

2. **Définition de `multiply`** :
   - La fonction `multiply :: Int -> Int -> Int` prend deux entiers `x` et `y` et retourne leur produit `x * y`.
   - Elle sert de base pour l’application partielle, similaire à `createEmail` dans l’exemple, qui prend plusieurs arguments avant de produire un résultat final.

3. **Création de `multiplierByFive`** :
   - La fonction `multiplierByFive :: Int -> Int` est définie en utilisant l’application partielle : `multiplierByFive = multiply 5`.
   - En fournissant `5` comme premier argument à `multiply`, on crée une nouvelle fonction qui attend un seul argument (le second argument de `multiply`) et le multiplie par 5.
   - Par exemple :
     - `multiplierByFive 3` équivaut à `multiply 5 3`, ce qui donne `5 * 3 = 15`.
     - `multiplierByFive 10` équivaut à `multiply 5 10`, ce qui donne `5 * 10 = 50`.
     - `multiplierByFive 0` équivaut à `multiply 5 0`, ce qui donne `5 * 0 = 0`.

4. **Signature de `multiplierByFive`** :
   - La signature `Int -> Int` indique que `multiplierByFive` prend un entier et retourne un entier, résultat de la multiplication par 5.
   - Cela correspond à l’effet de l’application partielle : en fixant le premier argument de `multiply :: Int -> Int -> Int`, on obtient une fonction avec un argument de moins.

5. **Fonction `main`** :
   - La fonction `main :: IO ()` teste `multiplierByFive` avec trois entiers : `3`, `10`, et `0`.
   - Chaque appel `multiplierByFive x` produit le résultat de `5 * x`, et `print` affiche les résultats dans la console :
     - `multiplierByFive 3` donne `15`.
     - `multiplierByFive 10` donne `50`.
     - `multiplierByFive 0` donne `0`.

6. **Parallèle avec l’exemple `createEmail`** :
   - Dans l’exemple, `createEmailTeckel = createEmail "teckel-owners.com"` fixe le `domain` pour créer une fonction qui attend `name` et `lastName`.
   - De la même manière, `multiplierByFive = multiply 5` fixe le premier argument de `multiply` à `5`, créant une fonction qui attend un seul entier à multiplier par 5.
   - Les deux cas illustrent comment l’application partielle réduit le nombre d’arguments pour créer une fonction spécialisée.

7. **Approche fonctionnelle** :
   - L’application partielle est une caractéristique clé de la programmation fonctionnelle en Haskell, où les fonctions sont des valeurs de première classe.
   - Grâce à la curryfication (toutes les fonctions à plusieurs arguments sont en réalité des fonctions à un argument retournant une autre fonction), `multiply 5` retourne naturellement une fonction `Int -> Int` sans effort supplémentaire.
   - L’évaluation paresseuse de Haskell garantit que les calculs ne sont effectués que lorsque les résultats sont nécessaires (ici, lors de l’affichage avec `print`).

### Résumé des explications

La fonction `multiplierByFive :: Int -> Int` est créée en utilisant l’application partielle sur `multiply :: Int -> Int -> Int`, en fixant le premier argument à `5` avec `multiplierByFive = multiply 5`. Cette nouvelle fonction multiplie n’importe quel entier par 5. Dans `main`, elle est testée sur les entiers `3`, `10`, et `0`, produisant `15`, `50`, et `0`, affichés avec `print`. L’application partielle, similaire à `createEmailTeckel` dans l’exemple, réduit le nombre d’arguments pour créer une fonction spécialisée, illustrant une approche fonctionnelle concise et efficace.
