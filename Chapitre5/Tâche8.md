HC5T8 : Style sans point
### Code Haskell

```haskell
-- Fonction addFive en style sans point
addFive :: Int -> Int
addFive = (+5)

-- Fonction main pour tester addFive
main :: IO ()
main = do
  print $ addFive 3   -- Résultat attendu : 8
  print $ addFive 10  -- Résultat attendu : 15
  print $ addFive 0   -- Résultat attendu : 5
```

### Explications détaillées

1. **Style sans point (point-free style)** :
   - En Haskell, le style sans point consiste à définir une fonction sans mentionner explicitement ses arguments. Au lieu d’écrire `f x = expr x`, on exprime `f` directement en termes d’autres fonctions ou opérateurs, en exploitant la curryfication et la composition.
   - Dans `addFive x = x + 5`, l’argument `x` apparaît explicitement des deux côtés de l’équation. En style sans point, on élimine `x` pour obtenir une définition plus concise.

2. **Conversion de `addFive`** :
   - La fonction originale `addFive x = x + 5` prend un entier `x` et retourne `x + 5`.
   - L’opération `x + 5` peut être vue comme l’application de l’opérateur `(+)` avec `5` comme second argument. En Haskell, les opérateurs comme `(+)` sont des fonctions curryfiées de type `Int -> Int -> Int`.
   - En fixant le second argument de `(+)` à `5`, on obtient la fonction `(+5) :: Int -> Int`, qui prend un entier et ajoute 5.
   - Ainsi, `addFive x = x + 5` devient `addFive = (+5)`, où l’argument `x` est implicite grâce à la curryfication.
   - Exemple :
     - `addFive 3 = (+5) 3 = 3 + 5 = 8`.
     - `addFive 10 = (+5) 10 = 10 + 5 = 15`.
     - `addFive 0 = (+5) 0 = 0 + 5 = 5`.

3. **Signature de `addFive`** :
   - La signature `addFive :: Int -> Int` indique que la fonction prend un entier et retourne un entier.
   - La forme sans point `(+5)` a exactement la même signature, car `(+)` est de type `Int -> Int -> Int`, et en fournissant `5`, on obtient une fonction de type `Int -> Int`.

4. **Fonction `main`** :
   - La fonction `main :: IO ()` teste `addFive` avec trois entiers : `3`, `10`, et `0`.
   - Chaque appel `addFive x` produit `x + 5`, et `print` affiche les résultats dans la console :
     - `addFive 3` donne `8`.
     - `addFive 10` donne `15`.
     - `addFive 0` donne `5`.

5. **Pourquoi `(+5)` fonctionne** :
   - En Haskell, les opérateurs infixes comme `(+)` peuvent être utilisés en style préfixe en les entourant de parenthèses, devenant une fonction `(+)` :: `Int -> Int -> Int`.
   - Fournir un argument, comme `(+5)`, est une application partielle, créant une nouvelle fonction qui attend un seul argument.
   - Cela correspond au style sans point, car l’argument de la fonction n’est plus mentionné explicitement dans la définition.

6. **Avantages du style sans point** :
   - Le style sans point rend la définition plus concise et élégante, en éliminant la redondance de l’argument `x`.
   - Il met en avant la structure fonctionnelle de l’expression, en se concentrant sur la composition des opérations plutôt que sur les variables.
   - Pour des fonctions simples comme `addFive`, il est particulièrement adapté, car l’opération est directe et ne nécessite pas de manipulation complexe des arguments.

7. **Approche fonctionnelle** :
   - Le style sans point exploite la curryfication, une caractéristique clé de Haskell, où les fonctions à plusieurs arguments sont des fonctions à un argument retournant une autre fonction.
   - L’évaluation paresseuse de Haskell garantit que les calculs ne sont effectués que lorsque nécessaires (ici, lors de l’affichage avec `print`).
   - Cette approche est déclarative, exprimant directement l’opération d’ajout de 5 sans mentionner explicitement l’argument.

### Résumé des explications

La fonction `addFive x = x + 5` est réécrite en style sans point comme `addFive = (+5)`, en utilisant l’application partielle de l’opérateur `(+)` avec `5` pour créer une fonction qui ajoute 5 à un entier. Dans `main`, elle est testée sur `3`, `10`, et `0`, produisant `8`, `15`, et `5`, affichés avec `print`. Le style sans point élimine l’argument explicite `x`, rendant la définition concise et fonctionnelle, tout en exploitant la curryfication pour une expression élégante et déclarative.
