HC1T1 - Tâche 1 : Composition de fonctions

```haskell
-- Définition des fonctions de base
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

-- Composition de double et increment
doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- Fonction principale pour tester
main :: IO ()
main = do
    let testValue = 5
    putStrLn $ "Entrée: " ++ show testValue
    putStrLn $ "Résultat de doubleThenIncrement: " ++ show (doubleThenIncrement testValue)
```

### Explications :

1. **Définition des fonctions de base** :
   - `double :: Int -> Int` : Cette fonction prend un entier `x` et retourne `x * 2`.
   - `increment :: Int -> Int` : Cette fonction prend un entier `x` et retourne `x + 1`.
   - Les signatures de type `Int -> Int` indiquent que ces fonctions transforment un entier en un autre entier.

2. **Composition de fonctions** :
   - `doubleThenIncrement :: Int -> Int` : Cette fonction est définie en utilisant l'opérateur de composition `.` (point) en Haskell.
   - La syntaxe `increment . double` signifie que l'on applique d'abord `double` à l'entrée, puis `increment` au résultat.
   - Mathématiquement, si `f = increment` et `g = double`, alors `(f . g) x = f (g x)`. Dans notre cas, pour une entrée `x`, `doubleThenIncrement x = increment (double x)`.

3. **Pourquoi utiliser la composition ?**
   - En Haskell, la composition de fonctions permet de créer une nouvelle fonction en combinant des fonctions existantes de manière concise.
   - Ici, `doubleThenIncrement` équivaut à écrire une fonction qui fait `increment (double x)`, mais la notation `increment . double` est plus élégante et met en avant le style fonctionnel.

4. **Fonction `main`** :
   - La fonction `main` est le point d'entrée du programme, de type `IO ()`, car elle effectue des opérations d'entrée/sortie (affichage dans la console).
   - On définit une valeur de test `testValue = 5`.
   - On utilise `putStrLn` pour afficher l'entrée et le résultat de `doubleThenIncrement testValue`.
   - L'expression `show` convertit les valeurs numériques en chaînes pour l'affichage.

" ++ show testValue.
Cette partie de code Haskell, extraite d'un programme principal (`main`), est utilisée pour tester la fonction `doubleThenIncrement` en affichant une valeur d'entrée et son résultat après application de la fonction. Voici une explication ligne par ligne :

```haskell
let testValue = 5
putStrLn $ "Entrée: " ++ show testValue
putStrLn $ "Résultat de doubleThenIncrement: " ++ show (doubleThenIncrement testValue)
```

### Explication détaillée :

1. **`let testValue = 5`** :
   - **Contexte** : Cette ligne se trouve dans un bloc `do` de la fonction `main :: IO ()`, qui est une action d'entrée/sortie.
   - **Rôle** : La déclaration `let` définit une variable locale `testValue` avec la valeur `5` (de type `Int`).
   - **But** : Cette valeur sera utilisée comme entrée pour tester la fonction `doubleThenIncrement`.

2. **`putStrLn $ "Entrée: " ++ show testValue`** :
   - **Rôle de `putStrLn`** : La fonction `putStrLn :: String -> IO ()` affiche une chaîne de caractères (`String`) dans la console, suivie d'un saut de ligne.
   - **Expression `"Entrée: " ++ show testValue`** :
     - **`"Entrée: "`** : Une chaîne littérale qui sert de libellé pour indiquer ce qui est affiché.
     - **`++`** : L'opérateur de concaténation de chaînes en Haskell, qui combine deux `String`.
     - **`show testValue`** : La fonction `show :: Show a => a -> String` convertit `testValue` (ici, l'entier `5`) en une `String` (résultat : `"5"`). Cela est nécessaire, car `putStrLn` attend une `String`, et `testValue` est un `Int`.
     - Ensemble, `"Entrée: " ++ show testValue` produit la chaîne `"Entrée: 5"`.
   - **Effet** : `putStrLn` affiche `"Entrée: 5"` dans la console, suivi d'un saut de ligne.

3. **`putStrLn $ "Résultat de doubleThenIncrement: " ++ show (doubleThenIncrement testValue)`** :
   - **Rôle de `putStrLn`** : Comme ci-dessus, affiche une `String` avec un saut de ligne.
   - **Expression `"Résultat de doubleThenIncrement: " ++ show (doubleThenIncrement testValue)`** :
     - **`"Résultat de doubleThenIncrement: "`** : Une chaîne littérale servant de libellé.
     - **`doubleThenIncrement testValue`** : Applique la fonction `doubleThenIncrement` (définie précédemment comme `increment . double`) à `testValue` (5).
       - Si `double x = x * 2` et `increment x = x + 1`, alors `doubleThenIncrement 5 = increment (double 5) = increment (10) = 11`.
     - **`show (doubleThenIncrement testValue)`** : Convertit le résultat `11` (de type `Int`) en `String`, donnant `"11"`.
     - Ensemble, la concaténation produit `"Résultat de doubleThenIncrement: 11"`.
   - **Effet** : `putStrLn` affiche `"Résultat de doubleThenIncrement: 11"` dans la console, suivi d'un saut de ligne.

### Rôle de `$` :
- L'opérateur `$` est utilisé pour éviter des parenthèses. Il a une faible précédence et applique la fonction à gauche (ici, `putStrLn`) à l'expression à droite. Ainsi :
  - `putStrLn $ expr` est équivalent à `putStrLn (expr)`.
  - Cela rend le code plus lisible en évitant des parenthèses explicites autour des expressions complexes comme `"Entrée: " ++ show testValue`.

