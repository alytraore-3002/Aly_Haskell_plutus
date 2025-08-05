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

### Exemple d'exécution :
Si on compile et exécute ce programme avec `testValue = 5` :
- `double 5` donne `10` (5 * 2).
- `increment 10` donne `11` (10 + 1).
- Donc, `doubleThenIncrement 5` retourne `11`.
