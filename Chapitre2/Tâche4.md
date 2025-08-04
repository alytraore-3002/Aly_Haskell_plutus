HC2T4 - Tâche 4 : Notation préfixe et infixe

### En Haskell, la notation préfixe et la notation infixe sont deux manières d'écrire des expressions utilisant des opérateurs ou des fonctions. Les opérateurs comme `+`, `*`, et `&&` sont généralement utilisés en notation infixe (entre les arguments), mais peuvent être utilisés en notation préfixe en les entourant de parenthèses. Inversement, les fonctions écrites en préfixe peuvent être converties en infixe en utilisant des backticks (\`).

### Conversion des expressions en notation préfixe

Les expressions données utilisent la notation infixe. Je vais les réécrire en notation préfixe, où l'opérateur est placé avant les arguments.

1. **5 + 3**
   - **Infixe** : `5 + 3`
   - **Préfixe** : En Haskell, pour utiliser un opérateur infixe comme une fonction préfixe, on l'entoure de parenthèses.
     ```haskell
     (+) 5 3
     ```
     - Explication : L'opérateur `+` devient la fonction `(+)`, et les arguments `5` et `3` suivent. Cette expression évalue à `8`.

2. **10 * 4**
   - **Infixe** : `10 * 4`
   - **Préfixe** :
     ```haskell
     (*) 10 4
     ```
     - Explication : L'opérateur `*` devient la fonction `(*)`, avec les arguments `10` et `4`. Cette expression évalue à `40`.

3. **Vrai et faux** (interprété comme `True && False`)
   - **Infixe** : `True && False`
   - **Préfixe** :
     ```haskell
     (&&) True False
     ```
     - Explication : L'opérateur `&&` devient la fonction `(&&)` en notation préfixe, avec les arguments `True` et `False`. Cette expression évalue à `False`.

### Conversion des fonctions en notation infixe

Les expressions données sont en notation préfixe (l'opérateur est une fonction suivie des arguments). Je vais les réécrire en notation infixe en utilisant des backticks pour transformer la fonction en opérateur.

1. **(+) 7 2**
   - **Préfixe** : `(+) 7 2`
   - **Infixe** :
     ```haskell
     7 + 2
     ```
     - Explication : La fonction `(+)`, qui est l'opérateur `+` en préfixe, est réécrite en notation infixe standard en plaçant `+` entre `7` et `2`. Cette expression évalue à `9`.

2. **(*) 6 5**
   - **Préfixe** : `(*) 6 5`
   - **Infixe** :
     ```haskell
     6 * 5
     ```
     - Explication : La fonction `(*)` devient l'opérateur `*` en notation infixe, placé entre `6` et `5`. Cette expression évalue à `30`.

3. **(&&) Vrai Faux** (interprété comme `(&&) True False`)
   - **Préfixe** : `(&&) True False`
   - **Infixe** :
     ```haskell
     True && False
     ```
     - Explication : La fonction `(&&)` est réécrite comme l'opérateur `&&` en notation infixe, placé entre `True` et `False`. Cette expression évalue à `False`.

### Code complet avec démonstration

Pour illustrer ces conversions, voici un programme Haskell avec une fonction `main` qui affiche les résultats des expressions dans les deux notations :

```haskell
main :: IO ()
main = do
    putStrLn "Expressions en notation préfixe :"
    print ((+) 5 3)          -- Affiche 8
    print ((*) 10 4)         -- Affiche 40
    print ((&&) True False)  -- Affiche False
    putStrLn "Expressions en notation infixe :"
    print (7 + 2)            -- Affiche 9
    print (6 * 5)            -- Affiche 30
    print (True && False)    -- Affiche False
```

### Explications

- **Notation préfixe** : Les opérateurs comme `+`, `*`, et `&&` deviennent des fonctions lorsqu'ils sont entourés de parenthèses (par exemple, `(+)`, `(*)`, `(&&)`). Les arguments suivent dans l'ordre.
- **Notation infixe** : Les fonctions préfixe peuvent être utilisées comme opérateurs infixes en les entourant de backticks (par exemple, `x + y` est équivalent à `(+) x y`). Dans ce cas, les expressions données étaient déjà des opérateurs standard, donc la conversion infixe utilise leur forme naturelle.
