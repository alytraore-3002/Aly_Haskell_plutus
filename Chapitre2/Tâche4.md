HC2T4 - Tâche 4 : Notation préfixe et infixe

### Conversion des expressions en notation préfixe

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

```module Main where

main :: IO ()
main = do
  -- 1. Conversion de notation infixe vers préfixe
  putStrLn "1. Expressions en notation infixe et leur équivalent en préfixe :"

  -- Expression : 5 + 3
  putStrLn "Infixe : 5 + 3"
  putStrLn $ "Préfixe : (+) 5 3 = " ++ show ((+) 5 3)

  -- Expression : 10 * 4
  putStrLn "Infixe : 10 * 4"
  putStrLn $ "Préfixe : (*) 10 4 = " ++ show ((*) 10 4)

  -- Expression : Vrai et Faux (en Haskell, 'et' est &&)
  putStrLn "Infixe : True && False"
  putStrLn $ "Préfixe : (&&) True False = " ++ show ((&&) True False)

  -- 2. Conversion de notation préfixe vers infixe
  putStrLn "\n2. Fonctions en notation préfixe et leur équivalent en infixe :"

  -- Fonction : (+) 7 2
  putStrLn "Préfixe : (+) 7 2"
  putStrLn $ "Infixe : 7 + 2 = " ++ show (7 + 2)

  -- Fonction : (*) 6 5
  putStrLn "Préfixe : (*) 6 5"
  putStrLn $ "Infixe : 6 * 5 = " ++ show (6 * 5)

  -- Fonction : (&&) True False
  putStrLn "Préfixe : (&&) True False"
  putStrLn $ "Infixe : True && False = " ++ show (True && False)
```

### Explications

- **Notation préfixe** : Les opérateurs comme `+`, `*`, et `&&` deviennent des fonctions lorsqu'ils sont entourés de parenthèses (par exemple, `(+)`, `(*)`, `(&&)`). Les arguments suivent dans l'ordre.
- **Notation infixe** : Les fonctions préfixe peuvent être utilisées comme opérateurs infixes en les entourant de backticks (par exemple, `x + y` est équivalent à `(+) x y`). Dans ce cas, les expressions données étaient déjà des opérateurs standard, donc la conversion infixe utilise leur forme naturelle.
