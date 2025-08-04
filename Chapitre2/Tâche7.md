HC2T7 - Tâche 7 : Expressions booléennes

### Expressions booléennes

1. **Expression qui retourne `True` en utilisant `&&`**
   - L'opérateur `&&` retourne `True` si les deux opérandes sont `True`.
   - Exemple :
     ```haskell
     True && True
     ```
     - Explication : `True && True` évalue à `True`, car les deux opérandes sont vraies. Une alternative pourrait être `1 < 2 && 3 > 0`, qui est aussi `True`.

2. **Expression qui retourne `False` en utilisant `||`**
   - L'opérateur `||` retourne `True` si au moins une des opérandes est `True`, donc pour obtenir `False`, les deux opérandes doivent être `False`.
   - Exemple :
     ```haskell
     False || False
     ```
     - Explication : `False || False` évalue à `False`, car aucune des opérandes n'est vraie. Une alternative pourrait être `1 > 2 || 3 < 0`, qui est aussi `False`.

3. **Expression qui retourne `True` en utilisant `not`**
   - L'opérateur `not` inverse la valeur booléenne de son opérande. Pour obtenir `True`, l'opérande doit être `False`.
   - Exemple :
     ```haskell
     not False
     ```
     - Explication : `not False` évalue à `True`, car `not` inverse la valeur `False`. Une alternative pourrait être `not (1 > 2)`, qui est aussi `True`.

4. **Une comparaison qui retourne `False`**
   - Une comparaison est une expression utilisant des opérateurs comme `<`, `>`, `==`, `/=`, etc., qui retourne `False` lorsque la condition n'est pas satisfaite.
   - Exemple :
     ```haskell
     5 < 2
     ```
     - Explication : `5 < 2` évalue à `False`, car 5 n'est pas inférieur à 2. Une alternative pourrait être `10 == 20` ou `1 /= 1`, qui retournent également `False`.

### Code complet avec démonstration

Voici un programme Haskell qui inclut ces expressions et une fonction `main` pour afficher leurs résultats :

```haskell
main :: IO ()
main = do
    putStrLn "Expression pour True avec && :"
    print (True && True)
    putStrLn "Expression pour False avec || :"
    print (False || False)
    putStrLn "Expression pour True avec not :"
    print (not False)
    putStrLn "Comparaison qui retourne False :"
    print (5 < 2)
```

### Résultats attendus

En exécutant ce programme (par exemple, en enregistrant dans `Main.hs` et en compilant avec `ghc Main.hs`), la sortie sera :

```
Expression pour True avec && :
True
Expression pour False avec || :
False
Expression pour True avec not :
True
Comparaison qui retourne False :
False
```

### Explications supplémentaires

- **Opérateur `&&`** : Retourne `True` uniquement si les deux opérandes sont `True`. Dans l'exemple, `True && True` satisfait cette condition.
- **Opérateur `||`** : Retourne `False` uniquement si les deux opérandes sont `False`. Dans l'exemple, `False || False` satisfait cette condition.
- **Opérateur `not`** : Inverse la valeur booléenne. `not False` donne `True`, et on pourrait aussi utiliser `not True` pour obtenir `False` si nécessaire.
- **Comparaison** : L'expression `5 < 2` est une comparaison simple qui évalue à `False`, car la condition n'est pas vérifiée.
