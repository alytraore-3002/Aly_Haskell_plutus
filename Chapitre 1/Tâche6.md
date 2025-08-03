HC1T6 - Tâche 6 : Utilisation de signatures de type

```haskell
-- Additionne deux entiers
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Fonction principale pour tester
main :: IO ()
main = do
    let x = 3
    let y = 5
    putStrLn $ "Nombres: " ++ show x ++ " et " ++ show y
    putStrLn $ "Somme: " ++ show (addNumbers x y)
```

### Explications :

1. **Fonction `addNumbers`** :
   - **Signature** : `addNumbers :: Int -> Int -> Int` indique que la fonction prend deux entiers (`Int`) et retourne un entier.
   - **Définition** : Utilise l'opérateur `+` pour additionner les deux arguments `x` et `y`.
   - **Pureté** : La fonction est pure, car elle dépend uniquement de ses entrées `x` et `y`, produit toujours le même résultat pour les mêmes entrées, et n'a pas d'effets de bord.

2. **Exemple d'exécution** :
   - Pour `addNumbers 3 5` :
     - `3 + 5` donne `8`.

Sortie dans la console :
```
Nombres: 3 et 5
Somme: 8
```

### Remarques :
- **Pureté** : `addNumbers` est pure et ne dépend d'aucun état externe.
- **Simplicité** : La fonction est concise, utilisant l'opérateur standard `+` de Haskell.
- **Type `Int`** : La fonction utilise `Int` pour les entiers, mais pourrait être généralisée à `Num a => a -> a -> a` pour d'autres types numériques si nécessaire.
