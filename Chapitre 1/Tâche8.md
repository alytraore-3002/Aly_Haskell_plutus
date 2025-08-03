HC1T8 - Tâche 8 : Fonctions d'ordre supérieur

```haskell
-- Applique une fonction deux fois à une valeur
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Fonction principale pour tester
main :: IO ()
main = do
    let x = 3
    let result = applyTwice (+1) x  -- Exemple avec une fonction qui incrémente
    putStrLn $ "Valeur initiale: " ++ show x
    putStrLn $ "Résultat après avoir appliqué (+1) deux fois: " ++ show result
```

### Explications :

1. **Fonction `applyTwice`** :
   - **Signature** : `applyTwice :: (a -> a) -> a -> a` indique que la fonction prend une fonction `f` de type `a -> a` (qui transforme une valeur de type `a` en une autre valeur du même type) et une valeur `x` de type `a`, et retourne une valeur de type `a`.
   - **Définition** : La fonction applique `f` à `x` pour obtenir `f x`, puis applique `f` à nouveau au résultat, soit `f (f x)`.
   - **Pureté** : La fonction est pure, car elle dépend uniquement de la fonction `f` et de la valeur `x`, et produit toujours le même résultat pour les mêmes entrées.

2. **Exemple d'exécution** :
   - Pour `applyTwice (+1) 3` :
     - Première application : `(+1) 3` donne `4`.
     - Deuxième application : `(+1) 4` donne `5`.

Sortie dans la console :
```
Valeur initiale: 3
Résultat après avoir appliqué (+1) deux fois: 5
```

### Remarques :
- **Pureté** : `applyTwice` est pure et ne dépend d'aucun état externe.
- **Généralité** : La fonction est polymorphe (grâce au type `a`), ce qui permet de l'utiliser avec n'importe quelle fonction `a -> a` et n'importe quel type de valeur d'entrée.
- **Simplicité** : La fonction est concise et illustre le style fonctionnel de Haskell en manipulant des fonctions comme des valeurs de première classe.
