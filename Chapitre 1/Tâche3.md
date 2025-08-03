HC1T3 - Tâche 3 : Vérifier si un nombre est supérieur à 18
Voici une fonction Haskell simple appelée greaterThan18 qui vérifie si un nombre est supérieur à 18 :

```haskell
-- Vérifie si un nombre est supérieur à 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- Fonction principale pour tester
main :: IO ()
main = do
    let testValue = 20
    putStrLn $ "Nombre: " ++ show testValue
    putStrLn $ "Supérieur à 18 ? " ++ show (greaterThan18 testValue)
```

### Explications :

1. **Fonction `greaterThan18`** :
   - **Signature** : `greaterThan18 :: Int -> Bool` indique que la fonction prend un entier (`Int`) et retourne un booléen (`Bool`).
   - **Définition** : La fonction utilise l'opérateur de comparaison `>` pour vérifier si l'entrée `x` est supérieure à 18. Elle retourne `True` si `x > 18`, sinon `False`.
   - **Pureté** : `greaterThan18` est pure, car elle dépend uniquement de son argument `x`, produit toujours le même résultat pour la même entrée, et n'a pas d'effets de bord.

2. **Exemple d'exécution** :
   - Pour `greaterThan18 20` :
     - `20 > 18` donne `True`.

Sortie dans la console :
```
Nombre: 20
Supérieur à 18 ? True
```

### Remarques :
- **Pureté** : La fonction est pure et ne dépend d'aucun état externe.
- **Simplicité** : La fonction est concise, utilisant l'opérateur de comparaison standard de Haskell.
- **Type `Int`** : J'ai utilisé `Int` pour l'entrée, mais la fonction pourrait être généralisée à d'autres types numériques avec `Num a => a -> Bool` si nécessaire.
