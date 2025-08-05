HC3T5 - Tâche 5 : Déterminer le type d'un triangle avec des gardes

### Voici la définition de la fonction `triangleType` en Haskell qui classifie un triangle selon ses côtés (a, b, c) en utilisant des gardes pour déterminer s'il est équilatéral, isocèle ou scalène, suivie des tests demandés.

### Code Haskell
```haskell
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Équilatéral"
    | a == b || b == c || a == c = "Isocèle"
    | otherwise = "Scalène"

-- Tests
main :: IO ()
main = do
    putStrLn $ "triangleType 3 3 3: " ++ triangleType 3 3 3  -- Affiche "triangleType 3 3 3: Équilatéral"
    putStrLn $ "triangleType 5 5 8: " ++ triangleType 5 5 8  -- Affiche "triangleType 5 5 8: Isocèle"
    putStrLn $ "triangleType 6 7 8: " ++ triangleType 6 7 8  -- Affiche "triangleType 6 7 8: Scalène"
```

### Explications
- **Définition de la fonction** : La fonction `triangleType` a la signature de type `Float -> Float -> Float -> String`, prenant trois nombres à virgule flottante (les longueurs des côtés a, b, c) et retournant une chaîne indiquant le type de triangle.
- **Logique des gardes** :
  - `| a == b && b == c` : Vérifie si tous les côtés sont égaux (triangle équilatéral).
  - `| a == b || b == c || a == c` : Vérifie si au moins deux côtés sont égaux (triangle isocèle).
  - `| otherwise` : Si aucune des conditions précédentes n'est satisfaite, le triangle est scalène (aucun côté égal).
- **Tests** : La fonction `main` teste `triangleType` avec les triplets `(3, 3, 3)`, `(5, 5, 8)` et `(6, 7, 8)`. Les sorties attendues sont :
  - `triangleType 3 3 3: Équilatéral` (tous les côtés égaux).
  - `triangleType 5 5 8: Isocèle` (deux côtés égaux : 5 et 5).
  - `triangleType 6 7 8: Scalène` (aucun côté égal).
- **Remarque** : La fonction suppose que les côtés forment un triangle valide (satisfaisant les inégalités triangulaires : \( a + b > c \), \( b + c > a \), \( a + c > b \)). Une vérification de validité pourrait être ajoutée pour plus de robustesse, mais elle n'est pas requise ici.

### Résultats des tests
- `triangleType 3 3 3: Équilatéral`
- `triangleType 5 5 8: Isocèle`
- `triangleType 6 7 8: Scalène`
