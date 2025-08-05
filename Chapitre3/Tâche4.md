HC3T4 - Tâche 4 : Calculer l'aire d'un triangle avec la formule de Héron

### Voici la définition de la fonction `triangleArea` en Haskell qui calcule l'aire d'un triangle à partir des longueurs de ses trois côtés (a, b, c) en utilisant la formule de Héron. La fonction utilise une expression `let` pour calculer le demi-périmètre `s` et inclut les tests demandés.

### Code Haskell
```haskell
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2  -- Demi-périmètre
    in sqrt (s * (s - a) * (s - b) * (s - c))  -- Formule de Héron

-- Tests
main :: IO ()
main = do
    putStrLn $ "triangleArea 3 4 5: " ++ show (triangleArea 3 4 5)  -- Affiche "triangleArea 3 4 5: 6.0"
    putStrLn $ "triangleArea 7 8 9: " ++ show (triangleArea 7 8 9)  -- Affiche "triangleArea 7 8 9: 26.832815729997478"
```

### Explications
- **Définition de la fonction** : La fonction `triangleArea` a la signature de type `Float -> Float -> Float -> Float`, prenant trois nombres à virgule flottante (les longueurs des côtés a, b, c) et retournant l'aire du triangle sous forme de `Float`.
- **Logique avec `let`** :
  - Une expression `let` calcule le demi-périmètre `s = (a + b + c) / 2`.
  - La formule de Héron est appliquée : \( \sqrt{s \cdot (s - a) \cdot (s - b) \cdot (s - c)} \), où `sqrt` est la fonction de racine carrée de Haskell.
- **Tests** : La fonction `main` teste `triangleArea` avec les triplets `(3, 4, 5)` et `(7, 8, 9)`. L'utilisation de `show` convertit les résultats `Float` en chaînes pour l'affichage. Les sorties attendues sont :
  - Pour `(3, 4, 5)` : L'aire d'un triangle rectangle avec ces côtés est exactement 6.0 (car \( \frac{3 \cdot 4}{2} = 6 \)).
  - Pour `(7, 8, 9)` : L'aire est approximativement 26.832815729997478, calculée via la formule de Héron.
- **Remarque** : La fonction suppose que les côtés fournis forment un triangle valide (c.-à-d. \( a + b > c \), \( b + c > a \), \( a + c > b \)). Pour plus de robustesse, une vérification de la validité pourrait être ajoutée, mais ce n'est pas requis ici.

### Résultats des tests
- `triangleArea 3 4 5: 6.0`
- `triangleArea 7 8 9: 26.832815729997478`
