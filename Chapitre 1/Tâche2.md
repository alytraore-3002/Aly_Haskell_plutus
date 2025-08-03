-- HC1T2 - Tâche 2 : Exemple de fonction pure

```haskell
-- Calcul de l'aire d'un cercle à partir du rayon
circleArea :: Double -> Double
circleArea r = pi * r * r

-- Fonction principale pour tester
main :: IO ()
main = do
    let radius = 5.0
    putStrLn $ "Rayon du cercle: " ++ show radius
    putStrLn $ "Aire du cercle: " ++ show (circleArea radius)
```

### Explications :

1. **Fonction `circleArea`** :
   - **Signature** : `circleArea :: Double -> Double` indique que la fonction prend un nombre à virgule flottante comme rayon et retourne l'aire.
   - **Formule** : L'aire d'un cercle est calculée avec la formule `π * r²`. En Haskell, `pi` est une constante intégrée dans le prélude, représentant π (environ 3.14159).
   - **Pureté** : `circleArea` est pure car elle dépend uniquement de son argument `r` et de la constante `pi`, sans effets de bord (pas d'entrée/sortie, pas de dépendance à un état externe).

2. **Exemple d'exécution** :
   - Pour `circleArea 5.0` :
     - Aire = `π * 5.0 * 5.0` ≈ `3.14159 * 25` ≈ `78.53975`.

Sortie dans la console :
```
Rayon du cercle: 5.0
Aire du cercle: 78.53981633974483
```

### Remarques :
- **Pureté** : `circleArea` est pure, car elle produit toujours le même résultat pour la même entrée et ne dépend que de `r` et `pi`.
- **Simplicité** : La fonction est concise et suit le style fonctionnel de Haskell.
