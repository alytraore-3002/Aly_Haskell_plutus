HC12T7 : Calculer l'aire d'un cercle
```haskell
-- Fonction pour calculer l'aire d'un cercle
calculateCircleArea :: Float -> Float
calculateCircleArea radius = pi * radius * radius

-- Fonction principale pour démontrer l'utilisation
main :: IO ()
main = do
  let radius1 = 5.0
  let radius2 = 3.0
  let area1 = calculateCircleArea radius1
  let area2 = calculateCircleArea radius2
  putStrLn $ "Aire du cercle avec rayon " ++ show radius1 ++ " : " ++ show area1
  putStrLn $ "Aire du cercle avec rayon " ++ show radius2 ++ " : " ++ show area2
```

### Explication détaillée
1. **Fonction `calculateCircleArea`**
   - **Signature** : `calculateCircleArea :: Float -> Float` indique que la fonction prend un rayon de type `Float` et retourne une aire de type `Float`.
   - **Calcul** : Utilise la constante `pi` (définie dans le prélude de Haskell) et multiplie \(\pi\) par le carré du rayon (`radius * radius`).
   - Exemple : Pour un rayon de 5.0, l'aire est approximativement \( 3.14159 \cdot 5^2 = 78.53975 \).

2. **Fonction `main`**
   - **Définition des rayons** : `radius1 = 5.0` et `radius2 = 3.0` sont des exemples de rayons statiques.
   - **Calcul des aires** : `area1 = calculateCircleArea radius1` et `area2 = calculateCircleArea radius2` appellent la fonction pour chaque rayon.
     
