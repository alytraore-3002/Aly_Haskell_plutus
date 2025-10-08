HC8T3 : Types algébriques et fonctions :
```haskell
-- Définir le type Shape
data Shape = Circle Float | Rectangle Float Float
  deriving (Show)

-- Fonction pour calculer l'aire
area :: Shape -> Float
area (Circle r) = pi * r * r       -- Aire d'un cercle : π * r²
area (Rectangle w h) = w * h       -- Aire d'un rectangle : largeur * hauteur

-- Fonction main pour tester
main :: IO ()
main = do
  let circle = Circle 5.0
      rectangle = Rectangle 10.0 5.0
  putStrLn $ "Aire du cercle de rayon 5 : " ++ show (area circle)
  putStrLn $ "Aire du rectangle 10x5 : " ++ show (area rectangle)
```

### Explications :
1. **Type `Shape`** :
   - `data Shape = Circle Float | Rectangle Float Float` définit un type algébrique avec deux constructeurs :
     - `Circle Float` pour un cercle avec un rayon (`Float`).
     - `Rectangle Float Float` pour un rectangle avec une largeur et une hauteur (`Float`).
   - `deriving (Show)` permet d'afficher les valeurs de `Shape` de manière lisible.

2. **Fonction `area`** :
   - `area :: Shape -> Float` prend une forme et retourne son aire en `Float`.
   - Pour un `Circle r`, l'aire est calculée avec la formule `pi * r * r` (où `pi` est une constante définie dans le module `Prelude`).
   - Pour un `Rectangle w h`, l'aire est simplement `w * h` (largeur * hauteur).
   - Les motifs (`Circle r` et `Rectangle w h`) permettent de décomposer les constructeurs pour extraire les valeurs.

3. **Fonction `main`** :
   - Crée une instance `circle` avec un rayon de 5.0.
   - Crée une instance `rectangle` avec une largeur de 10.0 et une hauteur de 5.0.
   - Utilise `putStrLn` pour afficher les aires calculées, converties en chaîne avec `show`.
