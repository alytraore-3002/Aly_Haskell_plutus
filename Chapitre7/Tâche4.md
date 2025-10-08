HC7T4 : Type personnalisé avec Show et Read

```haskell
module Main where

-- Définition du type de données Shape
data Shape = Circle Double | Rectangle Double Double
  deriving (Eq) -- Dérivation de Eq pour comparaison, facultatif mais utile

-- Instance Show pour Shape
instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

-- Instance Read pour Shape
instance Read Shape where
  readsPrec _ input = 
    let 
      -- Fonction auxiliaire pour lire un Circle
      readCircle s = case words s of
        ("Circle":r:_) -> [(Circle (read r), "")]
        _ -> []
      -- Fonction auxiliaire pour lire un Rectangle
      readRectangle s = case words s of
        ("Rectangle":w:h:_) -> [(Rectangle (read w) (read h), "")]
        _ -> []
    in readCircle input ++ readRectangle input

-- Fonction main pour tester Show et Read
main :: IO ()
main = do
  -- Test de Show
  let circle = Circle 5.0
  let rectangle = Rectangle 4.0 6.0
  putStrLn $ show circle      -- Affiche: Circle 5.0
  putStrLn $ show rectangle   -- Affiche: Rectangle 4.0 6.0
  
  -- Test de Read
  let circleFromString = read "Circle 3.0" :: Shape
  let rectangleFromString = read "Rectangle 2.0 7.0" :: Shape
  putStrLn $ show circleFromString    -- Affiche: Circle 3.0
  putStrLn $ show rectangleFromString -- Affiche: Rectangle 2.0 7.0
```

### Explications :
1. **Type de données `Shape`** :
   - `Circle Double` prend un rayon (type `Double`).
   - `Rectangle Double Double` prend une largeur et une hauteur (deux `Double`).
   - `deriving (Eq)` est ajouté pour permettre la comparaison, bien que ce ne soit pas requis par l'énoncé.

2. **Instance `Show`** :
   - Convertit un `Shape` en une chaîne de caractères.
   - Pour `Circle r`, affiche `"Circle r"`.
   - Pour `Rectangle w h`, affiche `"Rectangle w h"`.

3. **Instance `Read`** :
   - Permet de parser une chaîne en un `Shape`.
   - Utilise `readsPrec` pour lire une chaîne comme `"Circle 3.0"` ou `"Rectangle 2.0 7.0"`.
   - `words` divise la chaîne en tokens, et les fonctions auxiliaires `readCircle` et `readRectangle` vérifient le format attendu et convertissent les valeurs en `Double`.

4. **Fonction `main`** :
   - Teste `Show` en affichant un `Circle` et un `Rectangle`.
   - Teste `Read` en parsant des chaînes pour recréer des `Shape` et les afficher.
   - 
