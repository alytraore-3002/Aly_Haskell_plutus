HC8T6 : Syntaxe d'enregistrement pour variantes Shape

```haskell
-- Définition du type Shape avec deux constructeurs pour Circle et Rectangle
data Shape = Circle { center :: (Float, Float), color :: String, radius :: Float }
           | Rectangle { center :: (Float, Float), color :: String, width :: Float, height :: Float }
           deriving (Show)

-- Création d'une instance pour un cercle
circle :: Shape
circle = Circle { center = (0.0, 0.0), color = "Red", radius = 5.0 }

-- Création d'une instance pour un rectangle
rectangle :: Shape
rectangle = Rectangle { center = (2.0, 3.0), color = "Blue", width = 4.0, height = 6.0 }

-- Fonction main pour afficher les instances
main :: IO ()
main = do
  print circle
  print rectangle
```

**Explications :**
- Le type `Shape` est défini comme un type algébrique de données avec deux constructeurs : `Circle` et `Rectangle`.
- `Circle` a les champs `center` (un tuple de `Float` pour les coordonnées), `color` (String) et `radius` (Float).
- `Rectangle` a les champs `center` (un tuple de `Float`), `color` (String), `width` (Float) et `height` (Float).
- `deriving (Show)` permet d'afficher les instances de `Shape` avec `print`.
- Une instance de `Circle` est créée avec un centre à (0,0), couleur rouge et rayon 5.
- Une instance de `Rectangle` est créée avec un centre à (2,3), couleur bleue, largeur 4 et hauteur 6.
- La fonction `main` affiche les deux instances dans la console.
- 
