HC9T5 : Type de données paramétré avec syntaxe d'enregistrement
```haskell
-- Définition du type de données paramétré Shape
data Shape a = Circle a | Rectangle a deriving (Show)

-- Fonction main pour tester
main :: IO ()
main = do
    let circle = Circle "Blue"        -- Un cercle avec une couleur de type String
    let rectangle = Rectangle "Red"   -- Un rectangle avec une couleur de type String
    print circle                      -- Affiche: Circle "Blue"
    print rectangle                   -- Affiche: Rectangle "Red"
```

### Explications :
1. **Type `Shape a`** :
   - `Shape` est un type de données paramétré qui prend un paramètre de type `a`.
   - Il a deux constructeurs : `Circle` et `Rectangle`, chacun prenant une valeur de type `a` pour représenter la couleur (`color`).
   - Le `deriving (Show)` permet d'afficher les valeurs de `Shape a` de manière lisible.
2. **Fonction `main`** :
   - Crée deux formes : un `Circle` avec la couleur `"Blue"` et un `Rectangle` avec la couleur `"Red"`.
   - Utilise `print` pour afficher ces formes dans la console.
