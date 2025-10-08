HC7T9 : définir une classe de type avec plusieurs instances

```haskell
module Main where

-- Définition du type de données Shape
data Shape = Circle Double | Rectangle Double Double
  deriving (Show)

-- Déclaration de la classe de type Descriptable
class Descriptable a where
  describe :: a -> String

-- Instance de Descriptable pour Bool
instance Descriptable Bool where
  describe True = "Vrai"
  describe False = "Faux"

-- Instance de Descriptable pour Shape
instance Descriptable Shape where
  describe (Circle r) = "Un cercle de rayon " ++ show r
  describe (Rectangle w h) = "Un rectangle de largeur " ++ show w ++ " et hauteur " ++ show h

-- Fonction main pour tester Descriptable
main :: IO ()
main = do
  -- Tests avec Bool
  putStrLn $ "Description de True : " ++ describe True
  putStrLn $ "Description de False : " ++ describe False
  
  -- Tests avec Shape
  let circle = Circle 5.0
  let rectangle = Rectangle 4.0 6.0
  putStrLn $ "Description de " ++ show circle ++ " : " ++ describe circle
  putStrLn $ "Description de " ++ show rectangle ++ " : " ++ describe rectangle
```

### Explications du code :

1. **Module `Main`** :
   - `module Main where` déclare le module principal, point d'entrée du programme.

2. **Type de données `Shape`** :
   - `data Shape = Circle Double | Rectangle Double Double` définit un type avec deux constructeurs : un cercle avec un rayon et un rectangle avec une largeur et une hauteur.
   - `deriving (Show)` permet d'afficher les valeurs de `Shape` (utile pour `main`).

3. **Classe de type `Descriptable`** :
   - `class Descriptable a where describe :: a -> String` définit une classe de type avec une méthode `describe` qui prend une valeur de type `a` et retourne une description sous forme de `String`.
   - Cette classe permet d'implémenter `describe` pour différents types.

4. **Instance de `Descriptable` pour `Bool`** :
   - `instance Descriptable Bool where` implémente `describe` pour le type `Bool`.
   - `describe True = "Vrai"` et `describe False = "Faux"` fournissent des descriptions en français pour les valeurs booléennes.
   - Cela montre comment personnaliser la description selon la valeur.

5. **Instance de `Descriptable` pour `Shape`** :
   - `instance Descriptable Shape where` implémente `describe` pour le type `Shape`.
   - `describe (Circle r) = "Un cercle de rayon " ++ show r` crée une description pour un cercle, utilisant `show` pour convertir le rayon en chaîne.
   - `describe (Rectangle w h) = "Un rectangle de largeur " ++ show w ++ " et hauteur " ++ show h` crée une description pour un rectangle, concaténant les dimensions.

6. **Fonction `main`** :
   - **Type** : `main :: IO ()` effectue des opérations d'entrée/sortie.
   - **Structure `do`** : Exécute plusieurs actions d'affichage.
   - **Tests** :
     - Pour `Bool` : Affiche les descriptions de `True` et `False`.
     - Pour `Shape` : Définit un `circle` et un `rectangle`, puis affiche leurs descriptions avec `show` pour montrer la valeur originale et `describe` pour la description textuelle.
