HC9T2 : Implémenter un type de données paramétré:

```haskell
-- Définition du type de données paramétrique Box
data Box a = Empty | Has a
  deriving (Show)

-- Exemples d'instances de Box
emptyBox :: Box Int
emptyBox = Empty

filledBox :: Box String
filledBox = Has "Trésor"

-- Fonction main pour afficher les instances
main :: IO ()
main = do
  putStrLn $ "Boîte vide : " ++ show emptyBox
  putStrLn $ "Boîte pleine : " ++ show filledBox
```

**Explications :**
- Le type de données paramétrique `Box a` est défini avec deux constructeurs :
  - `Empty` : représente une boîte vide.
  - `Has a` : représente une boîte contenant une valeur de type `a`.
- `deriving (Show)` permet d'afficher les instances de `Box` avec `show`.
- Deux instances sont créées :
  - `emptyBox` : une boîte vide de type `Box Int`.
  - `filledBox` : une boîte contenant la chaîne "Trésor" de type `Box String`.
- La fonction `main` affiche les deux instances en utilisant `putStrLn` et `show`.

