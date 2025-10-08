 HC7T3 : Fonction avec contraintes multiples

```haskell
module Main where

-- Fonction compareValues qui compare deux valeurs de type a
-- où a doit être une instance de Eq et Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
    | x >= y    = x
    | otherwise = y

-- Fonction main pour tester compareValues
main :: IO ()
main = do
    -- Exemples avec des entiers
    print $ compareValues 5 3      -- Affiche 5
    print $ compareValues 2 7      -- Affiche 7
    -- Exemples avec des flottants
    print $ compareValues 3.14 2.71 -- Affiche 3.14
    -- Exemples avec des caractères
    print $ compareValues 'a' 'b'  -- Affiche 'b'
```

**Explications :**
- La contrainte `(Eq a, Ord a)` garantit que le type `a` supporte les opérations d'égalité (`Eq`) et d'ordre (`Ord`), nécessaires pour comparer les valeurs.
- La fonction `compareValues` utilise des gardes (`|`) pour retourner la plus grande valeur entre `x` et `y`.
- Le `main` teste la fonction avec des entiers, des flottants et des caractères pour montrer sa généricité.
- Vous pouvez compiler et exécuter ce code avec GHC (par exemple, `ghc fichier.hs` puis `./fichier`).
