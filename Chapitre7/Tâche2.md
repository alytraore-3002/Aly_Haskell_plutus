HC7T2 : Implémenter une instance Ord pour un type personnalisé

```haskell
-- Définition du type Color
data Color = Red | Green | Blue deriving (Show)

-- Instance de Eq pour Color
instance Eq Color where
  Red == Red     = True
  Green == Green = True
  Blue == Blue   = True
  _ == _         = False

-- Instance de Ord pour Color avec l'ordre Red < Green < Blue
instance Ord Color where
  compare Red Green   = LT
  compare Red Blue    = LT
  compare Green Blue  = LT
  compare Red Red     = EQ
  compare Green Green = EQ
  compare Blue Blue   = EQ
  compare c1 c2       = compare c2 c1  -- Inverse pour les cas comme Green < Red

-- Fonction main pour tester les comparaisons
main :: IO ()
main = do
  putStrLn "Comparaisons de couleurs (Red < Green < Blue) :"
  print $ Red < Green    -- True
  print $ Green < Blue   -- True
  print $ Red < Blue     -- True
  print $ Green < Red    -- False
  print $ Blue < Green   -- False
  print $ Red == Red     -- True
  print $ Red > Blue     -- False
```

### Explications :
1. **Type `Color`** : Le type `Color` est défini avec trois constructeurs : `Red`, `Green` et `Blue`. L'annotation `deriving (Show)` permet d'afficher les valeurs de `Color` dans la console.
2. **Instance `Eq`** : La classe `Eq` est implémentée pour permettre les comparaisons d'égalité. Deux couleurs sont égales si elles sont identiques.
3. **Instance `Ord`** : La classe `Ord` définit l'ordre des couleurs avec `compare`. On spécifie explicitement que `Red < Green`, `Red < Blue`, et `Green < Blue`. Les cas d'égalité (`EQ`) sont définis pour chaque couleur avec elle-même. Pour les cas inverses (par exemple, `Green > Red`), on utilise `compare c2 c1` pour respecter la symétrie.
4. **Fonction `main`** : Le `main` teste diverses comparaisons (`<`, `==`, `>`) et affiche les résultats pour démontrer que l'ordre `Red < Green < Blue` est respecté.
5. 
