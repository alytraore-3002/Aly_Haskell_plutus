HC8T8 : Synonymes de type et fonction de salutation

```haskell
-- Définitions des synonymes de type
type Name = String
type Age = Int

-- Fonction greet qui prend un Name et un Age et retourne une salutation
greet :: Name -> Age -> String
greet name age = "Bonjour, " ++ name ++ "! Vous avez " ++ show age ++ " ans."

-- Fonction main pour tester la fonction greet
main :: IO ()
main = do
  putStrLn $ greet "Alice" 30
  putStrLn $ greet "Bob" 25
```

**Explications :**
- Les synonymes de type `Name` et `Age` sont définis pour `String` et `Int` respectivement, à l'aide du mot-clé `type`.
- La fonction `greet` prend un `Name` (String) et un `Age` (Int) et construit une chaîne de salutation. La fonction `show` est utilisée pour convertir l'âge (`Int`) en `String` afin de le concaténer.
- La fonction `main` teste `greet` avec deux exemples : "Alice" (30 ans) et "Bob" (25 ans), en affichant les résultats avec `putStrLn`.
- 
