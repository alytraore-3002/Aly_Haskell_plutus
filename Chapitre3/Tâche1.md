

### Code Haskell
```haskell
checkNumber :: Int -> String
checkNumber n = 
    if n > 0 
        then "Positif"
        else if n < 0 
            then "Négatif"
            else "Zéro"

-- Tests
main :: IO ()
main = do
    putStrLn $ "checkNumber 5: " ++ checkNumber 5      -- Affiche "checkNumber 5: Positif"
    putStrLn $ "checkNumber (-3): " ++ checkNumber (-3) -- Affiche "checkNumber (-3): Négatif"
    putStrLn $ "checkNumber 0: " ++ checkNumber 0       -- Affiche "checkNumber 0: Zéro"
```

### Explications
- **Définition de la fonction** : La fonction `checkNumber` a la signature de type `Int -> String`, ce qui signifie qu'elle prend un entier (`Int`) comme argument et retourne une chaîne de caractères (`String`).
- **Logique if-then-else** : La fonction utilise une instruction `if-then-else` imbriquée pour vérifier la valeur de l'entier `n` :
  - Si `n > 0`, elle retourne `"Positif"`.
  - Sinon, si `n < 0`, elle retourne `"Négatif"`.
  - Sinon (donc si `n == 0`), elle retourne `"Zéro"`.
- **Tests** : La fonction `main` est utilisée pour tester `checkNumber` avec les entrées 5, -3, et 0. L'utilisation de `putStrLn` avec la concaténation (`++`) permet d'afficher les résultats de manière claire, sans guillemets supplémentaires autour des chaînes, contrairement à `print`. Les sorties attendues sont :
  - `checkNumber 5: Positif`
  - `checkNumber (-3): Négatif`
  - `checkNumber 0: Zéro`
