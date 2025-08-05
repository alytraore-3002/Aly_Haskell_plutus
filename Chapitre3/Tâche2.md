HC3T2 - Tâche 2 : Déterminer la note à partir d'un score avec des gardes

### Code Haskell
```haskell
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"

-- Tests
main :: IO ()
main = do
    putStrLn $ "grade 95: " ++ grade 95  -- Affiche "grade 95: A"
    putStrLn $ "grade 72: " ++ grade 72  -- Affiche "grade 72: C"
    putStrLn $ "grade 50: " ++ grade 50  -- Affiche "grade 50: F"
```

### Explications
- **Définition de la fonction** : La fonction `grade` a la signature de type `Int -> String`, prenant un entier (`score`) et retournant une chaîne de caractères (`String`) correspondant à la note.
- **Logique des gardes** : Les gardes (`|`) permettent d'évaluer les conditions dans l'ordre :
  - Si `score >= 90`, retourne `"A"`.
  - Si `score >= 80`, retourne `"B"`.
  - Si `score >= 70`, retourne `"C"`.
  - Si `score >= 60`, retourne `"D"`.
  - Sinon (`otherwise`), retourne `"F"`.
- **Tests** : La fonction `main` teste `grade` avec les valeurs 95, 72 et 50. L'utilisation de `putStrLn` avec la concaténation (`++`) affiche les résultats clairement. Les sorties attendues sont :
  - `grade 95: A`
  - `grade 72: C`
  - `grade 50: F`
