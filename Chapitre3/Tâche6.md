HC3T6 - Tâche avancée 6 : Vérifier une année bissextile avec if-then-else

### Code Haskell
```haskell
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0
        then True
        else if year `mod` 100 == 0
            then False
            else if year `mod` 4 == 0
                then True
                else False

-- Tests
main :: IO ()
main = do
    putStrLn $ "isLeapYear 2000: " ++ show (isLeapYear 2000)  -- Affiche "isLeapYear 2000: True"
    putStrLn $ "isLeapYear 1900: " ++ show (isLeapYear 1900)  -- Affiche "isLeapYear 1900: False"
    putStrLn $ "isLeapYear 2024: " ++ show (isLeapYear 2024)  -- Affiche "isLeapYear 2024: True"
```

### Explications
- **Définition de la fonction** : La fonction `isLeapYear` a la signature de type `Int -> Bool`, prenant une année (entier) et retournant un booléen indiquant si l'année est bissextile.
- **Logique if-then-else** :
  - Vérifie d'abord si l'année est divisible par 400 (`year mod 400 == 0`). Si oui, retourne `True` (bissextile).
  - Sinon, vérifie si l'année est divisible par 100 (`year mod 100 == 0`). Si oui, retourne `False` (non bissextile).
  - Sinon, vérifie si l'année est divisible par 4 (`year mod 4 == 0`). Si oui, retourne `True` (bissextile).
  - Sinon, retourne `False` (non bissextile).
- **Tests** : La fonction `main` teste `isLeapYear` avec les années 2000, 1900 et 2024. L'utilisation de `show` convertit les booléens en chaînes pour l'affichage. Les sorties attendues sont :
  - `isLeapYear 2000: True` (divisible par 400).
  - `isLeapYear 1900: False` (divisible par 100 mais pas par 400).
  - `isLeapYear 2024: True` (divisible par 4, non divisible par 100).

### Résultats des tests
- `isLeapYear 2000: True`
- `isLeapYear 1900: False`
- `isLeapYear 2024: True`
