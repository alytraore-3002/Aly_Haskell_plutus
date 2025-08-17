HC3T3 - Tâche 3 : Convertir une couleur RGB en chaîne hexadécimale avec let
### Code Haskell
```haskell
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let toHex n = 
            let hexDigits = "0123456789ABCDEF"
                clamped = max 0 (min 255 n) -- Limite les valeurs entre 0 et 255
                first = hexDigits !! (clamped `div` 16)
                second = hexDigits !! (clamped `mod` 16)
            in [first, second]
        rHex = toHex r
        gHex = toHex g
        bHex = toHex b
    in rHex ++ gHex ++ bHex

-- Tests
main :: IO ()
main = do
    putStrLn $ "rgbToHex (255, 0, 127): " ++ rgbToHex (255, 0, 127) -- Affiche "rgbToHex (255, 0, 127): FF007F"
    putStrLn $ "rgbToHex (0, 255, 64): " ++ rgbToHex (0, 255, 64)   -- Affiche "rgbToHex (0, 255, 64): 00FF40"
```

### Explications
- **Définition de la fonction** : La fonction `rgbToHex` a la signature de type `(Int, Int, Int) -> String`, prenant un triplet d'entiers (composants rouge, vert, bleu) et retournant une chaîne hexadécimale de 6 caractères.
- **Logique avec `let`** :
  - Une fonction auxiliaire `toHex` est définie dans un `let` pour convertir un entier en une chaîne hexadécimale à deux caractères :
    - `hexDigits` est une chaîne contenant les chiffres hexadécimaux (0-9, A-F).
    - `clamped` limite la valeur d'entrée entre 0 et 255 pour gérer les valeurs hors plage.
    - `first` est le caractère hexadécimal pour les 4 bits de poids fort (`div 16`).
    - `second` est le caractère hexadécimal pour les 4 bits de poids faible (`mod 16`).
    - La fonction retourne une liste de deux caractères `[first, second]`.
  - Les composants `r`, `g`, et `b` sont convertis en leurs représentations hexadécimales (`rHex`, `gHex`, `bHex`) en utilisant `toHex`.
  - Les trois chaînes de deux caractères sont concaténées avec `++` pour former la chaîne finale de 6 caractères.
- **Tests** : La fonction `main` teste `rgbToHex` avec les entrées `(255, 0, 127)` et `(0, 255, 64)`. Les sorties attendues sont :
  - `rgbToHex (255, 0, 127): FF007F` (255 = FF, 0 = 00, 127 = 7F)
  - `rgbToHex (0, 255, 64): 00FF40` (0 = 00, 255 = FF, 64 = 40)

### Remarque
- La fonction gère automatiquement les valeurs hors plage (négatives ou supérieures à 255) en les limitant à 0 ou 255 avec `max` et `min`.
- Les résultats sont des chaînes hexadécimales en majuscules, sans le préfixe `#`, conformément à la convention courante pour les couleurs RGB.
