

1. **Fonction `add`**
   - **Description** : Prend deux `Int` et retourne leur somme.
   - **Signature de type** : `add :: Int -> Int -> Int`
     - La fonction prend deux arguments de type `Int` et retourne un `Int`.
   - **Implémentation** :
     ```haskell
     add :: Int -> Int -> Int
     add x y = x + y
     ```
     - Explication : Utilise l'opérateur `+` pour additionner `x` et `y`.

2. **Fonction `isEven`**
   - **Description** : Prend un `Int` et retourne un `Bool` indiquant si le nombre est pair.
   - **Signature de type** : `isEven :: Int -> Bool`
     - La fonction prend un argument de type `Int` et retourne un `Bool`.
   - **Implémentation** :
     ```haskell
     isEven :: Int -> Bool
     isEven x = x `mod` 2 == 0
     ```
     - Explication : Vérifie si le reste de la division de `x` par 2 est 0 pour déterminer si le nombre est pair.

3. **Fonction `concatStrings`**
   - **Description** : Prend deux `String` et retourne leur concaténation.
   - **Signature de type** : `concatStrings :: String -> String -> String`
     - La fonction prend deux arguments de type `String` et retourne un `String`.
   - **Implémentation** :
     ```haskell
     concatStrings :: String -> String -> String
     concatStrings s1 s2 = s1 ++ s2
     ```
     - Explication : Utilise l'opérateur `++` pour concaténer les chaînes `s1` et `s2`.

### Code complet avec `main`
Voici le code Haskell complet, incluant les trois fonctions et une fonction `main` qui démontre leur utilisation en affichant des exemples de résultats à l'aide de `putStrLn` et `print` :

```haskell
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

main :: IO ()
main = do
    putStrLn "Test de la fonction add :"
    print (add 3 4)  -- Affiche 7
    putStrLn "Test de la fonction isEven :"
    print (isEven 4)  -- Affiche True
    print (isEven 3)  -- Affiche False
    putStrLn "Test de la fonction concatStrings :"
    putStrLn (concatStrings "Hello " "World!")  -- Affiche "Hello, World!"
```

### Explications de `main`
- La fonction `main` est de type `IO ()`, car elle effectue des actions d'entrée/sortie (affichage dans la console).
- `putStrLn` est utilisé pour afficher des messages descriptifs et le résultat de `concatStrings` (qui est un `String`).
- `print` est utilisé pour afficher les résultats de `add` (un `Int`) et `isEven` (un `Bool`), car il convertit automatiquement les valeurs non-`String` en chaînes pour l'affichage.

### Exécution
Pour exécuter ce programme :
1. Enregistrez le code dans un fichier, par exemple `Main.hs`.
2. Compilez-le avec `ghc Main.hs` et exécutez le programme généré.
3. L'exécution produira une sortie comme :
   ```
   Test de la fonction add :
   7
   Test de la fonction isEven :
   True
   False
   Test de la fonction concatStrings :
   Hello World!
   ```
