HC12T5 : Vérification de palindrome
```haskell
import Data.Char (toLower, isAlpha)

-- Fonction pour vérifier si une chaîne est un palindrome
isPalindrome :: String -> Bool
isPalindrome str = let cleaned = [c | c <- map toLower str, isAlpha c]
                   in cleaned == reverse cleaned

-- Fonction principale avec des tests statiques
main :: IO ()
main = do
  let test1 = "radar"
  let test2 = "hello"
  putStrLn $ "Test avec '" ++ test1 ++ "': " ++ show (isPalindrome test1)
  putStrLn $ "Test avec '" ++ test2 ++ "': " ++ show (isPalindrome test2)
```

### Explication détaillée
1. **Importation (`import Data.Char (toLower, isAlpha)`)**
   - Cette ligne importe les fonctions `toLower` (pour convertir en minuscules) et `isAlpha` (pour vérifier si un caractère est une lettre) du module `Data.Char`. Ces fonctions sont nécessaires pour nettoyer la chaîne et la rendre insensible à la casse.

2. **Fonction `isPalindrome`**
   - **Signature** : `isPalindrome :: String -> Bool` indique que cette fonction prend une chaîne (`String`) et retourne un booléen (`Bool`, soit `True` soit `False`).
   - **Nettoyage de la chaîne** :
     - `map toLower str` convertit toute la chaîne en minuscules.
     - `[c | c <- map toLower str, isAlpha c]` utilise une liste par compréhension pour garder uniquement les caractères alphabétiques (`isAlpha c`) après conversion en minuscules. Par exemple, "radar" devient ["r", "a", "d", "a", "r"], et "hello" devient ["h", "e", "l", "l", "o"].
   - **Vérification** : `cleaned == reverse cleaned` compare la chaîne nettoyée avec sa version inversée. Si elles sont identiques, c'est un palindrome (`True`), sinon ce n'est pas un palindrome (`False`).
   - Exemple : Pour "radar", ["r", "a", "d", "a", "r"] == ["r", "a", "d", "a", "r"] donne `True`.

3. **Fonction `main`**
   - **Définition des tests** : `let test1 = "radar"` et `let test2 = "hello"` définissent deux chaînes statiques à tester.
     
