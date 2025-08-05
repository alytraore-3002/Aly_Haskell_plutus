HC3T10 - Tâche avancée 10 : Vérifier si une chaîne est un palindrome (récursion, gardes)

### Définition de la fonction `isPalindrome` en Haskell qui vérifie si une chaîne est un palindrome en utilisant des gardes et une approche récursive, suivie des tests.

### Code Haskell
```haskell
isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1                 = True
    | head str == last str            = isPalindrome (tail (init str))
    | otherwise                       = False

-- Tests
main :: IO ()
main = do
    putStrLn $ "isPalindrome \"racecar\": " ++ show (isPalindrome "racecar")  -- Affiche "isPalindrome \"racecar\": True"
    putStrLn $ "isPalindrome \"haskell\": " ++ show (isPalindrome "haskell")  -- Affiche "isPalindrome \"haskell\": False"
    putStrLn $ "isPalindrome \"madam\": " ++ show (isPalindrome "madam")      -- Affiche "isPalindrome \"madam\": True"
```

### Explications
- **Définition de la fonction** : La fonction `isPalindrome` a la signature de type `String -> Bool`, prenant une chaîne de caractères et retournant un booléen indiquant si la chaîne est un palindrome (se lit de la même manière dans les deux sens).
- **Logique des gardes** :
  - `| length str <= 1` : Si la chaîne est vide (`length str == 0`) ou a un seul caractère (`length str == 1`), elle est considérée comme un palindrome, donc retourne `True`.
  - `| head str == last str` : Si le premier caractère (`head str`) est égal au dernier caractère (`last str`), la fonction appelle récursivement `isPalindrome` sur la sous-chaîne obtenue en enlevant le premier et le dernier caractère (`tail (init str)`).
  - `| otherwise` : Si les caractères aux extrémités ne correspondent pas, la chaîne n'est pas un palindrome, donc retourne `False`.
- **Tests** : La fonction `main` teste `isPalindrome` avec les chaînes `"racecar"`, `"haskell"`, et `"madam"`. L'utilisation de `show` convertit les booléens en chaînes pour l'affichage. Les sorties attendues sont :
  - Pour `"racecar"` :
    - `head "racecar" == 'r'`, `last "racecar" == 'r'`, donc vérifie `isPalindrome "aceca"`.
    - `head "aceca" == 'a'`, `last "aceca" == 'a'`, donc vérifie `isPalindrome "cec"`.
    - `head "cec" == 'c'`, `last "cec" == 'c'`, donc vérifie `isPalindrome "e"`.
    - `length "e" == 1`, donc `True`.
    - Résultat : `True`.
  - Pour `"haskell"` :
    - `head "haskell" == 'h'`, `last "haskell" == 'l'`, donc `False` (car `'h' /= 'l'`).
    - Résultat : `False`.
  - Pour `"madam"` :
    - `head "madam" == 'm'`, `last "madam" == 'm'`, donc vérifie `isPalindrome "ada"`.
    - `head "ada" == 'a'`, `last "ada" == 'a'`, donc vérifie `isPalindrome "d"`.
    - `length "d" == 1`, donc `True`.
    - Résultat : `True`.
- **Résultats des tests** :
  - `isPalindrome "racecar": True`
  - `isPalindrome "haskell": False`
  - `isPalindrome "madam": True`

### Remarque
- La fonction suppose que la chaîne contient des caractères valides. Elle est sensible à la casse (par exemple, `"Madam"` ne serait pas considéré comme un palindrome).
- L'approche récursive avec `tail` et `init` est utilisée pour respecter les exigences, bien qu'une alternative courante serait de comparer la chaîne avec son inverse (`str == reverse str`).
