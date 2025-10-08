HC10T10 : Classe concaténable
```haskell
-- Définition de la classe de type Concatenatable
class Concatenatable a where
  concatWith :: a -> a -> a

-- Instance de Concatenatable pour [Char] (String)
instance Concatenatable [Char] where
  concatWith xs ys = xs ++ ys

-- Fonction main pour tester
main :: IO ()
main = do
  let str1 = "Hello, "
  let str2 = "World!"
  
  -- Test de concatWith
  putStrLn $ "Concaténation de str1 et str2: " ++ concatWith str1 str2  -- Devrait afficher : Hello, World!
```

### Explications :
1. **Classe `Concatenatable`** :
   - Définie comme `class Concatenatable a where`, avec une méthode `concatWith :: a -> a -> a`, qui prend deux valeurs de type `a` et retourne une nouvelle valeur de type `a` représentant leur concaténation.
   - Cette classe est conçue pour permettre la concaténation de structures de données de manière générique.

2. **Instance `Concatenatable [Char]`** :
   - Implémente `concatWith` pour `[Char]`, qui est le type `String` en Haskell (une liste de caractères).
   - `concatWith xs ys = xs ++ ys` utilise l'opérateur `++` (concaténation de listes) pour combiner les deux chaînes `xs` et `ys`.
   - Puisque `String` est équivalent à `[Char]`, cette implémentation fonctionne naturellement pour les chaînes de caractères.

3. **Fonction `main`** :
   - Définit deux chaînes : `str1 = "Hello, "` et `str2 = "World!"`.
   - Utilise `concatWith str1 str2` pour concaténer ces chaînes et affiche le résultat avec `putStrLn`.
   - Le résultat est affiché directement comme une chaîne, sans nécessiter `show`, car `concatWith` retourne déjà une `String`.
   - 
