HC7T5 : Fonction utilisant la contrainte Num

```haskell
module Main where

-- Fonction qui inverse une liste de manière récursive
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Fonction main pour tester reverseList
main :: IO ()
main = do
  -- Test avec une liste d'entiers (type explicite)
  let list1 :: [Int] = [1, 2, 3, 4, 5]
  putStrLn $ "Liste originale : " ++ show list1
  putStrLn $ "Liste inversée : " ++ show (reverseList list1)
  
  -- Test avec une liste de caractères (type explicite)
  let list2 :: String = "hello"
  putStrLn $ "Liste originale : " ++ show list2
  putStrLn $ "Liste inversée : " ++ show (reverseList list2)
  
  -- Test avec une liste vide (type explicite)
  let list3 :: [Int] = []
  putStrLn $ "Liste originale : " ++ show list3
  putStrLn $ "Liste inversée : " ++ show (reverseList list3)
```

### Explication du code :

1. **Module `Main`** :
   - `module Main where` déclare le module principal du programme. En Haskell, chaque fichier source définit un module, et `Main` est le point d'entrée standard avec la fonction `main`.

2. **Fonction `reverseList`** :
   - **Signature** : `reverseList :: [a] -> [a]` indique que la fonction prend une liste de type quelconque `a` et retourne une liste du même type. Le type `a` est polymorphe grâce à la généricité de Haskell.
   - **Cas de base** : `reverseList [] = []` spécifie que l'inversion d'une liste vide reste une liste vide.
   - **Cas récursif** : `reverseList (x:xs) = reverseList xs ++ [x]` décompose la liste en tête `x` et queue `xs`. Elle appelle récursivement `reverseList` sur `xs`, puis concatène `[x]` à la fin avec l'opérateur `++`. Cela construit la liste inversée en ajoutant chaque élément à la fin.

3. **Fonction `main`** :
   - **Type** : `main :: IO ()` définit la fonction principale qui effectue des entrées/sorties (I/O) et ne retourne rien (`()`).
   - **Structure `do`** : Utilise une monade `IO` pour exécuter plusieurs actions d'affichage séquentiellement.
   - **Tests** :
     - **Liste d'entiers (`list1 :: [Int] = [1, 2, 3, 4, 5]`)** : Une annotation de type `[Int]` est ajoutée pour éviter l'ambiguïté. `putStrLn $ "Liste originale : " ++ show list1` affiche la liste originale, et `putStrLn $ "Liste inversée : " ++ show (reverseList list1)` affiche son inverse.
     - **Liste de caractères (`list2 :: String = "hello"`)** : `String` est un alias pour `[Char]`. L'annotation de type garantit que `show` utilise l'instance `Show` pour les chaînes.
     - **Liste vide (`list3 :: [Int] = []`)** : Teste le cas de base avec une liste vide, encore une fois avec une annotation pour clarifier le type.

4. **Utilisation de `show` et `++`** :
   - `show` convertit une valeur en une chaîne (nécessite une instance `Show`, fournie pour `[Int]` et `[Char]` par défaut).
   - `++` concatène des chaînes. L'expression `"Liste originale : " ++ show list1` combine une chaîne fixe avec la représentation textuelle de la liste.

### 
