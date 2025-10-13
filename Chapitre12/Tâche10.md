HC12T10 : Module d'opérations mathématiques
```haskell
-- Fonction pour calculer la puissance d'un nombre
power :: Double -> Double -> Double
power x n = x ** n

-- Fonction pour calculer la factorielle
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Fonction pour vérifier si un nombre est premier
isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | otherwise = not $ any (\x -> n `mod` x == 0) [2..floor (sqrt (fromIntegral n))]

-- Fonction pour calculer la somme des n premiers nombres
sumFirstN :: Integer -> Integer
sumFirstN n = sum [1..n]

-- Programme principal
main :: IO ()
main = do
  let x = 2.0
      n = 3.0
      num = 7
      count = 5

  putStrLn "Démonstration des opérations mathématiques :"
  putStrLn $ "Puissance : " ++ show x ++ "^" ++ show n ++ " = " ++ show (power x n)
  putStrLn $ "Factorielle de " ++ show num ++ " = " ++ show (factorial num)
  putStrLn $ show num ++ " est premier ? " ++ show (isPrime num)
  putStrLn $ "Somme des " ++ show count ++ " premiers nombres = " ++ show (sumFirstN count)
```

### Explication :

1. **Suppression du module** :
   - Dans le code initial, j'avais inclus `module MathOperations where` pour organiser les fonctions dans un module nommé `MathOperations`. Cependant, ton environnement a signalé une erreur ("there is no module name"), probablement parce qu'il ne gère pas correctement les modules ou parce que le fichier n'était pas nommé `MathOperations.hs`. En supprimant cette ligne, le code devient un programme Haskell autonome sans besoin de module, ce qui est plus compatible avec certains éditeurs en ligne ou environnements simplifiés.

2. **Fonctions mathématiques** :
   - `power x n` : Calcule \( x^n \) en utilisant l'opérateur `**` pour les nombres à virgule (type `Double`).
   - `factorial n` : Calcule la factorielle d'un entier (`Integer`) de manière récursive, avec un cas de base à 0 (factorielle de 0 = 1).
   - `isPrime n` : Vérifie si un nombre est premier en testant les diviseurs jusqu'à sa racine carrée. Utilise une garde (`|`) pour les cas spéciaux (n ≤ 1 ou n = 2) et `any` pour vérifier les divisibilités.
   - `sumFirstN n` : Calcule la somme des `n` premiers entiers en utilisant une liste `[1..n]` et la fonction `sum`.

3. **Fonction `main`** :
   - La fonction `main` est de type `IO ()`, ce qui signifie qu'elle effectue des opérations d'entrée/sortie. Elle utilise `do` pour exécuter une séquence d'instructions.
   - Des valeurs de test sont définies (`x = 2.0`, `n = 3.0`, `num = 7`, `count = 5`).
   - Les résultats des fonctions sont affichés avec `putStrLn`, en convertissant les nombres en chaînes avec `show` pour les concaténer.

   
