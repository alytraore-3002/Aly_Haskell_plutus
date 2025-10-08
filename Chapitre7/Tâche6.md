HC7T6 : Utiliser Intégral et Flottant

```haskell
module Main where

-- Fonction pour calculer la circonférence d'un cercle
circleCircumference :: (Real a) => a -> Double
circleCircumference r = 2 * pi * realToFrac r

-- Fonction main pour tester circleCircumference
main :: IO ()
main = do
  -- Test avec un type Integral (Int)
  let radiusInt :: Int = 5
  putStrLn $ "Circonférence avec rayon " ++ show radiusInt ++ " (Int) : " ++ show (circleCircumference radiusInt)
  
  -- Test avec un type Floating (Double)
  let radiusDouble :: Double = 3.5
  putStrLn $ "Circonférence avec rayon " ++ show radiusDouble ++ " (Double) : " ++ show (circleCircumference radiusDouble)
```

### Explications du code :

1. **Module `Main`** :
   - `module Main where` déclare le module principal du programme. En Haskell, c'est le point d'entrée standard avec la fonction `main`.

2. **Fonction `circleCircumference`** :
   - **Signature** : `circleCircumference :: (Real a) => a -> Double`.
     - `(Real a)` : La contrainte `Real` indique que le paramètre `a` peut être un type numérique réel, comme `Int`, `Integer`, `Float`, ou `Double`. Cela inclut les types `Integral` (entiers) et `Floating` (nombres à virgule).
     - `a -> Double` : La fonction prend un argument de type `a` et retourne un résultat de type `Double` pour une précision constante.
   - **Implémentation** : `circleCircumference r = 2 * pi * realToFrac r`.
     - `pi` : Constante approximative de π (valeur par défaut dans `Prelude`, environ 3.14159), compatible avec les types `Floating`.
     - `realToFrac r` : Convertit le paramètre `r` (de type `Real`) en un type compatible avec `Double`. Cela fonctionne pour les entiers (`Integral`) en les convertissant en décimal et pour les nombres à virgule (`Floating`) en les laissant tels quels.
     - `2 * pi * realToFrac r` : Calcule la circonférence avec la formule `C = 2 * π * r`.

3. **Fonction `main`** :
   - **Type** : `main :: IO ()` définit la fonction principale qui effectue des opérations d'entrée/sortie (I/O) et ne retourne rien (`()`).
   - **Structure `do`** : Utilise une monade `IO` pour exécuter plusieurs actions d'affichage séquentiellement.
   - **Tests** :
     - **Avec `Int`** : `let radiusInt :: Int = 5` définit un rayon entier. `putStrLn $ "Circonférence avec rayon " ++ show radiusInt ++ " (Int) : " ++ show (circleCircumference radiusInt)` affiche la circonférence pour un rayon de 5.
     - **Avec `Double`** : `let radiusDouble :: Double = 3.5` définit un rayon à virgule. `putStrLn $ "Circonférence avec rayon " ++ show radiusDouble ++ " (Double) : " ++ show (circleCircumference radiusDouble)` affiche la circonférence pour un rayon de 3.5.
   - **Utilisation de `show`** : Convertit les valeurs en chaînes pour les afficher avec `putStrLn`.
   - **Concaténation avec `++`** : Combine les chaînes de texte avec les valeurs converties.
