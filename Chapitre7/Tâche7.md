HC7T7 : Utiliser Bounded et Enum

```haskell
module Main where

-- Définition du type de données Color
data Color = Red | Green | Blue
  deriving (Show, Eq, Enum)

-- Fonction pour obtenir la couleur suivante
nextColor :: Color -> Color
nextColor color
  | color == Blue  = Red   -- Si on est à Blue, on revient à Red
  | otherwise      = succ color  -- Sinon, on prend la couleur suivante

-- Fonction main pour tester nextColor
main :: IO ()
main = do
  -- Tests avec chaque couleur
  putStrLn $ "Suivant de Red : " ++ show (nextColor Red)
  putStrLn $ "Suivant de Green : " ++ show (nextColor Green)
  putStrLn $ "Suivant de Blue : " ++ show (nextColor Blue)
```

### Explications du code :

1. **Module `Main`** :
   - `module Main where` déclare le module principal, qui contient la fonction d'entrée `main`.

2. **Type de données `Color`** :
   - `data Color = Red | Green | Blue` définit un type énuméré avec trois valeurs possibles : `Red`, `Green`, et `Blue`.
   - `deriving (Show, Eq, Enum)` :
     - `Show` permet de convertir les valeurs en chaînes pour l'affichage.
     - `Eq` permet les comparaisons d'égalité.
     - `Enum` permet d'utiliser `succ` (successeur) et `pred` (prédécesseur) pour naviguer dans l'ordre des valeurs, qui est implicite : `Red` < `Green` < `Blue`.

3. **Fonction `nextColor`** :
   - **Signature** : `nextColor :: Color -> Color` indique que la fonction prend une `Color` et retourne une `Color`.
   - **Implémentation** :
     - `| color == Blue = Red` : Si la couleur actuelle est `Blue` (la dernière), elle retourne `Red` (la première), créant un cycle.
     - `| otherwise = succ color` : Pour `Red` ou `Green`, `succ` donne la couleur suivante dans l'ordre (`Red` -> `Green`, `Green` -> `Blue`).
   - La logique cyclique est assurée par la condition explicite pour `Blue`.

4. **Fonction `main`** :
   - **Type** : `main :: IO ()` effectue des opérations d'entrée/sortie.
   - **Structure `do`** : Exécute plusieurs actions d'affichage.
   - **Tests** :
     - `putStrLn $ "Suivant de Red : " ++ show (nextColor Red)` teste la transition `Red` -> `Green`.
     - `putStrLn $ "Suivant de Green : " ++ show (nextColor Green)` teste `Green` -> `Blue`.
     - `putStrLn $ "Suivant de Blue : " ++ show (nextColor Blue)` teste `Blue` -> `Red`.
