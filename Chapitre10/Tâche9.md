HC10T9 : Classe de type MinMax:


```haskell
-- Définition de la classe de type MinMax
class MinMax a where
  minValue :: a
  maxValue :: a

-- Instance de MinMax pour Int
instance MinMax Int where
  minValue = minBound  -- Valeur minimale pour Int
  maxValue = maxBound  -- Valeur maximale pour Int

-- Fonction main pour tester
main :: IO ()
main = do
  putStrLn $ "Minimum value of Int: " ++ show (minValue :: Int)  -- Qualification explicite avec Int
  putStrLn $ "Maximum value of Int: " ++ show (maxValue :: Int)  -- Qualification explicite avec Int
```

### Explications des corrections :
1. **Erreur d'appel des méthodes** :
   - Dans le code original, `show minValue` et `show maxValue` tentaient d'utiliser `minValue` et `maxValue` comme des valeurs globales, ce qui est invalide. En Haskell, les méthodes d'une classe comme `MinMax` doivent être associées à une instance spécifique (ici, `MinMax Int`).
   - La correction consiste à ajouter une annotation de type explicite `(minValue :: Int)` et `(maxValue :: Int)` pour indiquer au compilateur que nous voulons utiliser les méthodes définies pour l'instance `MinMax Int`.

2. **Classe `MinMax`** :
   - Restée inchangée, elle définit `minValue :: a` et `maxValue :: a` comme des méthodes que chaque instance doit implémenter.

3. **Instance `MinMax Int`** :
   - Utilise `minBound` et `maxBound` (de la classe `Bounded`, dont `Int` est une instance) pour fournir les valeurs minimales et maximales. Ces valeurs sont correctes et dépendent de la taille de `Int` sur la plateforme (généralement 64 bits sur les systèmes modernes avec GHC).

4. **Fonction `main`** :
   - Maintenant, `putStrLn $ "Minimum value of Int: " ++ show (minValue :: Int)` et `putStrLn $ "Maximum value of Int: " ++ show (maxValue :: Int)` utilisent des annotations de type pour résoudre l'ambiguïté.
   - Cela garantit que le compilateur applique les méthodes `minValue` et `maxValue` à l'instance `MinMax Int`.
