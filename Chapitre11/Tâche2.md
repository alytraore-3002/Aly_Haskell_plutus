HC11T2 : Fonction fancyFunction pour WeAccept

```haskell
-- Définition de la typeclass WeAccept
class WeAccept a where
  accept :: a -> Bool

-- Définition des types
data Cardano = Cardano String Int deriving (Show, Eq)  -- e.g., Cardano "ADA" 1000
data Cash = Cash Double deriving (Show, Eq)            -- e.g., Cash 50.0
data Country = Country String Int deriving (Show, Eq)  -- e.g., Country "France" 67 (population in millions)

-- Instances de WeAccept pour chaque type
instance WeAccept Cardano where
  accept (Cardano _ amount) = amount > 500  -- Accepte si la quantité > 500

instance WeAccept Cash where
  accept (Cash value) = value >= 100.0      -- Accepte si la valeur >= 100.0

instance WeAccept Country where
  accept (Country _ pop) = pop > 50         -- Accepte si la population > 50 millions

-- Implémentation de la fonction fancyFunction
-- Cette fonction prend une liste hétérogène de valeurs (utilisant des listes de tuples pour simuler l'hétérogénéité)
-- et retourne une liste des valeurs acceptées (filtrées par accept)
fancyFunction :: (WeAccept a) => [(String, a)] -> [(String, a)]
fancyFunction items = filter (\(_, val) -> accept val) items

-- Fonction main pour tester avec différents types
main :: IO ()
main = do
  let cardanoItems = [("ADA1", Cardano "ADA" 600), ("ADA2", Cardano "ADA" 400)]
  let cashItems = [("Cash1", Cash 150.0), ("Cash2", Cash 80.0)]
  let countryItems = [("FR", Country "France" 67), ("CH", Country "Switzerland" 8)]

  putStrLn "Test avec Cardano :"
  print (fancyFunction cardanoItems)  -- [ ("ADA1", Cardano "ADA" 600) ]

  putStrLn "\nTest avec Cash :"
  print (fancyFunction cashItems)      -- [ ("Cash1", Cash 150.0) ]

  putStrLn "\nTest avec Country :"
  print (fancyFunction countryItems)   -- [ ("FR", Country "France" 67) ]
```

### Explications détaillées

#### 1. Rappel de la typeclass `WeAccept`
```haskell
class WeAccept a where
  accept :: a -> Bool
```
- **Description** : Typeclass générique définissant une méthode `accept` qui teste si une valeur de type `a` est acceptable (retourne `True` ou `False`).
- **Rôle** : Permet de définir des critères d'acceptation spécifiques pour différents types, favorisant la polymorphe via les instances.

#### 2. Définition des types
```haskell
data Cardano = Cardano String Int deriving (Show, Eq)
data Cash = Cash Double deriving (Show, Eq)
data Country = Country String Int deriving (Show, Eq)
```
- **Description** : 
  - `Cardano` : Représente une crypto-monnaie Cardano avec un nom (String) et une quantité (Int, e.g., ADA).
  - `Cash` : Représente de l'argent fiat avec une valeur (Double).
  - `Country` : Représente un pays avec un nom (String) et une population approximative (Int, en millions).
- **Dérivations** : `Show` pour l'affichage, `Eq` pour les comparaisons.
- **Rôle** : Types exemples pour démontrer l'utilisation polymorphe de `WeAccept` dans un contexte Haskell/Cardano.

#### 3. Instances de `WeAccept` pour chaque type
```haskell
instance WeAccept Cardano where
  accept (Cardano _ amount) = amount > 500

instance WeAccept Cash where
  accept (Cash value) = value >= 100.0

instance WeAccept Country where
  accept (Country _ pop) = pop > 50
```
- **Description** : Chaque instance implémente `accept` avec un critère spécifique :
  - `Cardano` : Accepte si la quantité d'ADA > 500.
  - `Cash` : Accepte si la valeur >= 100.0.
  - `Country` : Accepte si la population > 50 millions.
- **Fonctionnement** : Déconstruction du type pour extraire la valeur pertinente et application du prédicat.
- **Exemple** : `accept (Cardano "ADA" 600)` retourne `True` ; `accept (Cash 80.0)` retourne `False`.

#### 4. Implémentation de la fonction `fancyFunction`
```haskell
fancyFunction :: (WeAccept a) => [(String, a)] -> [(String, a)]
fancyFunction items = filter (\(_, val) -> accept val) items
```
- **Signature** : Contrainte par `WeAccept a`, prend une liste de paires (label String, valeur a) et retourne les paires acceptées.
- **Fonctionnement** :
  - Utilise `filter` pour conserver les paires où `accept` sur la seconde composante retourne `True`.
  - La lambda `(\(_, val) -> accept val)` ignore le label et teste la valeur.
  - Polymorphe : Fonctionne pour n'importe quel type `a` ayant une instance `WeAccept`.
- **Pureté** : Fonction pure, sans effets secondaires.
- **Exemple** : Pour `cardanoItems`, retourne `[("ADA1", Cardano "ADA" 600)]`.

### Résumé
- **Typeclass `WeAccept`** : `accept :: a -> Bool` pour tester l'acceptabilité.
- **Types** : `Cardano` (crypto), `Cash` (argent), `Country` (pays) avec instances spécifiques (e.g., quantité > 500 pour Cardano).
- **Fonction `fancyFunction`** : Filtre polymorphe une liste de paires (label, valeur) en ne gardant que les valeurs acceptées.
- **Fonction `main`** : Teste `fancyFunction` séparément sur des listes pour chaque type, affichant les résultats via IO.
- **Code** : Complet, testable, et étend le pattern précédent avec de nouveaux types pour illustrer la réutilisabilité.
