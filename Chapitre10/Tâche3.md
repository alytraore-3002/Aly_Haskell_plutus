HC10T3 : Classe de type Comparable

```haskell
-- Définition du type Blockchain
data Blockchain = Blockchain
  { blockId :: Int         -- Identifiant unique du bloc (par exemple, timestamp ou numéro)
  } deriving (Show)

-- Définition de la classe de type Comparable
class Comparable a where
  compareWith :: a -> a -> Ordering

-- Instance de Comparable pour Blockchain
instance Comparable Blockchain where
  compareWith (Blockchain id1) (Blockchain id2) = compare id1 id2

-- Fonction main pour tester
main :: IO ()
main = do
  let block1 = Blockchain 100    -- Bloc avec ID 100
  let block2 = Blockchain 200    -- Bloc avec ID 200
  let block3 = Blockchain 100    -- Bloc avec ID 100
  
  -- Test de compareWith
  putStrLn $ "block1 vs block2: " ++ show (compareWith block1 block2)  -- Devrait afficher LT (Less Than)
  putStrLn $ "block2 vs block1: " ++ show (compareWith block2 block1)  -- Devrait afficher GT (Greater Than)
  putStrLn $ "block1 vs block3: " ++ show (compareWith block1 block3)  -- Devrait afficher EQ (Equal)
```

### Explications :
1. **Type `Blockchain`** :
   - Défini comme un type de données avec un seul champ `blockId :: Int`, représentant un identifiant unique (par exemple, un timestamp ou un numéro de bloc).
   - Le `deriving (Show)` permet d'afficher les valeurs de `Blockchain` pour le débogage.

2. **Classe `Comparable`** :
   - Déclare une méthode `compareWith :: a -> a -> Ordering`, qui prend deux valeurs de type `a` et retourne une valeur de type `Ordering` (qui peut être `LT` pour "less than", `EQ` pour "equal", ou `GT` pour "greater than").
   - Cette classe permet de comparer des instances de n'importe quel type qui implémente cette interface.

3. **Instance `Comparable Blockchain`** :
   - Implémente `compareWith` pour `Blockchain` en comparant les `blockId` des deux blocs à l'aide de la fonction `compare` standard de Haskell, qui retourne un `Ordering`.
   - La comparaison est basée sur `blockId`, ce qui est logique pour une blockchain où l'ordre des blocs (par exemple, par timestamp ou numéro) est important.

4. **Fonction `main`** :
   - Crée trois instances de `Blockchain` avec différents `blockId` : `block1` (100), `block2` (200), et `block3` (100).
   - Teste `compareWith` dans trois cas :
     - `block1` vs `block2` : Comme 100 < 200, retourne `LT`.
     - `block2` vs `block1` : Comme 200 > 100, retourne `GT`.
     - `block1` vs `block3` : Comme 100 = 100, retourne `EQ`.
   - Utilise `putStrLn` avec `show` pour afficher les résultats sous forme de chaînes lisibles.
   - 
