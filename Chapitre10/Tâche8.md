HC10T8 : Sous-classe AdvancedEq de Eq
```haskell
-- Définition du type Pair
data Pair a b = Pair a b deriving (Show)

-- Définition de la classe AdvancedEq (héritant de Eq)
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

-- Instance de Eq pour Pair
instance (Eq a, Eq b) => Eq (Pair a b) where
  (Pair x1 y1) == (Pair x2 y2) = x1 == x2 && y1 == y2

-- Instance de AdvancedEq pour Pair
instance (Eq a, Eq b) => AdvancedEq (Pair a b) where
  compareEquality (Pair x1 y1) (Pair x2 y2) = x1 == x2 && y1 == y2

-- Fonction main pour tester
main :: IO ()
main = do
  let pair1 = Pair 1 "hello"    -- Paire avec 1 et "hello"
  let pair2 = Pair 1 "hello"    -- Même paire
  let pair3 = Pair 2 "world"    -- Paire différente
  
  -- Test de == (hérité de Eq)
  putStrLn $ "pair1 == pair2: " ++ show (pair1 == pair2)  -- Devrait afficher True
  putStrLn $ "pair1 == pair3: " ++ show (pair1 == pair3)  -- Devrait afficher False
  
  -- Test de compareEquality (de AdvancedEq)
  putStrLn $ "compareEquality pair1 pair2: " ++ show (compareEquality pair1 pair2)  -- Devrait afficher True
  putStrLn $ "compareEquality pair1 pair3: " ++ show (compareEquality pair1 pair3)  -- Devrait afficher False
```

### Explications :
1. **Type `Pair a b`** :
   - Défini comme un type paramétrique qui contient deux valeurs de types différents `a` et `b` (par exemple, un entier et une chaîne).
   - Le `deriving (Show)` permet d'afficher les paires (par exemple, `Pair 1 "hello"`) pour le débogage.

2. **Classe `AdvancedEq`** :
   - Définie comme `class Eq a => AdvancedEq a`, ce qui signifie que tout type qui implémente `AdvancedEq` doit aussi implémenter `Eq`. Cela simule une sous-classe, car `AdvancedEq` hérite des méthodes de `Eq` (`==` et `/=`).
   - Ajoute une méthode `compareEquality :: a -> a -> Bool`, qui retourne un booléen indiquant si deux valeurs sont égales selon une logique potentiellement différente de `==`.

3. **Instance `Eq (Pair a b)`** :
   - Implémente `==` pour `Pair` avec la contrainte `(Eq a, Eq b)`, ce qui signifie que les types `a` et `b` doivent supporter l'égalité.
   - `(Pair x1 y1) == (Pair x2 y2)` vérifie que `x1 == x2` et `y1 == y2`, comparant les deux composantes de la paire.

4. **Instance `AdvancedEq (Pair a b)`** :
   - Implémente `compareEquality` pour `Pair` avec la même contrainte `(Eq a, Eq b)`.
   - `(Pair x1 y1) `compareEquality` (Pair x2 y2)` utilise la même logique que `==` (c'est-à-dire `x1 == x2 && y1 == y2`), mais cela pourrait être personnalisé différemment si besoin (par exemple, ignorer une composante).
   - Ici, j'ai choisi de rendre `compareEquality` identique à `==` pour simplifier, mais tu peux le modifier pour une logique différente.

5. **Fonction `main`** :
   - Crée trois instances de `Pair` :
     - `pair1` : `Pair 1 "hello"`.
     - `pair2` : `Pair 1 "hello"` (identique à `pair1`).
     - `pair3` : `Pair 2 "world"` (différent de `pair1`).
   - Teste :
     - `==` (hérité de `Eq`) pour vérifier l'égalité standard.
     - `compareEquality` (de `AdvancedEq`) pour tester la méthode supplémentaire.
   - Utilise `putStrLn` avec `show` pour afficher les résultats (`True` ou `False`).
   - 
