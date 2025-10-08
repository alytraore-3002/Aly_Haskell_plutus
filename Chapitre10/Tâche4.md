
HC10T4 : Instance Eq pour Box:
```haskell
-- Définition du type paramétrique Box
data Box a = Box a deriving (Show)

-- Instance de Eq pour Box
instance Eq a => Eq (Box a) where
  (Box x) == (Box y) = x == y

-- Fonction main pour tester
main :: IO ()
main = do
  let box1 = Box 5         -- Boîte contenant 5
  let box2 = Box 5         -- Boîte contenant 5
  let box3 = Box 10        -- Boîte contenant 10
  
  -- Test de l'égalité
  putStrLn $ "box1 == box2: " ++ show (box1 == box2)  -- Devrait afficher True
  putStrLn $ "box1 == box3: " ++ show (box1 == box3)  -- Devrait afficher False
  putStrLn $ "box2 == box3: " ++ show (box2 == box3)  -- Devrait afficher False
```

### Explications :
1. **Type `Box a`** :
   - `Box a` est un type paramétrique qui encapsule une valeur de type `a` dans un constructeur `Box`.
   - Le `deriving (Show)` permet d'afficher les valeurs de `Box` (par exemple, `Box 5`) pour le débogage.

2. **Instance `Eq (Box a)`** :
   - Pour que `Box a` soit une instance de `Eq`, il faut que le type `a` soit aussi une instance de `Eq` (d'où la contrainte `Eq a =>`).
   - L'implémentation de `(==)` compare deux boîtes `Box x` et `Box y` en comparant leurs contenus `x` et `y` avec l'opérateur `==` de `a`. Cela signifie que l'égalité de deux boîtes dépend uniquement de l'égalité de leurs valeurs internes.
   - La classe `Eq` fournit aussi `/=`, qui est dérivé automatiquement comme complémentaire de `==`.

3. **Fonction `main`** :
   - Crée trois instances de `Box` : `box1` et `box2` contenant 5, et `box3` contenant 10.
   - Teste l'égalité avec `==` dans trois cas :
     - `box1 == box2` : Vrai, car 5 == 5.
     - `box1 == box3` : Faux, car 5 != 10.
     - `box2 == box3` : Faux, car 5 != 10.
   - Utilise `putStrLn` avec `show` pour afficher les résultats sous forme de chaînes lisibles (`True` ou `False`).
