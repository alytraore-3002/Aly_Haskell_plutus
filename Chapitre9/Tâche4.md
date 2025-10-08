HC9T4 : Extraire une valeur d'une Coffret

```haskell
-- Définition du type Box
data Box a = Empty | Value a deriving (Show)

-- Fonction extract qui renvoie la valeur dans la Box ou la valeur par défaut
extract :: a -> Box a -> a
extract defaultValue Empty = defaultValue      -- Si la boîte est vide, renvoie la valeur par défaut
extract _ (Value x) = x                       -- Si la boîte contient une valeur, renvoie cette valeur

-- Fonction main pour tester
main :: IO ()
main = do
    let box1 = Value 42       -- Une boîte contenant 42
    let box2 = Empty          -- Une boîte vide
    print $ extract 0 box1    -- Devrait afficher 42
    print $ extract 0 box2    -- Devrait afficher 0
```

### Explications :
1. **Type `Box`** : Le type `Box a` est un type algébrique avec deux constructeurs : `Empty` (boîte vide) et `Value a` (boîte contenant une valeur de type `a`).
2. **Fonction `extract`** :
   - Prend une valeur par défaut (`defaultValue`) et une `Box a`.
   - Si la boîte est `Empty`, renvoie `defaultValue`.
   - Si la boîte contient une valeur (`Value x`), renvoie `x`.
3. **Main** : Teste la fonction avec une boîte contenant 42 (renvoie 42) et une boîte vide (renvoie la valeur par défaut, 0).!
