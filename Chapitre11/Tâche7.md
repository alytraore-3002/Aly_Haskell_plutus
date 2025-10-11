HC11T7 : Instance Ord pour Box

```haskell
module Main where

-- Définition du type Box
data Box a = Box a | EmptyBox deriving (Show, Eq)

-- Instance de Ord pour Box
instance (Ord a) => Ord (Box a) where
  compare EmptyBox EmptyBox = EQ
  compare EmptyBox (Box _) = LT
  compare (Box _) EmptyBox = GT
  compare (Box x) (Box y) = compare x y

-- Fonction main pour tester
main :: IO ()
main = do
  let box1 = Box 5 :: Box Int
      box2 = Box 10 :: Box Int
      box3 = EmptyBox :: Box Int
      box4 = Box 5 :: Box Int

  putStrLn "Test de compare :"
  print $ compare box1 box2  -- LT
  print $ compare box2 box1  -- GT
  print $ compare box1 box4  -- EQ
  print $ compare box3 box1  -- LT
  print $ compare box1 box3  -- GT
  print $ compare box3 box3  -- EQ
```

### Explications détaillées

#### 1. Définition du type `Box`
```haskell
data Box a = Box a | EmptyBox deriving (Show, Eq)
```
- **Description** : `Box` est un type algébrique générique avec deux constructeurs :
  - `Box a` : Contient une valeur de type `a`.
  - `EmptyBox` : Représente un conteneur vide.
- **Dérivations** :
  - `Show` : Pour afficher les valeurs (e.g., `Box 5`, `EmptyBox`).
  - `Eq` : Nécessaire car `Ord` étend `Eq`, donc `Box` doit supporter l'égalité `==`.
- **Rôle** : Modélise un conteneur simple pouvant être vide ou contenir une valeur, adapté à l'implémentation de l'instance `Ord`.

#### 2. Instance de `Ord` pour `Box`
```haskell
instance (Ord a) => Ord (Box a) where
  compare EmptyBox EmptyBox = EQ
  compare EmptyBox (Box _) = LT
  compare (Box _) EmptyBox = GT
  compare (Box x) (Box y) = compare x y
```
- **Signature** : `instance (Ord a) => Ord (Box a)` indique que `Box a` est ordonnable si `a` l’est (`Ord a`). C'est la correction clé pour résoudre l'erreur de kind (`* -> *` pour `Box`), en appliquant `Box` à un type `a`.
- **Fonctionnement** :
  - **Cas 1 : `EmptyBox` vs `EmptyBox`** : Deux boîtes vides sont égales, retourne `EQ`.
  - **Cas 2 : `EmptyBox` vs `Box _`** : Une boîte vide est inférieure à une boîte non vide, retourne `LT`.
  - **Cas 3 : `Box _` vs `EmptyBox`** : Une boîte non vide est supérieure à une boîte vide, retourne `GT`.
  - **Cas 4 : `Box x` vs `Box y`** : Compare les valeurs contenues `x` et `y` en utilisant la fonction `compare` du type `a` (grâce à la contrainte `Ord a`).
- **Exemples** :
  - `compare (Box 5) (Box 10)` → `LT` (car `5 < 10`).
  - `compare (Box 5) (Box 5)` → `EQ` (car `5 == 5`).
  - `compare EmptyBox (Box 5)` → `LT`.
  - `compare (Box 5) EmptyBox` → `GT`.
  - `compare EmptyBox EmptyBox` → `EQ`.
- **Logique** : `EmptyBox` est considéré comme l'élément le plus petit, et pour les boîtes non vides, l'ordre dépend des valeurs contenues. Cela définit un ordre total cohérent.

### Résumé
- **Type `Box`** : `data Box a = Box a | EmptyBox`, un conteneur générique pouvant être vide ou contenir une valeur.
- **Instance `Ord` pour `Box`** : Implémentée pour `Box a` avec contrainte `Ord a`, utilisant `compare` pour définir `EmptyBox` < `Box _` et comparer les valeurs contenues.
- **Fonction `main`** : Teste `compare` sur des `Box Int` pour les cas d'infériorité, égalité, supériorité et vide, affichant les résultats via IO.
- **Code** : Complet, testable, et respecte les concepts du cours (pureté, généricité, IO).
