HC11T10 : Fonction sortContainers

```haskell
module Main where

import Data.List (sort)  -- Import explicite de sort

-- Définition du type Box
data Box a = Box a | EmptyBox deriving (Show, Eq)

-- Instance de Ord pour Box
instance (Ord a) => Ord (Box a) where
  compare EmptyBox EmptyBox = EQ
  compare EmptyBox (Box _) = LT
  compare (Box _) EmptyBox = GT
  compare (Box x) (Box y) = compare x y

-- Fonction sortContainers
sortContainers :: (Ord a) => [Box a] -> [Box a]
sortContainers = sort

-- Fonction main pour tester
main :: IO ()
main = do
  let boxes = [Box 5, EmptyBox, Box 2, Box 10, EmptyBox, Box 7] :: [Box Int]

  putStrLn "Liste avant tri :"
  print boxes

  putStrLn "Liste après tri :"
  print $ sortContainers boxes
```

### Explications détaillées

#### 1. Définition du type `Box`
```haskell
data Box a = Box a | EmptyBox deriving (Show, Eq)
```
- **Description** : `Box` est un type algébrique générique avec deux constructeurs :
  - `Box a` : Contient une valeur de type `a` (e.g., `Box 5`).
  - `EmptyBox` : Représente un conteneur vide.
- **Dérivations** :
  - `Show` : Permet d'afficher les valeurs (e.g., `Box 5`, `EmptyBox`) pour une sortie lisible.
  - `Eq` : Nécessaire car `Ord` étend `Eq`, permettant la comparaison d'égalité entre `Box`.
- **Rôle** : Sert de conteneur générique pour tester la fonction `sortContainers` avec l'instance `Ord`.

#### 2. Instance de `Ord` pour `Box`
```haskell
instance (Ord a) => Ord (Box a) where
  compare EmptyBox EmptyBox = EQ
  compare EmptyBox (Box _) = LT
  compare (Box _) EmptyBox = GT
  compare (Box x) (Box y) = compare x y
```
- **Signature** : `instance (Ord a) => Ord (Box a)` définit l'instance pour `Box a`, où `a` doit être ordonnable (`Ord a`). Cela corrige le problème de kind (`* -> *`) en appliquant `Box` à un type concret.
- **Fonctionnement** :
  - **Cas 1 : `EmptyBox` vs `EmptyBox`** : Deux boîtes vides sont égales, retourne `EQ` (Equal).
  - **Cas 2 : `EmptyBox` vs `Box _`** : Une boîte vide est considérée comme inférieure à une boîte non vide, retourne `LT` (Less Than).
  - **Cas 3 : `Box _` vs `EmptyBox`** : Une boîte non vide est supérieure à une boîte vide, retourne `GT` (Greater Than).
  - **Cas 4 : `Box x` vs `Box y`** : Compare les valeurs contenues `x` et `y` en utilisant la fonction `compare` du type `a`, qui doit être une instance de `Ord`.
- **Exemples** :
  - `compare (Box 5) (Box 10)` → `LT` (car `5 < 10`).
  - `compare EmptyBox (Box 5)` → `LT`.
  - `compare (Box 5) EmptyBox` → `GT`.
  - `compare EmptyBox EmptyBox` → `EQ`.
- **Logique** : Cette définition établit un ordre total où `EmptyBox` est le plus petit élément, suivi des `Box` triés par leurs valeurs croissantes. Cela permet un tri cohérent avec `sort`.

#### 3. Fonction `sortContainers`
```haskell
sortContainers :: (Ord a) => [Box a] -> [Box a]
sortContainers = sort
```
- **Signature** : Prend une liste de `Box a` où `a` est ordonnable (`Ord a`) et retourne une liste triée de `Box a`.
- **Fonctionnement** :
  - Utilise la fonction `sort` de `Data.List`, qui trie une liste en s'appuyant sur l'instance `Ord` du type des éléments.
  - Grâce à l'instance `Ord (Box a)`, `sort` applique l'ordre défini dans `compare` : tous les `EmptyBox` apparaissent en premier (car `LT`), suivis des `Box` triés par leurs valeurs croissantes.
- **Dépendance** : L'import `import Data.List (sort)` garantit que `sort` est disponible, évitant l'erreur "Not in scope".
- **Exemples** :
  - `sortContainers [Box 5, EmptyBox, Box 2]` → `[EmptyBox, Box 2, Box 5]`.
  - `sortContainers [Box 10, EmptyBox, Box 7, EmptyBox]` → `[EmptyBox, EmptyBox, Box 7, Box 10]`.
- **Note** : La fonction est pure, car elle ne modifie pas d'état externe et repose uniquement sur la définition d'ordre de `Ord`.

#### 4. Fonction `main`
```haskell
main :: IO ()
main = do
  let boxes = [Box 5, EmptyBox, Box 2, Box 10, EmptyBox, Box 7] :: [Box Int]

  putStrLn "Liste avant tri :"
  print boxes

  putStrLn "Liste après tri :"
  print $ sortContainers boxes
```
- **Description** : Point d'entrée du programme, de type `IO ()`, exécutant des actions d'entrée/sortie.
- **Structure** :
  - Crée une liste `boxes` contenant des `Box Int` et `EmptyBox`.
  - Utilise `putStrLn` pour afficher des messages descriptifs.
  - Utilise `print` pour afficher la liste avant et après tri, convertissant automatiquement les valeurs en chaînes via `Show`.
- **Sortie attendue** :
  ```
  Liste avant tri :
  [Box 5,EmptyBox,Box 2,Box 10,EmptyBox,Box 7]
  Liste après tri :
  [EmptyBox,EmptyBox,Box 2,Box 5,Box 7,Box 10]
  ```
- **Lien avec le cours** : Utilise la notation `do` et les actions IO (`putStrLn`, `print`) comme vu dans la leçon sur les E/S.

### Résumé
- **Type `Box`** : `data Box a = Box a | EmptyBox`, conteneur générique avec instances `Show` et `Eq`.
- **Instance `Ord` pour `Box`** : Définit `compare` avec `EmptyBox` < `Box _`, comparant les valeurs pour `Box x` vs `Box y` (requiert `Ord a`).
- **Fonction `sortContainers`** : Trie une liste de `Box a` en utilisant `sort` de `Data.List`, avec l'import explicite pour garantir sa disponibilité.
- **Fonction `main`** : Teste `sortContainers` sur une liste de `Box Int`, affichant avant et après tri via IO.
- **Code** : Complet, testable, et respecte les concepts du cours (pureté, généricité, IO).
