HC11T3 : Classe de type Container pour Box

```haskell
-- Définition du type Box
data Box a = Box a | EmptyBox deriving (Show, Eq)

-- Définition de la typeclass Container
class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> a -> c a

-- Instance de Container pour Box
instance Container Box where
  isEmpty EmptyBox = True
  isEmpty (Box _) = False

  contains EmptyBox _ = False
  contains (Box x) y = x == y

  replace _ x = Box x

-- Fonction main pour tester
main :: IO ()
main = do
  let box1 = Box 5 :: Box Int
      box2 = EmptyBox :: Box Int
      box3 = Box "hello" :: Box String

  -- Test de isEmpty
  putStrLn "Test de isEmpty :"
  print $ isEmpty box1          -- False
  print $ isEmpty box2          -- True

  -- Test de contains
  putStrLn "\nTest de contains :"
  print $ contains box1 5       -- True
  print $ contains box1 3       -- False
  print $ contains box2 5       -- False
  print $ contains box3 "hello" -- True

  -- Test de replace
  putStrLn "\nTest de replace :"
  print $ replace box1 10       -- Box 10
  print $ replace box2 7        -- Box 7
  print $ replace box3 "world"  -- Box "world"
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
  - `Show` : Pour afficher les boîtes (e.g., `Box 5`, `EmptyBox`).
  - `Eq` : Nécessaire pour `contains`, qui compare des valeurs avec `==`.
- **Rôle** : Modélise un conteneur simple pouvant être vide ou contenir une valeur, adapté à la typeclass `Container`.

#### 2. Définition de la typeclass `Container`
```haskell
class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> a -> c a
```
- **Description** :
  - `isEmpty` : Retourne `True` si le conteneur est vide, `False` sinon.
  - `contains` : Vérifie si le conteneur contient une valeur donnée (contrainte `Eq a` pour permettre la comparaison).
  - `replace` : Remplace le contenu du conteneur par une nouvelle valeur, retournant un nouveau conteneur.
- **Généricité** : Le type `c` est un constructeur de type (e.g., `Box`), et `a` est le type des valeurs contenues. Cela permet à `Container` de s’appliquer à différents types de conteneurs.
- **Contrainte `Eq`** : Nécessaire pour `contains` afin de comparer les valeurs avec `==`.

#### 3. Instance de `Container` pour `Box`
```haskell
instance Container Box where
  isEmpty EmptyBox = True
  isEmpty (Box _) = False

  contains EmptyBox _ = False
  contains (Box x) y = x == y

  replace _ x = Box x
```
- **Méthode `isEmpty`** :
  - **Fonctionnement** : Retourne `True` pour `EmptyBox`, `False` pour `Box _` (peu importe la valeur contenue).
  - **Exemple** : `isEmpty EmptyBox` → `True`, `isEmpty (Box 5)` → `False`.
- **Méthode `contains`** :
  - **Fonctionnement** :
    - Pour `EmptyBox`, retourne toujours `False` (un conteneur vide ne contient rien).
    - Pour `Box x`, compare `x` avec la valeur donnée `y` en utilisant `==`.
  - **Exemple** : `contains (Box 5) 5` → `True`, `contains (Box 5) 3` → `False`.
  - **Contrainte `Eq`** : Nécessaire pour que `x == y` soit valide.
- **Méthode `replace`** :
  - **Fonctionnement** : Ignore le contenu actuel et retourne un nouveau `Box x` avec la valeur donnée `x`.
  - **Exemple** : `replace EmptyBox 10` → `Box 10`, `replace (Box 5) 7` → `Box 7`.
  - **Note** : Cette implémentation remplace toujours par un `Box`, même si le conteneur était `EmptyBox`, ce qui est cohérent avec l’idée d’un conteneur qui peut être rempli.

#### 4. Fonction `main`
- **Description** : Teste les trois méthodes sur des instances de `Box` avec différents types (`Int` et `String`) pour démontrer la généricité.
- **Structure** :
  - Crée des boîtes : `Box 5` (Int), `EmptyBox` (Int), `Box "hello"` (String).
  - Teste `isEmpty` sur une boîte pleine et une vide.
  - Teste `contains` avec des valeurs correspondantes et non correspondantes.
  - Teste `replace` pour remplacer les contenus ou remplir une boîte vide.
- **Lien avec le cours** : Utilise la notation `do` et les actions IO (`putStrLn`, `print`) comme vu dans la leçon sur les E/S.

### Résumé
- **Type `Box`** : `data Box a = Box a | EmptyBox`, un conteneur générique pouvant être vide ou contenir une valeur.
- **Typeclass `Container`** : Définie avec `isEmpty`, `contains` (avec contrainte `Eq`), et `replace` pour manipuler des conteneurs.
- **Instance pour `Box`** : 
  - `isEmpty` : `True` pour `EmptyBox`, `False` pour `Box _`.
  - `contains` : Vérifie l’égalité des valeurs pour `Box x`, `False` pour `EmptyBox`.
  - `replace` : Retourne un nouveau `Box` avec la valeur donnée.
- **Fonction `main`** : Teste les méthodes avec des boîtes de types `Int` et `String`, affichant les résultats via IO.
- **Code** : Complet, respecte les concepts du cours (pureté, généricité, IO).
