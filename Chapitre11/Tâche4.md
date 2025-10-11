HC11T4 : Conteneur d'instance pour présent

```haskell
-- Définition du type Present
data Present a = Present a | EmptyPresent deriving (Show, Eq)

-- Définition de la typeclass Container
class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> a -> c a

-- Instance de Container pour Present
instance Container Present where
  isEmpty EmptyPresent = True
  isEmpty (Present _) = False

  contains EmptyPresent _ = False
  contains (Present x) y = x == y

  replace _ x = Present x

-- Fonction main pour tester
main :: IO ()
main = do
  let present1 = Present 10 :: Present Int
      present2 = EmptyPresent :: Present Int
      present3 = Present "gift" :: Present String

  -- Test de isEmpty
  putStrLn "Test de isEmpty :"
  print $ isEmpty present1      -- False
  print $ isEmpty present2      -- True

  -- Test de contains
  putStrLn "\nTest de contains :"
  print $ contains present1 10  -- True
  print $ contains present1 5   -- False
  print $ contains present2 10  -- False
  print $ contains present3 "gift" -- True

  -- Test de replace
  putStrLn "\nTest de replace :"
  print $ replace present1 20      -- Present 20
  print $ replace present2 15      -- Present 15
  print $ replace present3 "surprise" -- Present "surprise"
```

### Explications détaillées

#### 1. Définition du type `Present`
```haskell
data Present a = Present a | EmptyPresent deriving (Show, Eq)
```
- **Description** : `Present` est un type algébrique générique avec deux constructeurs :
  - `Present a` : Contient une valeur de type `a` (e.g., un cadeau).
  - `EmptyPresent` : Représente un conteneur vide (e.g., une boîte à cadeau vide).
- **Dérivations** :
  - `Show` : Permet d'afficher les valeurs (e.g., `Present 10`, `EmptyPresent`).
  - `Eq` : Nécessaire pour `contains`, qui utilise `==` pour comparer les valeurs.
- **Rôle** : Modélise un conteneur simple, adapté à la typeclass `Container`.

#### 2. Définition de la typeclass `Container`
```haskell
class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> a -> c a
```
- **Description** :
  - `isEmpty` : Retourne `True` si le conteneur est vide, `False` sinon.
  - `contains` : Vérifie si le conteneur contient une valeur donnée, avec une contrainte `Eq a` pour la comparaison.
  - `replace` : Remplace le contenu du conteneur par une nouvelle valeur, retournant un nouveau conteneur.
- **Généricité** : `c` est un constructeur de type (e.g., `Present`), et `a` est le type des valeurs contenues.
- **Contrainte `Eq`** : Requise pour `contains` afin de permettre l’utilisation de `==`.

#### 3. Instance de `Container` pour `Present`
```haskell
instance Container Present where
  isEmpty EmptyPresent = True
  isEmpty (Present _) = False

  contains EmptyPresent _ = False
  contains (Present x) y = x == y

  replace _ x = Present x
```
- **Méthode `isEmpty`** :
  - **Fonctionnement** : Retourne `True` pour `EmptyPresent`, `False` pour `Present _` (quelle que soit la valeur).
  - **Exemple** : `isEmpty EmptyPresent` → `True`, `isEmpty (Present 10)` → `False`.
- **Méthode `contains`** :
  - **Fonctionnement** :
    - Pour `EmptyPresent`, retourne `False` (un conteneur vide ne contient rien).
    - Pour `Present x`, compare `x` avec la valeur donnée `y` en utilisant `==`.
  - **Exemple** : `contains (Present 10) 10` → `True`, `contains (Present 10) 5` → `False`.
  - **Contrainte `Eq`** : Assure que `x == y` est valide pour le type `a`.
- **Méthode `replace`** :
  - **Fonctionnement** : Ignore le contenu actuel et retourne un nouveau `Present x` avec la valeur donnée `x`.
  - **Exemple** : `replace EmptyPresent 15` → `Present 15`, `replace (Present 10) 20` → `Present 20`.
  - **Note** : Remplace toujours par un `Present`, même si le conteneur était vide, ce qui est cohérent pour un conteneur.

### Résumé
- **Type `Present`** : `data Present a = Present a | EmptyPresent`, un conteneur générique pouvant être vide ou contenir une valeur.
- **Typeclass `Container`** : Définie avec `isEmpty`, `contains` (avec contrainte `Eq`), et `replace` pour manipuler des conteneurs.
- **Instance pour `Present`** :
  - `isEmpty` : `True` pour `EmptyPresent`, `False` pour `Present _`.
  - `contains` : Vérifie l’égalité des valeurs pour `Present x`, `False` pour `EmptyPresent`.
  - `replace` : Retourne un nouveau `Present` avec la valeur donnée.
- **Fonction `main`** : Teste les méthodes avec des `Present` de types `Int` et `String`, affichant les résultats via IO.
- **Code** : Complet, respecte les concepts du cours (pureté, généricité, IO).
