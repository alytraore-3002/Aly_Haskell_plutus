HC11T5 : Fonction devinerWhat'sInside pour Container

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

-- Fonction GuessWhat'sInside
guessWhatsInside :: (Container c, Eq a, Show a) => c a -> a -> String
guessWhatsInside container item
  | isEmpty container = "Le conteneur est vide, il ne contient rien !"
  | contains container item = "Bingo ! Le conteneur contient " ++ show item ++ "."
  | otherwise = "Désolé, le conteneur ne contient pas " ++ show item ++ "."

-- Fonction main pour tester
main :: IO ()
main = do
  let present1 = Present 42 :: Present Int
      present2 = EmptyPresent :: Present Int
      present3 = Present "surprise" :: Present String

  -- Test de guessWhatsInside
  putStrLn "Test de guessWhatsInside :"
  putStrLn $ guessWhatsInside present1 42      -- Bingo ! Le conteneur contient 42.
  putStrLn $ guessWhatsInside present1 10      -- Désolé, le conteneur ne contient pas 10.
  putStrLn $ guessWhatsInside present2 42      -- Le conteneur est vide, il ne contient rien !
  putStrLn $ guessWhatsInside present3 "surprise" -- Bingo ! Le conteneur contient "surprise".
  putStrLn $ guessWhatsInside present3 "gift"  -- Désolé, le conteneur ne contient pas "gift".
```

### Explications détaillées

#### 1. Contexte et hypothèses
- **Typeclass `Container`** : Définie précédemment avec :
  - `isEmpty :: c a -> Bool` : Vérifie si le conteneur est vide.
  - `contains :: (Eq a) => c a -> a -> Bool` : Vérifie si une valeur est dans le conteneur.
  - `replace :: c a -> a -> c a` : Remplace le contenu (non utilisé ici).
- **Type `Present`** : `data Present a = Present a | EmptyPresent`, un conteneur générique pouvant être vide ou contenir une valeur de type `a`.
- **Fonction `guessWhatsInside`** : Doit prendre un conteneur (de type `c a`, où `c` est une instance de `Container`) et un élément (de type `a`), et vérifier si cet élément est dans le conteneur, en renvoyant un message descriptif sous forme de `String`.
- **Contexte du cours** : Utilisation de fonctions pures, généricité, et actions IO pour les tests dans `main`.

#### 2. Définition du type `Present`
```haskell
data Present a = Present a | EmptyPresent deriving (Show, Eq)
```
- **Description** : Type algébrique générique avec :
  - `Present a` : Contient une valeur de type `a`.
  - `EmptyPresent` : Représente un conteneur vide.
- **Dérivations** :
  - `Show` : Pour afficher les valeurs (e.g., `Present 42`).
  - `Eq` : Nécessaire pour `contains` qui utilise `==`.
- **Rôle** : Sert de conteneur pour tester la fonction `guessWhatsInside`.

#### 3. Définition de la typeclass `Container`
```haskell
class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> a -> c a
```
- **Description** :
  - `isEmpty` : Vérifie si le conteneur est vide.
  - `contains` : Teste si une valeur donnée est dans le conteneur (requiert `Eq a` pour `==`).
  - `replace` : Non utilisé ici, mais inclus pour cohérence avec la définition précédente.
- **Généricité** : `c` est un constructeur de type (e.g., `Present`), `a` est le type des valeurs contenues.

#### 4. Instance de `Container` pour `Present`
```haskell
instance Container Present where
  isEmpty EmptyPresent = True
  isEmpty (Present _) = False

  contains EmptyPresent _ = False
  contains (Present x) y = x == y

  replace _ x = Present x
```
- **Méthode `isEmpty`** : Retourne `True` pour `EmptyPresent`, `False` pour `Present _`.
- **Méthode `contains`** : Retourne `False` pour `EmptyPresent`, sinon compare la valeur contenue dans `Present x` avec `y` via `==`.
- **Méthode `replace`** : Non utilisée ici, mais incluse pour compléter l’instance.
- **Exemples** :
  - `isEmpty (Present 42)` → `False`, `isEmpty EmptyPresent` → `True`.
  - `contains (Present 42) 42` → `True`, `contains (Present 42) 10` → `False`.

#### 5. Fonction `guessWhatsInside`
```haskell
guessWhatsInside :: (Container c, Eq a, Show a) => c a -> a -> String
guessWhatsInside container item
  | isEmpty container = "Le conteneur est vide, il ne contient rien !"
  | contains container item = "Bingo ! Le conteneur contient " ++ show item ++ "."
  | otherwise = "Désolé, le conteneur ne contient pas " ++ show item ++ "."
```
- **Signature** : Prend un conteneur `c a` (instance de `Container`) et un élément `a`, retourne un `String`. Contraintes :
  - `Container c` : Pour utiliser `isEmpty` et `contains`.
  - `Eq a` : Pour `contains` (comparaison avec `==`).
  - `Show a` : Pour convertir `item` en `String` avec `show`.
- **Fonctionnement** :
  - Utilise des guards pour trois cas :
    1. Si `isEmpty container` est `True`, retourne un message indiquant que le conteneur est vide.
    2. Si `contains container item` est `True`, retourne un message de succès avec l’élément affiché.
    3. Sinon, retourne un message indiquant que l’élément n’est pas dans le conteneur.
  - La fonction est pure, renvoyant un `String` formaté pour l’affichage.
- **Exemples** :
  - `guessWhatsInside (Present 42) 42` → `"Bingo ! Le conteneur contient 42."`
  - `guessWhatsInside (Present 42) 10` → `"Désolé, le conteneur ne contient pas 10."`
  - `guessWhatsInside EmptyPresent 42` → `"Le conteneur est vide, il ne contient rien !"`

#### 6. Fonction `main`
- **Description** : Teste `guessWhatsInside` avec des instances de `Present` contenant des types `Int` et `String`.
- **Structure** :
  - Crée trois conteneurs : `Present 42` (Int), `EmptyPresent` (Int), `Present "surprise"` (String).
  - Teste `guessWhatsInside` avec des éléments correspondants et non correspondants.
  - Utilise `putStrLn` pour afficher les messages résultats.
- **Lien avec le cours** : Exploite la notation `do` et les actions IO (`putStrLn`, `print`) pour afficher les résultats, conformément à la leçon sur les E/S.

### Résumé
- **Type `Present`** : `data Present a = Present a | EmptyPresent`, un conteneur générique.
- **Typeclass `Container`** : Définie avec `isEmpty`, `contains` (requiert `Eq`), et `replace`.
- **Instance pour `Present`** : Implémente `isEmpty` (`True` pour `EmptyPresent`), `contains` (égalité pour `Present x`), et `replace` (nouveau `Present`).
- **Fonction `guessWhatsInside`** : Vérifie si un élément est dans un conteneur, retourne un message descriptif (`String`) selon trois cas (vide, contient, ne contient pas).
- **Fonction `main`** : Teste la fonction avec des `Present` de types `Int` et `String` via IO.
- **Code** : Complet, respecte les concepts du cours (pureté, généricité, IO).
