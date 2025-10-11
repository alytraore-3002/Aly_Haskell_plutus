HC11T1 : Instance WeAccept pour Box

```haskell
-- Définition du type Box
data Box = Box Int deriving (Show, Eq)

-- Définition de la typeclass WeAccept
class WeAccept a where
  accept :: a -> Bool

-- Instance de WeAccept pour Box
instance WeAccept Box where
  accept (Box n) = n > 0

-- Fonction pour filtrer les boîtes acceptées
acceptedBoxes :: [Box] -> [Box]
acceptedBoxes boxes = filter accept boxes

-- Fonction main pour tester
main :: IO ()
main = do
  let boxes = [Box 5, Box (-3), Box 0, Box 7, Box (-1)]
  putStrLn "Liste originale des boîtes :"
  print boxes
  putStrLn "Boîtes acceptées :"
  print (acceptedBoxes boxes)
```

### Explications détaillées

#### 1. Définition du type `Box`
```haskell
data Box = Box Int deriving (Show, Eq)
```
- **Description** : `Box` est un type de données algébrique simple avec un seul constructeur `Box` qui encapsule une valeur entière (`Int`). 
- **Dérivations** :
  - `Show` permet d'afficher les valeurs de `Box` (par exemple, `Box 5`).
  - `Eq` permet de comparer les boîtes (bien que non utilisé ici, c'est une bonne pratique).
- **Rôle** : Représente une "boîte" contenant une valeur, souvent utilisée dans les exercices Haskell pour illustrer l'encapsulation.

#### 2. Définition de la typeclass `WeAccept`
```haskell
class WeAccept a where
  accept :: a -> Bool
```
- **Description** : Une typeclass générique qui définit une méthode `accept`. Cette méthode prend une valeur de type `a` et retourne `True` si la valeur est acceptable, `False` sinon.
- **Hypothèse** : Comme la typeclass n'est pas définie dans la question, j'assume qu'elle sert à filtrer des valeurs selon un critère (similaire à un prédicat). Si `WeAccept` implique un pattern Visitor ou d'autres méthodes, il faudra ajuster.

#### 3. Instance de `WeAccept` pour `Box`
```haskell
instance WeAccept Box where
  accept (Box n) = n > 0
```
- **Description** : Cette instance définit comment `accept` fonctionne pour le type `Box`.
- **Fonctionnement** :
  - On déconstruit `Box n` pour extraire la valeur entière `n`.
  - On applique le critère `n > 0`, ce qui signifie que seules les boîtes contenant un entier strictement positif sont acceptées.
  - Exemple : `accept (Box 5)` retourne `True`, mais `accept (Box (-3))` retourne `False`.
- **Flexibilité** : Le critère `n > 0` est arbitraire pour l'exemple. On pourrait utiliser `n >= 0`, `n `mod` 2 == 0` (pour les nombres pairs), etc., selon les besoins du TP.

#### 4. Fonction `acceptedBoxes`
```haskell
acceptedBoxes :: [Box] -> [Box]
acceptedBoxes boxes = filter accept boxes
```
- **Signature** : Prend une liste de `Box` (`[Box]`) et retourne une liste des boîtes acceptées (`[Box]`).
- **Fonctionnement** :
  - Utilise `filter :: (a -> Bool) -> [a] -> [a]` pour ne garder que les éléments pour lesquels `accept` retourne `True`.
  - Comme `Box` est une instance de `WeAccept`, `accept` est automatiquement disponible et utilisé par `filter`.
- **Pureté** : Cette fonction est pure (pas d'effets secondaires), ce qui correspond au style fonctionnel de Haskell vu dans le cours.
- **Exemple** :
  ```haskell
  acceptedBoxes [Box 5, Box (-3), Box 0, Box 7, Box (-1)]  -- Retourne [Box 5, Box 7]
  ```

### Résumé
- **Type `Box`** : `data Box = Box Int`, une boîte contenant un entier.
- **Typeclass `WeAccept`** : Définie avec `accept :: a -> Bool` pour tester l'acceptabilité.
- **Instance** : `accept (Box n) = n > 0` accepte les boîtes avec des entiers positifs.
- **Fonction `acceptedBoxes`** : Filtre une liste de `Box` pour ne garder que celles où `accept` retourne `True`.
- **Fonction `main`** : Teste le filtrage en affichant une liste de boîtes et les boîtes acceptées via des actions IO.
- **Code** : Complet, testable, et respecte les concepts du cours (pureté, IO, notation `do`).
