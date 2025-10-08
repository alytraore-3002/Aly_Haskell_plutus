HC9T9 : Vérifier la présence d'un élément dans une séquence
```haskell
-- Définition du type de données récursif Sequence
data Sequence a = Empty | Node a (Sequence a) deriving (Show)

-- Fonction elemSeq qui vérifie si un élément est présent dans la séquence
elemSeq :: (Eq a) => a -> Sequence a -> Bool
elemSeq _ Empty = False                        -- Si la séquence est vide, l'élément n'est pas présent
elemSeq x (Node value next) = x == value || elemSeq x next  -- Vérifie l'élément courant ou passe au suivant

-- Fonction main pour tester
main :: IO ()
main = do
  -- Création d'une séquence : 1 -> 2 -> 3 -> Empty
  let seq = Node 1 (Node 2 (Node 3 Empty))
  
  -- Test de la fonction elemSeq
  print $ elemSeq 2 seq  -- Devrait afficher : True
  print $ elemSeq 4 seq  -- Devrait afficher : False
```

### Explications :
1. **Type `Sequence a`** :
   - Comme précédemment, `Sequence a` est un type récursif avec deux constructeurs :
     - `Empty` : fin de la séquence.
     - `Node a (Sequence a)` : un nœud avec une valeur de type `a` et un lien vers la séquence suivante.
   - Le `deriving (Show)` permet d'afficher la séquence pour le débogage.
2. **Fonction `elemSeq`** :
   - Prend un élément `x` de type `a` et une `Sequence a`, avec la contrainte `(Eq a)` pour permettre la comparaison d'égalité.
   - Si la séquence est `Empty`, retourne `False` (l'élément n'est pas présent).
   - Si la séquence est un `Node value next`, vérifie si `x` est égal à `value` (l'élément courant) ou si `x` est présent dans la sous-séquence `next` (appel récursif).
   - Utilise l'opérateur `||` pour combiner les deux cas : l'élément est présent si soit il est dans le nœud courant, soit il est dans la suite.
3. **Fonction `main`** :
   - Crée une séquence contenant les valeurs 1, 2, 3 (terminée par `Empty`).
   - Teste `elemSeq` pour vérifier si 2 (présent) et 4 (absent) sont dans la séquence.
   - Affiche les résultats avec `print`.
   - 
