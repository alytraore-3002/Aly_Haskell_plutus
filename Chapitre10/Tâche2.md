HC10T2 : Classe de type Sommable
```haskell
-- Définition de la classe de type Summable
class Summable a where
  sumUp :: [a] -> Maybe a

-- Instance de Summable pour Int
instance Summable Int where
  sumUp [] = Nothing          -- Liste vide : retourne Nothing
  sumUp xs = Just (sum xs)    -- Liste non vide : retourne la somme dans Just

-- Fonction main pour tester
main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5] :: [Int]  -- Type explicite ajouté
  let emptyList = [] :: [Int]              -- Type explicite
  print $ sumUp numbers                    -- Devrait afficher : Just 15
  print $ sumUp emptyList                  -- Devrait afficher : Nothing
```

### Explication détaillée :
1. **Classe `Summable`** :
   - La classe `Summable` définit une méthode `sumUp :: [a] -> Maybe a`, qui prend une liste de type `[a]` et retourne une valeur de type `Maybe a`. L'utilisation de `Maybe` permet de gérer proprement les cas où la liste est vide (`Nothing`) ou contient des éléments à sommer (`Just` avec le résultat).

2. **Instance `Summable Int`** :
   - Pour le type `Int`, l'instance spécifie :
     - `sumUp [] = Nothing` : Si la liste est vide, retourne `Nothing`, car il n'y a rien à sommer.
     - `sumUp xs = Just (sum xs)` : Si la liste contient des éléments, utilise la fonction `sum` (définie dans la bibliothèque standard pour les types `Num`) pour calculer la somme et l'enveloppe dans `Just`.
   - Cela nécessite que `Int` soit une instance de la classe `Num`, ce qui est vrai par défaut en Haskell.

3. **Problème de l'erreur "Ambiguous type variable"** :
   - L'erreur précédente (`Ambiguous type variable`) survenait car, dans le code initial, les listes `[1, 2, 3, 4, 5]` et `[]` n'avaient pas de type explicite. Haskell infère qu'elles sont des listes de type `Num a => [a]`, mais il ne peut pas décider si `a` est `Int`, `Double`, ou un autre type `Num`, car plusieurs instances sont possibles.
   - Le compilateur a suggéré d'utiliser une instance spécifique comme `Num Int` ou `Num Double`, ce qui indique qu'il faut préciser le type.

4. **Correction avec des types explicites** :
   - J'ai ajouté `:: [Int]` à `numbers = [1, 2, 3, 4, 5] :: [Int]` pour indiquer que c'est une liste d'entiers. Cela force le compilateur à utiliser l'instance `Summable Int`.
   - De même, `emptyList = [] :: [Int]` spécifie que c'est une liste vide d'entiers, évitant toute ambiguïté.
   - Avec ces annotations, Haskell sait que `sumUp` doit utiliser l'instance `Summable Int`, et l'erreur disparaît.

5. **Fonction `main`** :
   - Le `do`-block dans `main` définit les variables `numbers` et `emptyList`, puis utilise `print` pour afficher les résultats de `sumUp`.
   - `print $ sumUp numbers` affiche `Just 15`, car 1 + 2 + 3 + 4 + 5 = 15.
   - `print $ sumUp emptyList` affiche `Nothing`, car la liste est vide.
   
