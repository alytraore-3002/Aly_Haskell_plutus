HC12T6 : Trier une liste d'entiers
```haskell
import Data.List (sort)

-- Fonction pour trier une liste d'entiers
sortList :: [Int] -> [Int]
sortList = sort

-- Fonction principale avec des tests statiques
main :: IO ()
main = do
  let testList1 = [5, 2, 9, 1]
  let testList2 = [10, -3, 7, 0]
  let sortedList1 = sortList testList1
  let sortedList2 = sortList testList2
  putStrLn "Liste triée 1 :"
  print sortedList1
  putStrLn "Liste triée 2 :"
  print sortedList2
```

### Explication détaillée
1. **Importation (`import Data.List (sort)`)**
   - Importe explicitement la fonction `sort` du module `Data.List` pour s'assurer qu'elle est disponible.

2. **Fonction `sortList`**
   - Prend une liste d'entiers `[Int]` et utilise `sort` pour la trier dans l'ordre croissant.
   - C'est une fonction simple qui repose sur la fonction standard `sort`.

3. **Fonction `main`**
   - **Listes statiques** : `testList1 = [5, 2, 9, 1]` et `testList2 = [10, -3, 7, 0]` sont des exemples de listes prédéfinies.
   - **Tri** : `sortList testList1` et `sortList testList2` trient ces listes.
   
