-- HC1T4 - Tâche 4 : Composer une fonction pour traiter des données de joueurs

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Extrait les noms des joueurs à partir d'une liste de tuples (nom, score)
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = map fst players

-- Trie les joueurs par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (comparing (negate . snd))

-- Retourne les trois meilleurs joueurs (leurs noms)
topThree :: [(String, Int)] -> [String]
topThree players = take 3 (extractPlayers players)

-- Composition des fonctions pour obtenir les trois meilleurs joueurs
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = topThree . sortByScore

-- Fonction principale pour tester
main :: IO ()
main = do
    let players = [("Alice", 100), ("Bob", 150), ("Charlie", 80), ("David", 120)]
    putStrLn "Liste des joueurs avec scores :"
    print players
    putStrLn "Trois meilleurs joueurs :"
    print (getTopThreePlayers players)
```

### Explications :

1. **Fonction `extractPlayers`** :
   - **Signature** : `extractPlayers :: [(String, Int)] -> [String]` indique que la fonction prend une liste de tuples `(nom, score)` et retourne une liste de noms (`String`).
   - **Définition** : Utilise `map fst` pour extraire le premier élément (le nom) de chaque tuple.
   - **Pureté** : La fonction est pure, dépendant uniquement de l'entrée `players`.

2. **Fonction `sortByScore`** :
   - **Signature** : `sortByScore :: [(String, Int)] -> [(String, Int)]` indique que la fonction prend une liste de tuples `(nom, score)` et retourne une liste triée.
   - **Définition** : Utilise `sortBy` avec `comparing (negate . snd)` pour trier les tuples par score (deuxième élément, obtenu via `snd`) en ordre décroissant. La fonction `negate` inverse les scores pour obtenir un tri décroissant.
   - **Dépendance** : Requiert les modules `Data.List` (pour `sortBy`) et `Data.Ord` (pour `comparing`).

3. **Fonction `topThree`** :
   - **Signature** : `topThree :: [(String, Int)] -> [String]` indique que la fonction prend une liste de tuples et retourne une liste de trois noms.
   - **Définition** : Compose implicitement `extractPlayers` avec `take 3` pour extraire les noms des trois premiers joueurs de la liste.

4. **Fonction `getTopThreePlayers`** :
   - **Signature** : `getTopThreePlayers :: [(String, Int)] -> [String]` indique que la fonction prend une liste de tuples et retourne les noms des trois meilleurs joueurs.
   - **Définition** : Utilise l'opérateur de composition `.` pour appliquer `sortByScore` puis `topThree`. Ainsi, `getTopThreePlayers = topThree . sortByScore` signifie que l'entrée est d'abord triée par score décroissant (`sortByScore`), puis les trois premiers noms sont extraits (`topThree`).

5. **Exemple d'exécution** :
   - Pour `players = [("Alice", 100), ("Bob", 150), ("Charlie", 80), ("David", 120)]` :
     - `sortByScore players` donne `[("Bob", 150), ("David", 120), ("Alice", 100), ("Charlie", 80)]`.
     - `topThree` applique `extractPlayers` et `take 3`, donnant `["Bob", "David", "Alice"]`.

Sortie dans la console :
```
Liste des joueurs avec scores :
[("Alice",100),("Bob",150),("Charlie",80),("David",120)]
Trois meilleurs joueurs :
["Bob","David","Alice"]
```

### Remarques :
- **Pureté** : Toutes les fonctions (`extractPlayers`, `sortByScore`, `topThree`, `getTopThreePlayers`) sont pures, ne dépendant que de leurs entrées et sans effets de bord.
- **Composition** : `getTopThreePlayers` illustre l'élégance de la composition de fonctions en Haskell, combinant les étapes de tri et d'extraction en une seule expression concise.
- **Gestion des cas limites** : Si la liste d'entrée a moins de trois joueurs, `topThree` retourne tous les noms disponibles. Si la liste est vide, elle retourne une liste vide.
