HC10T5 : Classe de type ShowDetailed:
```haskell
-- Définition du type User
data User = User
  { userName :: String  -- Nom de l'utilisateur
  , userAge  :: Int     -- Âge de l'utilisateur
  } deriving (Show)

-- Définition de la classe de type ShowDetailed
class ShowDetailed a where
  showDetailed :: a -> String

-- Instance de ShowDetailed pour User
instance ShowDetailed User where
  showDetailed (User name age) = "User: " ++ name ++ ", Age: " ++ show age

-- Fonction main pour tester
main :: IO ()
main = do
  let user1 = User "Alice" 25    -- Utilisateur Alice, 25 ans
  let user2 = User "Bob" 30      -- Utilisateur Bob, 30 ans
  
  -- Test de showDetailed
  putStrLn $ showDetailed user1  -- Devrait afficher : User: Alice, Age: 25
  putStrLn $ showDetailed user2  -- Devrait afficher : User: Bob, Age: 30
```

### Explications :
1. **Type `User`** :
   - Défini comme un type de données avec deux champs : `userName :: String` (le nom de l'utilisateur) et `userAge :: Int` (l'âge de l'utilisateur).
   - Le `deriving (Show)` permet d'afficher les valeurs de `User` (par exemple, `User {userName = "Alice", userAge = 25}`) pour le débogage.

2. **Classe `ShowDetailed`** :
   - Déclare une méthode `showDetailed :: a -> String`, qui prend une valeur de type `a` et retourne une chaîne de caractères détaillant cette valeur.
   - Cette classe est conçue pour fournir une représentation plus personnalisée et détaillée qu'un simple affichage standard.

3. **Instance `ShowDetailed User`** :
   - Implémente `showDetailed` pour `User` en construisant une chaîne qui inclut le nom et l'âge de l'utilisateur.
   - Utilise `++` pour concaténer les chaînes, et `show age` pour convertir l'entier `age` en une chaîne.
   - Le format choisi est `"User: <nom>, Age: <âge>"`, mais tu peux le personnaliser selon tes besoins.

4. **Fonction `main`** :
   - Crée deux instances de `User` : `user1` (Alice, 25 ans) et `user2` (Bob, 30 ans).
   - Teste `showDetailed` sur ces deux utilisateurs et affiche les résultats avec `putStrLn`.
   - Les sorties incluront les détails formatés selon la logique définie dans `showDetailed`.
