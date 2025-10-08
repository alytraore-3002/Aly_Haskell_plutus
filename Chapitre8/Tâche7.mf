HC8T7 : Types de données et description d'animaux
```haskell
-- Définition du type Animal avec deux constructeurs
data Animal = Dog String | Cat String
  deriving (Show)

-- Fonction pour décrire un animal
describeAnimal :: Animal -> String
describeAnimal (Dog name) = "C'est un chien nommé " ++ name ++ "."
describeAnimal (Cat name) = "C'est un chat nommé " ++ name ++ "."

-- Création d'une instance pour un chien
dog :: Animal
dog = Dog "Rex"

-- Création d'une instance pour un chat
cat :: Animal
cat = Cat "Mimi"

-- Fonction main pour afficher les descriptions
main :: IO ()
main = do
  putStrLn $ describeAnimal dog
  putStrLn $ describeAnimal cat
```

**Explications :**
- Le type `Animal` est défini avec deux constructeurs : `Dog` et `Cat`, chacun prenant un `String` pour le nom de l'animal.
- `deriving (Show)` est ajouté pour permettre l'affichage des instances de `Animal` si nécessaire (bien que non utilisé directement ici).
- La fonction `describeAnimal` prend un `Animal` et retourne une description sous forme de `String` en utilisant le pattern matching pour différencier `Dog` et `Cat`.
- Une instance de `Dog` est créée avec le nom "Rex".
- Une instance de `Cat` est créée avec le nom "Mimi".
- La fonction `main` utilise `putStrLn` pour afficher les descriptions des deux animaux.
