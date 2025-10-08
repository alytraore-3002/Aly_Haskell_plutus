HC8T5 : Syntaxe d'enregistrement pour Personne

```haskell
-- Définition du type Person avec la syntaxe d'enregistrement
data Person = Person { name :: String, age :: Int, isEmployed :: Bool }
  deriving (Show)

-- Création des deux personnes
person1 :: Person
person1 = Person { name = "Alice", age = 30, isEmployed = True }

person2 :: Person
person2 = Person { name = "Bob", age = 25, isEmployed = False }

-- Fonction main pour afficher les personnes
main :: IO ()
main = do
  print person1
  print person2
```

**Explications :**
- Le type `Person` est défini avec la syntaxe d'enregistrement, contenant les champs `name` (String), `age` (Int) et `isEmployed` (Bool).
- `deriving (Show)` permet d'afficher les instances de `Person` directement avec `print`.
- `person1` est définie comme une personne employée (Alice, 30 ans, isEmployed = True).
- `person2` est définie comme une personne sans emploi (Bob, 25 ans, isEmployed = False).
- La fonction `main` affiche les deux personnes dans la console.
