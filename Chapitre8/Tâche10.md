HC8T10 : Spectacle dérivé pour livre
```haskell
-- Définition du type Book avec la syntaxe d'enregistrement
data Book = Book { title :: String, author :: String, year :: Int }
  deriving (Show)

-- Création d'une instance de Book
book :: Book
book = Book { title = "1984", author = "George Orwell", year = 1949 }

-- Fonction main pour afficher l'instance de Book
main :: IO ()
main = print book
```

**Explications :**
- Le type `Book` est défini avec la syntaxe d'enregistrement, contenant les champs `title` (String), `author` (String) et `year` (Int).
- `deriving (Show)` est utilisé pour permettre l'affichage automatique de l'instance de `Book` via `print`.
- Une instance de `Book` est créée pour le livre "1984" de George Orwell, publié en 1949.
- La fonction `main` utilise `print` pour afficher l'instance de `Book`, en exploitant l'instance dérivée de `Show`.
- 
