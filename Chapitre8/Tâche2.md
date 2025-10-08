HC8T2 : Types personnalisés et constructeurs de données:
```haskell
-- Définir le type PaymentMethod
data PaymentMethod = Cash | Card | Cryptocurrency
  deriving (Show)

-- Définir le type Person
data Person = Person {
  name :: String,
  address :: (String, Int), -- Tuple pour l'adresse (ville, code postal)
  paymentMethod :: PaymentMethod
} deriving (Show)

-- Créer une instance de personne (Bob)
bob :: Person
bob = Person {
  name = "Bob",
  address = ("Paris", 75001),
  paymentMethod = Cash
}

-- Fonction main pour tester
main :: IO ()
main = do
  print bob
```

### Explications :
1. **Type `PaymentMethod`** :
   - `data PaymentMethod = Cash | Card | Cryptocurrency` définit un type algébrique avec trois constructeurs sans arguments (`Cash`, `Card`, `Cryptocurrency`).
   - `deriving (Show)` permet d'afficher les valeurs de ce type de manière lisible (par exemple, `Cash` sera affiché comme "Cash").

2. **Type `Person`** :
   - `data Person = Person { name :: String, address :: (String, Int), paymentMethod :: PaymentMethod }` définit un type avec des champs nommés (`name`, `address`, `paymentMethod`).
   - L'`address` est un tuple `(String, Int)` représentant une ville et un code postal.
   - `deriving (Show)` permet d'afficher les instances de `Person` de manière structurée.

3. **Instance `bob`** :
   - `bob` est une valeur de type `Person` avec `name = "Bob"`, `address = ("Paris", 75001)` (une adresse fictive), et `paymentMethod = Cash`.

4. **Fonction `main`** :
   - Utilise `print` pour afficher les détails de `bob`. Grâce à `deriving (Show)`, la sortie sera formatée automatiquement.
   - 
