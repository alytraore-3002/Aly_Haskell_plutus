HC8T4 : Syntaxe d'enregistrement pour Employé:
```haskell
-- Définir le type Employee en syntaxe d'enregistrement
data Employee = Employee {
  name :: String,
  experienceInYears :: Float
} deriving (Show)

-- Créer un employé Richard
richard :: Employee
richard = Employee {
  name = "Richard",
  experienceInYears = 7.5
}

-- Fonction main pour tester
main :: IO ()
main = do
  print richard
```

### Explications :
1. **Type `Employee`** :
   - `data Employee = Employee { name :: String, experienceInYears :: Float }` définit un type d'enregistrement avec deux champs :
     - `name :: String` pour le nom de l'employé.
     - `experienceInYears :: Float` pour le nombre d'années d'expérience (en utilisant `Float` pour permettre les décimales).
   - `deriving (Show)` permet d'afficher les instances de `Employee` de manière lisible.

2. **Instance `richard`** :
   - `richard` est une valeur de type `Employee` avec `name = "Richard"` et `experienceInYears = 7.5`.

3. **Fonction `main`** :
   - Utilise `print` pour afficher les détails de `richard`. Grâce à `deriving (Show)`, la sortie sera formatée automatiquement.
