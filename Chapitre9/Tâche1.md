HC9T1 : définir un synonyme de type paramétrique
```haskell
-- Définition du synonyme de type paramétrique Entity
type Entity a = (a, String)  -- Premier élément : type paramétrique, deuxième élément : adresse (String)

-- Exemple d'utilisation avec des entités
person :: Entity String
person = ("Alice", "123 Rue Principale")

company :: Entity Int
company = (101, "456 Avenue des Entreprises")

-- Fonction main pour afficher les entités
main :: IO ()
main = do
  putStrLn $ "Entité personne : " ++ show person
  putStrLn $ "Entité entreprise : " ++ show company
```

**Explications :**
- Le synonyme de type paramétrique `Entity a` est défini comme un tuple où le premier élément est de type `a` (paramétrique, peut être n'importe quel type) et le second élément est une `String` représentant une adresse.
- Deux instances sont créées :
  - `person` : une entité avec `a = String` pour le nom "Alice" et l'adresse "123 Rue Principale".
  - `company` : une entité avec `a = Int` pour l'identifiant 101 et l'adresse "456 Avenue des Entreprises".
- La fonction `main` affiche les deux entités en utilisant `show` pour convertir les tuples en chaînes de caractères.`
