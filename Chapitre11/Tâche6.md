HC11T6 : AdvancedEq pour Blockchain

```haskell
-- Définition du type Blockchain
data Blockchain = Blockchain String Int deriving (Show)

-- Définition de la typeclass AdvancedEq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> String

-- Instance de Eq pour Blockchain
instance Eq Blockchain where
  (Blockchain name1 size1) == (Blockchain name2 size2) = name1 == name2 && size1 == size2

-- Instance de AdvancedEq pour Blockchain
instance AdvancedEq Blockchain where
  compareEquality (Blockchain name1 size1) (Blockchain name2 size2)
    | name1 == name2 && size1 == size2 = "Les blockchains sont identiques."
    | name1 == name2 = "Les blockchains ont le même nom (" ++ name1 ++ "), mais des tailles différentes (" ++ show size1 ++ " vs " ++ show size2 ++ ")."
    | size1 == size2 = "Les blockchains ont la même taille (" ++ show size1 ++ "), mais des noms différents (" ++ name1 ++ " vs " ++ name2 ++ ")."
    | otherwise = "Les blockchains diffèrent par leur nom (" ++ name1 ++ " vs " ++ name2 ++ ") et leur taille (" ++ show size1 ++ " vs " ++ show size2 ++ ")."

-- Fonction main pour tester
main :: IO ()
main = do
  let bc1 = Blockchain "Bitcoin" 100
      bc2 = Blockchain "Bitcoin" 100
      bc3 = Blockchain "Bitcoin" 200
      bc4 = Blockchain "Ethereum" 100
      bc5 = Blockchain "Ethereum" 300

  -- Test de compareEquality
  putStrLn "Test de compareEquality :"
  putStrLn $ compareEquality bc1 bc2  -- Les blockchains sont identiques.
  putStrLn $ compareEquality bc1 bc3  -- Les blockchains ont le même nom (Bitcoin), mais des tailles différentes (100 vs 200).
  putStrLn $ compareEquality bc1 bc4  -- Les blockchains ont la même taille (100), mais des noms différents (Bitcoin vs Ethereum).
  putStrLn $ compareEquality bc1 bc5  -- Les blockchains diffèrent par leur nom (Bitcoin vs Ethereum) et leur taille (100 vs 300).
```

### Explications détaillées

#### 1. Définition du type `Blockchain`
```haskell
data Blockchain = Blockchain String Int deriving (Show)
```
- **Description** : `Blockchain` est un type algébrique avec un constructeur `Blockchain` prenant :
  - Un `String` : Le nom de la blockchain (e.g., "Bitcoin", "Ethereum").
  - Un `Int` : La taille (e.g., nombre de blocs, pour simplifier).
- **Dérivations** :
  - `Show` : Permet d'afficher les valeurs (e.g., `Blockchain "Bitcoin" 100`).
- **Rôle** : Représente une blockchain pour tester la typeclass `AdvancedEq`.

#### 2. Définition de la typeclass `AdvancedEq`
```haskell
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> String
```
- **Description** :
  - `AdvancedEq` étend la typeclass `Eq`, donc tout type implémentant `AdvancedEq` doit aussi implémenter `Eq`.
  - `compareEquality` : Prend deux valeurs de type `a` et retourne un `String` décrivant leur égalité ou leurs différences.
- **Rôle** : Permet une comparaison plus détaillée que `==` en fournissant un message explicatif.
- **Contrainte `Eq`** : Assure que `==` est disponible pour tester l’égalité dans l’implémentation.

#### 3. Instance de `Eq` pour `Blockchain`
```haskell
instance Eq Blockchain where
  (Blockchain name1 size1) == (Blockchain name2 size2) = name1 == name2 && size1 == size2
```
- **Description** : Définit l’égalité pour `Blockchain` :
  - Deux blockchains sont égales si elles ont le même nom (`name1 == name2`) et la même taille (`size1 == size2`).
- **Fonctionnement** :
  - Déconstruit les deux valeurs `Blockchain` pour comparer leurs champs.
  - Utilise `&&` pour combiner les comparaisons.
- **Exemple** : `Blockchain "Bitcoin" 100 == Blockchain "Bitcoin" 100` → `True`, sinon `False`.

#### 4. Instance de `AdvancedEq` pour `Blockchain`
```haskell
instance AdvancedEq Blockchain where
  compareEquality (Blockchain name1 size1) (Blockchain name2 size2)
    | name1 == name2 && size1 == size2 = "Les blockchains sont identiques."
    | name1 == name2 = "Les blockchains ont le même nom (" ++ name1 ++ "), mais des tailles différentes (" ++ show size1 ++ " vs " ++ show size2 ++ ")."
    | size1 == size2 = "Les blockchains ont la même taille (" ++ show size1 ++ "), mais des noms différents (" ++ name1 ++ " vs " ++ name2 ++ ")."
    | otherwise = "Les blockchains diffèrent par leur nom (" ++ name1 ++ " vs " ++ name2 ++ ") et leur taille (" ++ show size1 ++ " vs " ++ show size2 ++ ")."
```
- **Description** : Implémente `compareEquality` pour comparer deux `Blockchain` et retourner un message descriptif.
- **Fonctionnement** :
  - Utilise des guards pour quatre cas :
    1. **Égalité totale** : Si les noms et tailles sont identiques (`name1 == name2 && size1 == size2`).
    2. **Même nom, tailles différentes** : Si seuls les noms sont égaux.
    3. **Même taille, noms différents** : Si seules les tailles sont égales.
    4. **Différents partout** : Si ni les noms ni les tailles ne correspondent.
  - Les messages incluent les valeurs des champs (via `show` pour `Int`) pour plus de clarté.
- **Exemples** :
  - `compareEquality (Blockchain "Bitcoin" 100) (Blockchain "Bitcoin" 100)` → `"Les blockchains sont identiques."`
  - `compareEquality (Blockchain "Bitcoin" 100) (Blockchain "Bitcoin" 200)` → `"Les blockchains ont le même nom (Bitcoin), mais des tailles différentes (100 vs 200)."`
  - `compareEquality (Blockchain "Bitcoin" 100) (Blockchain "Ethereum" 100)` → `"Les blockchains ont la même taille (100), mais des noms différents (Bitcoin vs Ethereum)."`
  - `compareEquality (Blockchain "Bitcoin" 100) (Blockchain "Ethereum" 300)` → `"Les blockchains diffèrent par leur nom (Bitcoin vs Ethereum) et leur taille (100 vs 300)."`

### Résumé
- **Type `Blockchain`** : `data Blockchain = Blockchain String Int`, représente une blockchain avec un nom et une taille.
- **Typeclass `AdvancedEq`** : Étend `Eq` avec `compareEquality :: a -> a -> String` pour des comparaisons détaillées.
- **Instance `Eq` pour `Blockchain`** : Deux blockchains sont égales si leurs noms et tailles sont identiques.
- **Instance `AdvancedEq` pour `Blockchain`** : Fournit un message décrivant si les blockchains sont identiques, partiellement égales (nom ou taille), ou différentes.
- **Fonction `main`** : Teste `compareEquality` avec différentes paires de `Blockchain`, affichant les résultats via IO.
- **Code** : Complet, respecte les concepts du cours (pureté, typeclasses, IO).
