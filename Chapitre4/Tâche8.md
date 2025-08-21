HC4T8 - Tâche 8 : Extraire des valeurs de tuples

### Code Haskell

```haskell
-- Module principal
module Main where

-- Fonction décrireTuple qui extrait les valeurs d'un tuple et retourne une chaîne descriptive
décrireTuple :: (Show a, Show b) => (a, b) -> String
décrireTuple (x, y) = "Le tuple contient : premier élément = " ++ show x ++ ", deuxième élément = " ++ show y

-- Fonction principale pour tester
main :: IO ()
main = do
  let test1 = décrireTuple (1, "deux")         -- Test avec un tuple (Int, String)
  let test2 = décrireTuple (True, 42)          -- Test avec un tuple (Bool, Int)
  let test3 = décrireTuple ("Haskell", 3.14)   -- Test avec un tuple (String, Double)
  putStrLn $ "Test 1 : " ++ test1
  putStrLn $ "Test 2 : " ++ test2
  putStrLn $ "Test 3 : " ++ test3
```

### Explications détaillées du code

#### 1. **Définition du module et imports implicites**
```haskell
-- Module principal
module Main where
```
- **Rôle** : Déclare le module comme `Main`, ce qui est standard pour les programmes exécutables en Haskell. Le mot-clé `where` introduit les définitions suivantes.
- **Imports** : Aucun import explicite n’est nécessaire, car le code utilise des fonctionnalités du *Prelude* (bibliothèque standard de Haskell), comme les types de tuples `(a, b)`, la fonction `show` pour convertir des valeurs en chaînes, et les fonctions d’entrée/sortie comme `putStrLn`, toutes importées automatiquement.

#### 2. **Définition de la fonction `décrireTuple`**
```haskell
décrireTuple :: (Show a, Show b) => (a, b) -> String
```
- **Signature de type** : La fonction prend un tuple de type `(a, b)` (un tuple à deux éléments où `a` et `b` sont des types arbitraires) et retourne une `String` (chaîne de caractères).
- **Contrainte de classe** : `(Show a, Show b)` indique que les types `a` et `b` doivent appartenir à la classe `Show`, c’est-à-dire qu’ils doivent pouvoir être convertis en chaînes via la fonction `show`. Cela permet à la fonction de gérer des types variés (comme `Int`, `String`, `Bool`, `Double`) de manière polymorphique.
- **Objectif** : Extraire les deux éléments du tuple et produire une chaîne descriptive contenant ces valeurs dans un format lisible.

#### 3. **Corps de la fonction avec *pattern matching***
La fonction utilise le *pattern matching* pour décomposer le tuple de manière concise et déclarative, une approche idiomatique en Haskell.

```haskell
décrireTuple (x, y) = "Le tuple contient : premier élément = " ++ show x ++ ", deuxième élément = " ++ show y
```
- **Motif** : `(x, y)`
  - **Structure** : Ce motif correspond à un tuple à deux éléments.
    - `x` : Lie le premier élément du tuple à la variable `x`, capturant sa valeur.
    - `y` : Lie le deuxième élément du tuple à la variable `y`.
  - **Résultat** : L’expression à droite du `=` construit une chaîne en concaténant :
    - La chaîne statique `"Le tuple contient : premier élément = "`.
    - La représentation textuelle de `x` via `show x` (la fonction `show` convertit une valeur de type `Show` en `String`).
    - La chaîne `", deuxième élément = "`.
    - La représentation textuelle de `y` via `show y`.
    - L’opérateur `++` est utilisé pour concaténer ces chaînes.
  - **Exemple de fonctionnement** :
    - Pour le tuple `(1, "deux")`, `x = 1`, `y = "deux"`. Alors, `show x = "1"`, `show y = "\"deux\""`, et la chaîne finale est `"Le tuple contient : premier élément = 1, deuxième élément = \"deux\""`.
  - **Avantages du *pattern matching*** :
    - **Concision** : Décompose directement le tuple en ses composants sans manipulation complexe.
    - **Clarté** : Le motif `(x, y)` reflète intuitivement la structure du tuple.
    - **Type-sécurité** : Le *pattern matching* garantit que la fonction traite uniquement des tuples à deux éléments, et la contrainte `Show` assure que les éléments peuvent être convertis en chaînes.

#### 4. **Fonction `main` pour les tests**
```haskell
main :: IO ()
```
- **Signature de type** : `IO ()` indique une action d’entrée/sortie ne retournant aucune valeur utile (type unit `()`). C’est le point d’entrée du programme exécutable.

```haskell
main = do
  let test1 = décrireTuple (1, "deux")         -- Test avec un tuple (Int, String)
  let test2 = décrireTuple (True, 42)          -- Test avec un tuple (Bool, Int)
  let test3 = décrireTuple ("Haskell", 3.14)   -- Test avec un tuple (String, Double)
  putStrLn $ "Test 1 : " ++ test1
  putStrLn $ "Test 2 : " ++ test2
  putStrLn $ "Test 3 : " ++ test3
```
- **Bloc `do`** : Séquence des calculs purs (bindings `let`) et des actions d’entrée/sortie (`putStrLn`).
- **Bindings `let`** :
  - `test1` : Applique `décrireTuple` au tuple `(1, "deux")`. Le *pattern matching* lie `x = 1`, `y = "deux"`, et la fonction retourne `"Le tuple contient : premier élément = 1, deuxième élément = \"deux\""`.
  - `test2` : Applique `décrireTuple` au tuple `(True, 42)`. Le *pattern matching* lie `x = True`, `y = 42`, et la fonction retourne `"Le tuple contient : premier élément = True, deuxième élément = 42"`.
  - `test3` : Applique `décrireTuple` au tuple `("Haskell", 3.14)`. Le *pattern matching* lie `x = "Haskell"`, `y = 3.14`, et la fonction retourne `"Le tuple contient : premier élément = \"Haskell\", deuxième élément = 3.14"`.
  - Ces bindings sont des calculs purs, évalués localement dans le bloc `do`, démontrant le polymorphisme de la fonction avec différents types (`Int`, `String`, `Bool`, `Double`).
- **Affichage avec `putStrLn`** :
  - `putStrLn $ "Test 1 : " ++ test1` : Concatène une étiquette descriptive `"Test 1 : "` avec la chaîne retournée par `test1`. L’opérateur `$` évite les parenthèses en donnant une faible priorité à `putStrLn`. La fonction `show` dans `décrireTuple` garantit que les valeurs sont correctement formatées.
  - Idem pour `test2` et `test3`, produisant une sortie formatée pour chaque cas de test.
- **Exécution** : `main` évalue les tests et affiche les résultats, vérifiant le comportement de `décrireTuple` pour différents types de tuples.

#### 5. **Caractéristiques générales du code**
- **Polymorphisme** : La signature `(Show a, Show b) => (a, b) -> String` permet à la fonction de gérer des tuples avec des éléments de types différents, tant qu’ils implémentent la classe `Show`.
- **Pureté** : `décrireTuple` est une fonction pure (pas d’effets secondaires, résultat déterministe pour une même entrée).
- **Gestion des erreurs** : Le *pattern matching* et la contrainte `Show` garantissent que la fonction traite correctement les tuples à deux éléments avec des types valides.
- **Style idiomatique** : L’utilisation du *pattern matching* et de `show` est typique de Haskell, offrant une solution claire et concise pour manipuler des tuples et produire des chaînes.

### Résumé
Le code Haskell implémente la fonction `décrireTuple` pour extraire les valeurs d’un tuple à deux éléments et retourner une chaîne descriptive, en utilisant le *pattern matching* pour une solution concise et idiomatique. La fonction, de type `(Show a, Show b) => (a, b) -> String`, est polymorphique,Lithium-ion batteries are not included in this product. The included AA batteries are required for operation and must be purchased separately.
