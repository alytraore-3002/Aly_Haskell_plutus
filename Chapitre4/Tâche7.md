HC4T7 - Tâche 7 : Ignorer des éléments dans une liste

### Code Haskell

```haskell
-- Module principal
module Main where

-- Fonction firstAndThird qui retourne le premier et le troisième élément d'une liste
-- en utilisant le pattern matching
firstAndThird :: [a] -> [a]
firstAndThird (x:_:y:_) = [x, y]  -- Pattern matching pour extraire le 1er et 3e élément
firstAndThird _ = []              -- Cas par défaut pour les listes trop courtes

-- Fonction principale pour tester
main :: IO ()
main = do
  let test1 = firstAndThird [1, 2, 3, 4, 5]  -- Test avec une liste de 5 éléments
  let test2 = firstAndThird [1, 2]           -- Test avec une liste trop courte
  let test3 = firstAndThird ["a", "b", "c"]  -- Test avec une liste de chaînes
  putStrLn $ "Test 1 (Liste [1,2,3,4,5]): " ++ show test1
  putStrLn $ "Test 2 (Liste [1,2]): " ++ show test2
  putStrLn $ "Test 3 (Liste [\"a\",\"b\",\"c\"]): " ++ show test3
```

### Explications détaillées du code

#### 1. **Définition du module et imports implicites**
```haskell
-- Module principal
module Main where
```
- **Rôle** : Déclare le module comme `Main`, standard pour les programmes exécutables en Haskell. Le mot-clé `where` introduit les définitions suivantes.
- **Imports** : Aucun import explicite n’est requis, car le code utilise des fonctionnalités du *Prelude* (bibliothèque standard de Haskell), comme les listes `[a]`, les fonctions d’entrée/sortie (`putStrLn`, `show`), et les opérateurs de base (comme `++`), importés automatiquement.

#### 2. **Définition de la fonction `firstAndThird`**
```haskell
firstAndThird :: [a] -> [a]
```
- **Signature de type** : La fonction accepte une liste de type `[a]` (liste polymorphique où `a` peut être n’importe quel type, comme `Int`, `String`, etc.) et retourne une liste du même type `[a]`.
- **Objectif** : Extraire le premier et le troisième élément de la liste d’entrée, en ignorant les autres, et les retourner dans une nouvelle liste. Si la liste a moins de trois éléments, retourner une liste vide.

#### 3. **Corps de la fonction avec *pattern matching***
La fonction utilise le *pattern matching* (correspondance de motifs), une technique clé en Haskell pour décomposer les structures de données comme les listes de manière déclarative et concise. Cette approche est plus idiomatique que l’utilisation d’indices ou de vérifications de longueur.

```haskell
firstAndThird (x:_:y:_) = [x, y]  -- Pattern matching pour extraire le 1er et 3e élément
```
- **Premier motif** : `(x:_:y:_)`
  - **Structure** : Ce motif correspond à une liste ayant au moins trois éléments.
    - `x` : Lie le premier élément (tête de la liste) à la variable `x`, capturant sa valeur.
    - `_` : Un *wildcard* (joker) qui correspond au deuxième élément mais l’ignore, sans le lier à une variable, sautant ainsi explicitement cet élément.
    - `y` : Lie le troisième élément à la variable `y`.
    - `_` : Un autre *wildcard* qui correspond au reste de la liste (tous les éléments après le troisième), les ignorant.
  - **Résultat** : Si la liste correspond à ce motif (au moins trois éléments), l’expression `[x, y]` crée une nouvelle liste contenant uniquement le premier et le troisième élément.
  - **Avantages** :
    - **Concision** : Évite de calculer la longueur de la liste (opération O(n) avec `length`) ou d’accéder aux éléments par indice (comme `xs !! 0`), rendant le code plus clair.
    - **Sécurité** : Si la liste ne correspond pas à ce motif (trop courte), Haskell passe au motif suivant, évitant les erreurs d’exécution.
    - **Efficacité** : Le *pattern matching* ne vérifie que les trois premiers éléments (opération O(1)), contrairement à `length` qui parcourt toute la liste.

```haskell
firstAndThird _ = []              -- Cas par défaut pour les listes trop courtes
```
- **Deuxième motif** : `_`
  - **Structure** : Un *wildcard* qui correspond à toute liste non capturée par le premier motif, c’est-à-dire les listes avec moins de trois éléments (vide `[]`, un élément `[a]`, ou deux éléments `[a, b]`).
  - **Résultat** : Retourne une liste vide `[]`, indiquant qu’il n’y a pas assez d’éléments pour extraire le premier et le troisième.
- **Ordre des motifs** : Haskell évalue les motifs de haut en bas. Le motif spécifique `(x:_:y:_)` est testé en premier pour capturer les listes valides, et le *wildcard* `_` sert de cas par défaut. Inverser l’ordre rendrait le *wildcard* prioritaire, empêchant le motif spécifique d’être atteint.

#### 4. **Fonction `main` pour les tests**
```haskell
main :: IO ()
```
- **Signature de type** : `IO ()` indique une action d’entrée/sortie ne retournant aucune valeur utile (type unit `()`). C’est le point d’entrée du programme.

```haskell
main = do
  let test1 = firstAndThird [1, 2, 3, 4, 5]  -- Test avec une liste de 5 éléments
  let test2 = firstAndThird [1, 2]           -- Test avec une liste trop courte
  let test3 = firstAndThird ["a", "b", "c"]  -- Test avec une liste de chaînes
  putStrLn $ "Test 1 (Liste [1,2,3,4,5]): " ++ show test1
  putStrLn $ "Test 2 (Liste [1,2]): " ++ show test2
  putStrLn $ "Test 3 (Liste [\"a\",\"b\",\"c\"]): " ++ show test3
```
- **Bloc `do`** : Séquence des calculs purs (bindings `let`) et des actions d’entrée/sortie (`putStrLn`).
- **Bindings `let`** :
  - `test1` : Applique `firstAndThird` à `[1, 2, 3, 4, 5]`. Le premier motif correspond, liant `x = 1`, ignorant `2`, liant `y = 3`, et ignorant `[4, 5]`. Résultat : `[1, 3]`.
  - `test2` : Applique `firstAndThird` à `[1, 2]`. Le premier motif ne correspond pas (pas assez d’éléments), donc le *wildcard* retourne `[]`.
  - `test3` : Applique `firstAndThird` à `["a", "b", "c"]`. Le premier motif correspond, liant `x = "a"`, ignorant `"b"`, liant `y = "c"`. Résultat : `["a", "c"]`, démontrant le polymorphisme.
  - Ces bindings sont des calculs purs, évalués localement dans le bloc `do`.
- **Affichage avec `putStrLn`** :
  - `putStrLn $ "Test 1 ... " ++ show test1` : Concatène une chaîne descriptive avec la représentation textuelle de `test1` (via `show`, qui convertit `[1, 3]` en "[1,3]"). L’opérateur `$` évite les parenthèses en donnant une faible priorité à `putStrLn`.
  - Idem pour `test2` et `test3`, produisant une sortie formatée.
- **Exécution** : `main` évalue les tests et affiche les résultats, vérifiant le comportement de `firstAndThird` dans différents cas.

#### 5. **Caractéristiques générales du code**
- **Polymorphisme** : Le type `[a]` permet à la fonction de fonctionner avec tout type d’éléments (entiers, chaînes, etc.).
- **Pureté** : `firstAndThird` est une fonction pure (sans effets secondaires, résultat déterministe pour une même entrée).
- **Gestion des erreurs** : Le *pattern matching* gère les cas invalides (listes trop courtes) sans vérifications explicites ni exceptions.
- **Style idiomatique** : Le *pattern matching* est typique de Haskell, exploitant la nature récursive des listes et évitant les opérations coûteuses comme l’accès par indice.

### Résumé
Le code Haskell implémente la fonction `firstAndThird` pour extraire le premier et le troisième élément d’une liste, en ignorant les autres, à l’aide du *pattern matching* pour une solution concise et idiomatique. La fonction, de type `[a] -> [a]`, est polymorphique, fonctionnant avec tout type de liste, et retourne une liste vide pour les listes de moins de trois éléments. Le motif `(x:_:y:_)` capture le premier (`x`) et le troisième (`y`) élément, ignorant le deuxième et le reste, tandis que le *wildcard* `_` gère les listes trop courtes. La fonction `main` teste l’implémentation avec trois cas : une liste de cinq entiers (résultat `[1, 3]`), une liste courte (résultat `[]`), et une liste de chaînes (résultat `["a", "c"]`), démontrant la robustesse et le polymorphisme. Cette approche est efficace (accès en O(1)), sécurisée, et respecte les principes de programmation fonctionnelle de Haskell.
