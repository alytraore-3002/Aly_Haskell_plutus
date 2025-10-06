HC6T8 : Filtrer les nombres paires

### Code Haskell

```haskell
-- Définition de la fonction filtrePaires récursive
filtrePaires :: [Int] -> [Int]
filtrePaires [] = []  -- Cas de base : liste vide retourne liste vide
filtrePaires (x:xs) 
    | even x    = x : filtrePaires xs  -- Si x pair, l'ajoute à la tête du résultat filtré
    | otherwise = filtrePaires xs      -- Sinon, ignore x et filtre le reste

-- Fonction main pour tester la fonction filtrePaires
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5, 6, 7, 8]  -- Liste à tester (paires : [2,4,6,8])
    putStrLn $ "La liste originale : " ++ show liste
    putStrLn $ "Les nombres pairs : " ++ show (filtrePaires liste)
```

### Explications détaillées

#### 1. **Signature de la fonction `filtrePaires`**
```haskell
filtrePaires :: [Int] -> [Int]
```
- **Type** : La fonction prend une liste d'entiers `[Int]` et retourne une liste d'entiers `[Int]` contenant uniquement les nombres pairs. Elle n'est pas polymorphique ici (limitée à `Int`), car le test de parité (`even`) nécessite un type numérique entier.
- **Pourquoi `Int` ?** La parité est définie pour les entiers. Pour d'autres types numériques (ex. `Integer`), on pourrait adapter, mais `Int` suffit pour cet exemple. Si on voulait du polymorphisme, on utiliserait `Integral a => [a] -> [a]`, mais restons simple.

#### 2. **Cas de base**
```haskell
filtrePaires [] = []
```
- **Rôle** : Pour une liste vide `[]`, il n'y a pas de nombres pairs à filtrer, donc retourne la liste vide. C'est le **cas de base** qui arrête la récursion et évite une boucle infinie.
- Sans ce cas, l'appel sur une liste vide mènerait à une erreur.

#### 3. **Cas récursif**
```haskell
filtrePaires (x:xs) 
    | even x    = x : filtrePaires xs
    | otherwise = filtrePaires xs
```
- **Décomposition** : Utilise la correspondance de motifs `(x:xs)` pour séparer le premier élément `x` (tête) du reste `xs` (queue).
- **Logique** : Utilise des **gardes** (`|`) pour tester si `x` est pair via `even x` (fonction de la Prelude qui retourne `True` si x est divisible par 2).
  - Si pair (`even x = True`), ajoute `x` à la tête du résultat filtré du reste via `x :` (cons).
  - Sinon (`otherwise`), ignore `x` et filtre récursivement seulement `xs`.
- **Pourquoi des gardes ?** Elles rendent le code lisible et séquentiel, comme un `if-then-else`. Alternative sans gardes : `if even x then x : filtrePaires xs else filtrePaires xs`.
- **Exemple de déroulement pour `[1,2,3,4]`** :
  - `filtrePaires [1,2,3,4] = filtrePaires [2,3,4]` (1 impair, ignoré)
  - `= 2 : filtrePaires [3,4]` (2 pair, ajouté)
  - `= 2 : filtrePaires [4]` (3 impair, ignoré)
  - `= 2 : (4 : filtrePaires [])` (4 pair, ajouté)
  - `= 2 : (4 : []) = [2,4]`
- **Gestion des cas extrêmes** :
  - Liste vide : `[]`.
  - Tous impairs : `[]`.
  - Tous pairs : Retourne la liste inchangée.
  - Liste d'un élément : `[2]` → `[2]` ; `[1]` → `[]`.
  - Négatifs : `even` gère les négatifs (ex. `-2` est pair).

#### 4. **Efficacité et améliorations potentielles**
- **Complexité** : O(n) en temps (traverse la liste une fois) et O(n) en espace (stack récursif, pire cas si tous pairs). Efficace pour la plupart des listes ; GHC optimise souvent la récursion tail dans ce cas.
- **Alternative avec `foldr`** : Comme dans la leçon, on peut l'abstraire : `filtrePaires = foldr (\x acc -> if even x then x : acc else acc) []`. Mais la version récursive explicite est plus proche de l'exemple `filter` de la leçon.
- **Lien avec la leçon** : Suit les étapes pour une fonction récursive : type → cas possibles (`[]` et `(x:xs)`) → base → récursion avec condition (gardes pour filtrer) → simplification. C'est un cas classique de `filter` : accumuler conditionnellement via cons.

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5, 6, 7, 8]
    putStrLn $ "La liste originale : " ++ show liste
    putStrLn $ "Les nombres pairs : " ++ show (filtrePaires liste)
```
- **Rôle** : Point d'entrée du programme pour tester la fonction avec une liste mixte (paires attendues : `[2,4,6,8]`).
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans retour significatif.
- **Explication** : `let` définit une liste de test. `putStrLn` affiche deux lignes concaténées via `++` et `show` (conversion en chaîne). Résultat affiché :
  ```
  La liste originale : [1,2,3,4,5,6,7,8]
  Les nombres pairs : [2,4,6,8]
  ```

#### 6. **Pourquoi récursif ?**
- Le filtrage est naturellement récursif : on traite la tête conditionnellement et on filtre la queue. En Haskell, cela remplace une boucle `for` avec `if`, en gardant la pureté fonctionnelle et la paresse (utile pour les listes infinies, ex. `take 5 (filtrePaires [1..])`).

### Résumé
- **Fonction `filtrePaires`** : Filtre récursivement les nombres pairs via le cas base `[]` et le cas `(x:xs)` avec gardes (`even x` pour ajouter ou ignorer).
- **Type** : `[Int] -> [Int]` pour listes d'entiers.
- **Main** : Teste avec `[1,2,3,4,5,6,7,8]` et affiche les pairs `[2,4,6,8]`.
- **Points forts** : Simple, efficace (O(n)), préserve l'ordre. Limites : Limité à `Int` ; pour d'autres types, adapter `even`.
- **Philosophie Haskell** : Met en lumière la récursion conditionnelle comme base du filtrage, alignée sur les exemples de la leçon (`filter`), favorisant des fonctions composables et expressives.
