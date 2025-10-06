HC6T9 : Implémentation de map

### Code Haskell

```haskell
-- Définition de la fonction applique récursive (comme map)
applique :: (a -> b) -> [a] -> [b]
applique _ [] = []  -- Cas de base : liste vide retourne liste vide
applique f (x:xs) = f x : applique f xs  -- Cas récursif : applique f à x et au reste

-- Fonction main pour tester la fonction applique
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Liste à tester
    let double = (* 2)  -- Fonction à appliquer (multiplie par 2)
    putStrLn $ "La liste originale : " ++ show liste
    putStrLn $ "Après application de (*2) : " ++ show (applique double liste)
```

### Explications détaillées

#### 1. **Signature de la fonction `applique`**
```haskell
applique :: (a -> b) -> [a] -> [b]
```
- **Type** : La fonction est hautement polymorphique : elle prend une fonction `f` de type `(a -> b)` (qui transforme un `a` en `b`), une liste `[a]`, et retourne une liste `[b]` où chaque élément a été transformé par `f`. Cela fonctionne pour n'importe quels types `a` et `b` (ex. : `a = Int`, `b = Int` pour doubler ; ou `a = String`, `b = String` pour uppercaser).
- **Pourquoi polymorphique ?** L'application ne dépend pas des types spécifiques : `f` gère la transformation, et la liste est préservée en structure. Cela rend la fonction réutilisable, comme `map` : `applique (+1) [1,2]` ou `applique (++ "!") ["hello"]`.
- **Currying** : L'ordre `(a -> b) -> [a] -> [b]` permet des applications partielles (ex. : `applique (+1)` retourne une fonction qui incrémente une liste).

#### 2. **Cas de base**
```haskell
applique _ [] = []
```
- **Rôle** : Pour une liste vide `[]`, il n'y a rien à transformer, donc retourne la liste vide. Le `_` ignore la fonction `f` (inutile ici). C'est le **cas de base** qui arrête la récursion.
- Sans ce cas, l'appel sur une liste vide mènerait à une erreur.

#### 3. **Cas récursif**
```haskell
applique f (x:xs) = f x : applique f xs
```
- **Décomposition** : Utilise la correspondance de motifs `(x:xs)` pour séparer le premier élément `x` (tête) du reste `xs` (queue).
- **Logique** : Applique `f` à `x` pour obtenir le premier élément du résultat, puis construit récursivement la liste transformée du reste via `:` (cons). Cela préserve l'ordre des éléments.
- **Pourquoi `:` ?** C'est la construction de liste : `f x` devient la tête, suivie de la queue transformée.
- **Exemple de déroulement pour `applique (*2) [1,2,3]`** :
  - `applique (*2) [1,2,3] = (*2) 1 : applique (*2) [2,3]` → `2 : applique (*2) [2,3]`
  - `= 2 : ((*2) 2 : applique (*2) [3])` → `2 : (4 : applique (*2) [3])`
  - `= 2 : (4 : ((*2) 3 : applique (*2) []))` → `2 : (4 : (6 : [])) = [2,4,6]`
- **Gestion des cas extrêmes** :
  - Liste vide : `[]`.
  - Liste d'un élément `[x]` : `[f x]`.
  - Fonction identité (`id`) : Retourne la liste inchangée.
  - Listes infinies : Produit une liste infinie transformée (paresseux, ex. : `take 5 (applique (+1) [1..]) = [2,3,4,5,6]`).

#### 4. **Efficacité et améliorations potentielles**
- **Complexité** : O(n) en temps (applique `f` une fois par élément) et O(n) en espace (stack récursif). Efficace et optimisable par GHC (récursion tail-optimisée ici).
- **Alternative avec `foldr`** : Comme dans la leçon, on peut l'abstraire : `applique f = foldr (\x acc -> f x : acc) []`. Cela utilise le repli à droite pour construire la liste de fin en début, mais le résultat est identique.
- **Lien avec la leçon** : Suit les étapes pour une fonction récursive : type → cas possibles (`[]` et `(x:xs)`) → base → récursion (appliquer à la tête et cons) → simplification. C'est exactement l'exemple `map` : transformer conditionnellement ou systématiquement les éléments.

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]
    let double = (* 2)
    putStrLn $ "La liste originale : " ++ show liste
    putStrLn $ "Après application de (*2) : " ++ show (applique double liste)
```
- **Rôle** : Point d'entrée du programme pour tester la fonction avec une liste d'entiers et la fonction de doublement (résultat attendu : `[2,4,6,8,10]`).
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans retour significatif.
- **Explication** : `let double = (* 2)` définit une section de fonction (multiplication partielle). `putStrLn` affiche deux lignes concaténées via `++` et `show` (conversion en chaîne). Résultat affiché :
  ```
  La liste originale : [1,2,3,4,5]
  Après application de (*2) : [2,4,6,8,10]
  ```

#### 6. **Pourquoi récursif ?**
- L'application à chaque élément est intrinsèquement récursive : on traite la tête et on applique au reste. En Haskell, cela remplace une boucle `forEach` impérative, en gardant la pureté et la paresse (idéal pour les streams ou listes partielles).

### Résumé
- **Fonction `applique`** : Applique récursivement une fonction `f` à chaque élément d'une liste via le cas base `[]` et le cas `(x:xs) = f x : applique f xs`.
- **Type** : Polymorphique `(a -> b) -> [a] -> [b]` pour toute transformation.
- **Main** : Teste avec `[1,2,3,4,5]` et `(*2)`, affiche `[2,4,6,8,10]`.
- **Points forts** : Simple, efficace (O(n)), hautement réutilisable. Limites : Stack pour très grandes listes ; `foldr` pour plus d'abstraction.
- **Philosophie Haskell** : Incarne `map` comme pilier de la programmation fonctionnelle : composition récursive pour transformer des structures, alignée sur les exemples de la leçon.
