HC6T6 : Existence d'un élément dans une liste

### Code Haskell

```haskell
-- Définition de la fonction membre récursive
membre :: Eq a => a -> [a] -> Bool
membre _ [] = False  -- Cas de base : élément non trouvé dans liste vide
membre x (y:ys) = x == y || membre x ys  -- Cas récursif : vérifie si x == y ou dans le reste

-- Fonction main pour tester la fonction membre
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Liste à tester
    let element = 3  -- Élément à chercher (existe)
    putStrLn $ "L'élément " ++ show element ++ " existe-t-il dans " ++ show liste ++ " ? " ++ show (membre element liste)
    
    let elementAbsent = 6  -- Élément absent
    putStrLn $ "L'élément " ++ show elementAbsent ++ " existe-t-il dans " ++ show liste ++ " ? " ++ show (membre elementAbsent liste)
```

### Explications détaillées

#### 1. **Signature de la fonction `membre`**
```haskell
membre :: Eq a => a -> [a] -> Bool
```
- **Type** : La contrainte `Eq a` indique que le type `a` doit appartenir à la classe `Eq` (types supportant l'égalité `==`, comme `Int`, `Char`, `String`, etc.). La fonction prend un élément `a` et une liste `[a]`, et retourne un `Bool` (`True` si l'élément existe, `False` sinon).
- **Pourquoi `Eq a` ?** L'égalité `==` est nécessaire pour comparer l'élément avec ceux de la liste. Sans cela, la fonction ne pourrait pas vérifier les correspondances. Elle est polymorphique : fonctionne pour n'importe quel type égalisable (ex. : `[1,2]` pour `Int`, `["a","b"]` pour `String`).
- **Ordre des arguments** : `a -> [a] -> Bool` suit le style curryfié d'Haskell, permettant des applications partielles (ex. : `membre 3` retourne une fonction qui teste si 3 est dans une liste).

#### 2. **Cas de base**
```haskell
membre _ [] = False
```
- **Rôle** : Pour une liste vide `[]`, l'élément ne peut pas exister, donc retourne `False`. Le `_` ignore l'élément recherché (inutile ici). C'est le **cas de base** qui arrête la récursion.
- Sans ce cas, l'appel sur une liste vide mènerait à une erreur ou une boucle infinie.

#### 3. **Cas récursif**
```haskell
membre x (y:ys) = x == y || membre x ys
```
- **Décomposition** : Utilise la correspondance de motifs `(y:ys)` pour séparer le premier élément `y` (tête) du reste `ys` (queue).
- **Logique** : Vérifie si `x == y` (égalité immédiate). Si oui, retourne `True` via `||` (ou logique). Sinon, cherche récursivement dans `ys`.
- **Pourquoi `||` ?** C'est une courte-circuit : si `x == y` est `True`, `membre x ys` n'est même pas évalué (efficace pour les listes longues).
- **Exemple de déroulement pour `membre 3 [1,2,3,4]`** :
  - `membre 3 [1,2,3,4] = 3 == 1 || membre 3 [2,3,4]` → `False || membre 3 [2,3,4]`
  - `= False || (3 == 2 || membre 3 [3,4])` → `False || (False || membre 3 [3,4])`
  - `= False || (False || (3 == 3 || membre 3 [4]))` → `False || (False || (True || ...)) = True`
- **Gestion des cas extrêmes** :
  - Liste vide : `False`.
  - Élément en tête : `True` immédiatement.
  - Élément absent : Traverse toute la liste, retourne `False`.
  - Listes avec doublons : Retourne `True` dès la première occurrence (pas de comptage).
  - Listes infinies : Ne termine pas (comportement attendu ; Haskell supporte les listes paresseuses).

#### 4. **Efficacité et améliorations potentielles**
- **Complexité** : O(n) en temps (traverse au pire toute la liste), O(n) en espace (stack récursif). Pour de très grandes listes, une version tail-recursive avec accumulation est possible, mais inutile ici car la récursion est simple.
- **Alternative avec `foldr`** : Comme dans la leçon, on peut l'abstraire : `membre x = foldr (\y acc -> x == y || acc) False`. Mais la version récursive explicite est plus pédagogique.
- **Lien avec la leçon** : Suit les étapes pour créer une fonction récursive : type → cas possibles (`[]` et `(y:ys)`) → base → récursion (avec `||` pour combiner) → simplification (le `_` ignore l'inutile).

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]
    let element = 3
    putStrLn $ "L'élément " ++ show element ++ " existe-t-il dans " ++ show liste ++ " ? " ++ show (membre element liste)
    
    let elementAbsent = 6
    putStrLn $ "L'élément " ++ show elementAbsent ++ " existe-t-il dans " ++ show liste ++ " ? " ++ show (membre elementAbsent liste)
```
- **Rôle** : Point d'entrée du programme pour tester la fonction avec un élément présent (3 → `True`) et absent (6 → `False`).
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans retour significatif.
- **Explication** : Définit une liste et deux éléments de test. `putStrLn` affiche deux lignes concaténées via `++` et `show` (conversion en chaîne). Résultat affiché :
  ```
  L'élément 3 existe-t-il dans [1,2,3,4,5] ? True
  L'élément 6 existe-t-il dans [1,2,3,4,5] ? False
  ```

#### 6. **Pourquoi récursif ?**
- La recherche dans une liste est intrinsèquement récursive : on traite la tête et on cherche dans la queue. En Haskell, cela remplace élégamment les boucles `while` impératives, en suivant la philosophie fonctionnelle : définition en termes de soi-même, avec paresse et pureté.

### Résumé
- **Fonction `membre`** : Détermine récursivement si un élément existe dans une liste via le cas base `False` pour `[]` et le cas `(y:ys) = x == y || membre x ys`.
- **Type** : `Eq a => a -> [a] -> Bool` pour types égalisables.
- **Main** : Teste avec `[1,2,3,4,5]` et éléments 3 (`True`) / 6 (`False`), affiche les résultats.
- **Points forts** : Simple, efficace (O(n)), courte-circuit avec `||`. Limites : Ne gère pas les types non-`Eq` ; pour les ensembles, préférez `Set` de `Data.Set`.
- **Philosophie Haskell** : Renforce la récursion comme outil naturel pour les structures de données, avec une correspondance de motifs claire et une évaluation paresseuse optimisée.
