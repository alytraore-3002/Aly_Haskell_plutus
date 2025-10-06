HC6T3 : Somme des éléments avec dossier

### Code Haskell

```haskell
-- Import nécessaire pour foldr (déjà inclus dans Prelude, mais pour clarté)
import Prelude hiding (sum)  -- Masque la somme standard pour éviter confusion

-- Définition de la fonction somme utilisant foldr
somme :: Num a => [a] -> a
somme = foldr (+) 0

-- Alternative explicite pour illustrer (équivalente à la ligne ci-dessus)
sommeExplicite :: Num a => [a] -> a
sommeExplicite xs = foldr (+) 0 xs

-- Fonction main pour tester la fonction somme
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Liste à tester (somme = 15)
    putStrLn $ "La somme des éléments de " ++ show liste ++ " est : " ++ show (somme liste)
```

### Explications détaillées

#### 1. **Signature de la fonction `somme`**
```haskell
somme :: Num a => [a] -> a
```
- **Type** : La contrainte `Num a` indique que `a` doit appartenir à la classe `Num` (types numériques comme `Int`, `Integer`, `Float`, etc., supportant l'addition `+`). La fonction prend une liste de valeurs numériques `[a]` et retourne une valeur numérique `a`.
- **Pourquoi polymorphique ?** Cela rend la fonction réutilisable pour n'importe quel type numérique, sans se limiter à `Int` ou `Integer`. Par exemple, elle fonctionne avec des entiers (`[1::Int, 2]`) ou des flottants (`[1.5::Float, 2.5]`).
- **Pourquoi `foldr` ?** `foldr` (fold right, repli à droite) est une fonction de base en Haskell qui généralise la récursion sur les listes. Sa signature est `foldr :: (a -> b -> b) -> b -> [a] -> b`. Elle "replit" une liste en appliquant une fonction binaire (ici `+`) de droite à gauche, en commençant par une valeur initiale (ici `0`).

#### 2. **Définition de `somme` avec `foldr`**
```haskell
somme = foldr (+) 0
```
- **Explication** : 
  - `(+)` est la fonction d'addition, de type `Num a => a -> a -> a` (prend deux `a` et retourne un `a`).
  - `0` est la **valeur initiale** (identité pour l'addition : ajouter 0 ne change rien).
  - `foldr (+) 0 xs` applique `+` récursivement sur la liste `xs`, en partant de la fin :
    - Pour une liste vide `[]`, retourne `0`.
    - Pour une liste non vide `(x:xs)`, calcule `x + (foldr (+) 0 xs)`.
- **Équivalence récursive** : Sans `foldr`, on écrirait une récursion explicite comme :
  ```haskell
  sommeR :: Num a => [a] -> a
  sommeR [] = 0
  sommeR (x:xs) = x + sommeR xs
  ```
  `foldr` est exactement cette récursion abstraite : elle évite de réécrire le motif de récursion (cas vide et cas cons `(x:xs)`) à chaque fois. C'est plus concis, lisible et réutilisable.
- **Exemple de déroulement pour `[1,2,3]`** :
  - `somme [1,2,3] = foldr (+) 0 [1,2,3] = 1 + (foldr (+) 0 [2,3])`
  - `= 1 + (2 + (foldr (+) 0 [3]))`
  - `= 1 + (2 + (3 + (foldr (+) 0 [])))`
  - `= 1 + (2 + (3 + 0)) = 1 + (2 + 3) = 1 + 5 = 6`
- **Version explicite** : `sommeExplicite xs = foldr (+) 0 xs` montre que la définition est une section de fonction (partielle application de `foldr`), ce qui est idiomatique en Haskell pour la composition.

#### 3. **Avantages de `foldr` pour la somme**
- **Généralisation** : `foldr` peut remplacer la plupart des fonctions récursives sur listes (comme `sum`, `product`, `length`, `reverse`). Par exemple, pour le produit : `product = foldr (*) 1`.
- **Lazy evaluation** : En Haskell, `foldr` est paresseux : il n'évalue que les parties nécessaires de la liste, ce qui est efficace pour les listes infinies ou partielles (ex. : `take 5 (somme [1..])` ne calcule que les 5 premiers).
- **Direction du repli** : À droite (de la fin vers le début), ce qui préserve l'ordre naturel pour les opérations associatives comme `+`. Contrairement à `foldl` (repli à gauche), qui peut être moins efficace sans optimisation (risque de stack overflow pour de grandes listes).
- **Gestion des cas extrêmes** :
  - Liste vide `[]` : Retourne `0` (identité additive).
  - Liste avec un élément `[x]` : `x + 0 = x`.
  - Pas de gestion spéciale pour les négatifs ou zéros, car `+` les gère naturellement.

#### 4. **Alternative avec `sommeExplicite`**
- C'est la même chose que `somme`, mais avec l'argument `xs` explicite. Utile pour débutants : elle montre que `somme = foldr (+) 0` est une application partielle (currying en Haskell, où les fonctions sont naturellement curryfiées).

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]
    putStrLn $ "La somme des éléments de " ++ show liste ++ " est : " ++ show (somme liste)
```
- **Rôle** : Point d'entrée du programme pour tester et afficher le résultat de la somme.
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans retour significatif.
- **Explication** : `let liste = [1, 2, 3, 4, 5]` définit une liste de test (somme = 15). `putStrLn` affiche le message concaténé via `++` et `show` (conversion en chaîne). Résultat affiché : `"La somme des éléments de [1,2,3,4,5] est : 15"`.

#### 6. **Lien avec la récursion et la leçon**
- Comme vu dans la leçon, la récursion sur listes suit le motif `[]` (base) et `(x:xs)` (récursif). `foldr` encapsule ce motif : il est "récursion sans les détails fastidieux". Pour la somme, c'est plus élégant que d'écrire la récursion manuelle, et cela suit les étapes : type → cas → base → récursion → simplification.

### Résumé
- **Fonction `somme`** : Utilise `foldr (+) 0` pour replier une liste numérique en sommant ses éléments, de manière récursive et abstraite.
- **Type** : Polymorphique sur `Num a` pour toute valeur numérique.
- **Main** : Teste avec `[1,2,3,4,5]` et affiche la somme (15).
- **Points forts** : Concis, efficace (paresseux), généralisable à d'autres opérations. Limites : Nécessite une classe `Num` ; pour des types non-numériques, adapter la fonction binaire.
- **Philosophie Haskell** : `foldr` incarne la puissance des abstractions fonctionnelles : transforme une récursion basique en outil réutilisable, favorisant la composition et la clarté. Idéal pour remplacer des implémentations manuelles comme dans les exemples de la leçon (length, reverse, etc.).
