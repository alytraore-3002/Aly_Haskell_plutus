HC6T7 : Taille d'une liste

### Code Haskell

```haskell
-- Définition de la fonction longueur récursive
longueur :: [a] -> Int
longueur [] = 0  -- Cas de base : liste vide a longueur 0
longueur (_:xs) = 1 + longueur xs  -- Cas récursif : ajoute 1 pour la tête et compte le reste

-- Fonction main pour tester la fonction longueur
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Liste à tester (longueur = 5)
    putStrLn $ "La longueur de " ++ show liste ++ " est : " ++ show (longueur liste)
    
    let listeChaine = ['a'..'z']  -- Liste de caractères (longueur = 26)
    putStrLn $ "La longueur de " ++ show listeChaine ++ " est : " ++ show (longueur listeChaine)
```

### Explications détaillées

#### 1. **Signature de la fonction `longueur`**
```haskell
longueur :: [a] -> Int
```
- **Type** : La fonction est polymorphique (`[a] -> Int`), ce qui signifie qu'elle fonctionne avec n'importe quel type `a` (entiers, caractères, chaînes, etc.), tant que la liste est de type `[a]`. Elle prend une liste en entrée et retourne un entier `Int` représentant sa longueur.
- **Pourquoi polymorphique ?** Le calcul de longueur ne dépend pas des valeurs des éléments (pas d'opérations sur `a`), seulement du nombre d'éléments. Cela rend la fonction réutilisable, par exemple pour `[1,2,3]` (entiers) ou `['a'..'z']` (caractères).
- **Pourquoi `Int` ?** Les longueurs de listes finies sont des entiers positifs ou zéro. Pour des listes très longues, on pourrait utiliser `Integer`, mais `Int` suffit pour la plupart des cas (limite ~2 milliards sur 32 bits).

#### 2. **Cas de base**
```haskell
longueur [] = 0
```
- **Rôle** : Pour une liste vide `[]`, la longueur est trivialement 0. C'est le **cas de base** qui arrête la récursion et évite une boucle infinie.
- Sans ce cas, l'appel sur une liste vide ne serait pas défini, menant à une erreur.

#### 3. **Cas récursif**
```haskell
longueur (_:xs) = 1 + longueur xs
```
- **Décomposition** : Utilise la correspondance de motifs `(_:xs)` pour séparer la tête (ignorée avec `_`, car on n'a pas besoin de sa valeur) du reste `xs` (queue).
- **Logique** : Ajoute 1 pour l'élément courant (tête) et calcule récursivement la longueur du reste `xs`. Cela compte progressivement tous les éléments.
- **Pourquoi `_` ?** On ignore la tête car seule sa présence compte (pas sa valeur). Cela simplifie le motif et évite d'introduire une variable inutile.
- **Exemple de déroulement pour `[1,2,3]`** :
  - `longueur [1,2,3] = 1 + longueur [2,3]`
  - `= 1 + (1 + longueur [3])`
  - `= 1 + (1 + (1 + longueur []))`
  - `= 1 + (1 + (1 + 0)) = 1 + (1 + 1) = 1 + 2 = 3`
- **Gestion des cas extrêmes** :
  - Liste vide : 0.
  - Liste d'un élément `[x]` : `1 + longueur [] = 1`.
  - Listes infinies : Ne termine pas (comportement attendu ; Haskell supporte les listes paresseuses, mais la récursion sature la pile).

#### 4. **Efficacité et améliorations potentielles**
- **Complexité** : O(n) en temps (traverse la liste une fois) et O(n) en espace (stack récursif). Pour de très grandes listes, une version tail-recursive avec accumulation est possible (ex. : utiliser `foldl`), mais la récursion simple est optimisée par GHC (compilateur Haskell) via l'optimisation des appels terminaux dans certains cas.
- **Alternative avec `foldr`** : Comme dans la leçon, on peut l'abstraire : `longueur = foldr (\_ acc -> 1 + acc) 0`. Mais la version récursive explicite est plus pédagogique et suit directement les étapes : type → cas possibles (`[]` et `(_:xs)`) → base → récursion → simplification.
- **Lien avec la leçon** : Exactement l'exemple `length'` : on ajoute 1 pour chaque élément via récursion sur la queue, ignorant la tête avec `_`.

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]
    putStrLn $ "La longueur de " ++ show liste ++ " est : " ++ show (longueur liste)
    
    let listeChaine = ['a'..'z']
    putStrLn $ "La longueur de " ++ show listeChaine ++ " est : " ++ show (longueur listeChaine)
```
- **Rôle** : Point d'entrée du programme pour tester la fonction avec une liste d'entiers (longueur 5) et une de caractères (longueur 26).
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans retour significatif.
- **Explication** : `let` définit deux listes de test. `putStrLn` affiche deux lignes concaténées via `++` et `show` (conversion en chaîne). Résultat affiché :
  ```
  La longueur de [1,2,3,4,5] est : 5
  La longueur de ['a','b','c',...,'z'] est : 26
  ```

#### 6. **Pourquoi récursif ?**
- Le comptage d'éléments est intrinsèquement récursif : on traite un élément à la fois et on compte le reste. En Haskell, cela remplace élégamment les boucles `for` impératives, en suivant la philosophie fonctionnelle : définition en termes de soi-même, avec pureté et paresse.

### Résumé
- **Fonction `longueur`** : Calcule récursivement la longueur d'une liste via le cas base `0` pour `[]` et le cas `(_:xs) = 1 + longueur xs`.
- **Type** : Polymorphique `[a] -> Int` pour toute liste.
- **Main** : Teste avec `[1,2,3,4,5]` (5) et `['a'..'z']` (26), affiche les résultats.
- **Points forts** : Simple, efficace (O(n)), polymorphique. Limites : Stack pour très grandes listes ; pour l'optimisation, utiliser `foldl' (+1) 0`.
- **Philosophie Haskell** : Illustre la récursion comme alternative naturelle aux boucles, avec une correspondance de motifs intuitive pour décomposer les listes, comme dans les exemples de la leçon.
