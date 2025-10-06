HC6T5 : Inverser une liste (récursif)

### Code Haskell

```haskell
-- Définition de la fonction inverse récursive
inverse :: [a] -> [a]
inverse [] = []  -- Cas de base : liste vide reste vide
inverse (x:xs) = inverse xs ++ [x]  -- Cas récursif : inverse le reste et ajoute x à la fin

-- Fonction main pour tester la fonction inverse
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]  -- Liste à tester
    putStrLn $ "La liste originale : " ++ show liste
    putStrLn $ "La liste inversée : " ++ show (inverse liste)
```

### Explications détaillées

#### 1. **Signature de la fonction `inverse`**
```haskell
inverse :: [a] -> [a]
```
- **Type** : La fonction est polymorphique (`[a] -> [a]`), ce qui signifie qu'elle fonctionne avec n'importe quel type `a` (entiers, chaînes, etc.), tant que la liste est de type `[a]`. Elle prend une liste en entrée et retourne une liste du même type.
- **Pourquoi polymorphique ?** L'inversion ne dépend pas des valeurs des éléments (pas d'opérations sur `a`), seulement de leur ordre. Cela rend la fonction réutilisable, par exemple pour `[1,2,3]` ou `["a","b"]`.

#### 2. **Cas de base**
```haskell
inverse [] = []
```
- **Rôle** : Pour une liste vide `[]`, l'inverse est trivialement la liste vide elle-même. C'est le **cas de base** qui arrête la récursion et évite une boucle infinie.
- Sans ce cas, l'appel sur une liste vide ne serait pas défini, menant à une erreur.

#### 3. **Cas récursif**
```haskell
inverse (x:xs) = inverse xs ++ [x]
```
- **Décomposition** : Utilise la correspondance de motifs `(x:xs)` pour séparer le premier élément `x` (tête) du reste `xs` (queue).
- **Logique** : Pour inverser la liste, on inverse d'abord le reste `xs` (récursivement), puis on ajoute `x` à la fin de ce résultat via la concaténation `++ [x]`.
- **Pourquoi ça marche ?** Cela déplace progressivement les éléments vers la fin : chaque appel récursif "pousse" l'élément courant à la position finale.
- **Exemple de déroulement pour `[1,2,3]`** :
  - `inverse [1,2,3] = inverse [2,3] ++ [1]`
  - `inverse [2,3] = inverse [3] ++ [2]`
  - `inverse [3] = inverse [] ++ [3] = [] ++ [3] = [3]`
  - Remontant : `inverse [2,3] = [3] ++ [2] = [3,2]`
  - `inverse [1,2,3] = [3,2] ++ [1] = [3,2,1]`
- **Gestion des cas extrêmes** :
  - Liste vide : Retourne `[]`.
  - Liste d'un élément `[x]` : `inverse [x] = inverse [] ++ [x] = [x]`.
  - Listes imbriquées ou infinies : Fonctionne pour les finies ; pour les infinies, la récursion ne termine pas (comportement attendu).

#### 4. **Efficacité et améliorations potentielles**
- **Complexité** : O(n²) en temps, car chaque `++` copie la liste accumulée (coût linéaire par appel). Pour de grandes listes (n > 1000), c'est inefficace. Une version optimisée utilise l'accumulation (tail-recursive) : 
  ```haskell
  inverseAcc :: [a] -> [a]
  inverseAcc = reverseAcc []
    where reverseAcc acc [] = acc
          reverseAcc acc (x:xs) = reverseAcc (x:acc) xs
  ```
  Cela est O(n) et tail-recursive (optimisable en boucle par GHC).
- **Lien avec la leçon** : Comme dans l'exemple `reverse'` de la leçon, cette implémentation suit les étapes : type → cas possibles (`[]` et `(x:xs)`) → base → récursion → simplification (pas besoin de `x` dans le motif, mais gardé pour clarté).

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let liste = [1, 2, 3, 4, 5]
    putStrLn $ "La liste originale : " ++ show liste
    putStrLn $ "La liste inversée : " ++ show (inverse liste)
```
- **Rôle** : Point d'entrée du programme pour tester et afficher l'original et l'inversé.
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans retour significatif.
- **Explication** : `let liste = [1, 2, 3, 4, 5]` définit une liste de test (inversée = `[5,4,3,2,1]`). `putStrLn` affiche deux lignes concaténées via `++` et `show` (conversion en chaîne). Résultat affiché :
  ```
  La liste originale : [1,2,3,4,5]
  La liste inversée : [5,4,3,2,1]
  ```

#### 6. **Pourquoi récursif ?**
- L'inversion est naturellement récursive : elle repose sur le traitement du reste de la liste. En Haskell, cela évite les boucles impératives (non natives) et suit la philosophie fonctionnelle : définir en termes de soi-même, avec un cas de base clair.

### Résumé
- **Fonction `inverse`** : Inverse une liste récursivement via le cas base `[]` et le cas `(x:xs) = inverse xs ++ [x]`.
- **Type** : Polymorphique `[a] -> [a]` pour toute liste.
- **Main** : Teste avec `[1,2,3,4,5]` et affiche l'original et l'inversé.
- **Points forts** : Simple, fidèle à la définition récursive de la leçon. Limites : O(n²) ; pour l'efficacité, utiliser une version accumulatrice.
- **Philosophie Haskell** : Illustre la récursion comme alternative élégante aux boucles, avec une correspondance de motifs intuitive pour décomposer les structures de données.
