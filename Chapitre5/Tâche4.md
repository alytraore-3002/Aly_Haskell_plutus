HC5T4 : Utiliser les fonctions lambda

### Code Haskell

```haskell
-- Fonction main pour tester la fonction lambda
main :: IO ()
main = do
  let x1 = 5
  let x2 = 12
  let x3 = 10
  print $ (\x -> x > 10) x1  -- Résultat attendu : False
  print $ (\x -> x > 10) x2  -- Résultat attendu : True
  print $ (\x -> x > 10) x3  -- Résultat attendu : False
```

### Explications détaillées

1. **Remplacement de `biggerThan10`** :
   - La fonction originale `biggerThan10 x = x > 10` prend un entier `x` et retourne un booléen (`True` si `x > 10`, `False` sinon).
   - Elle est réécrite comme une fonction lambda `\x -> x > 10`, qui fait exactement la même chose : prend un entier `x` et évalue `x > 10` pour retourner un `Bool`.
   - La lambda est anonyme, évitant une définition nommée, ce qui est idéal pour une fonction utilisée une seule fois, comme mentionné dans votre description des fonctions lambda.

2. **Application directe de la fonction lambda** :
   - Puisque vous avez spécifié de ne pas utiliser `any` et que la fonction prend un entier et retourne un booléen, j’applique la lambda `\x -> x > 10` directement à des entiers individuels.
   - Dans `main`, trois entiers sont testés :
     - `x1 = 5` : `(\x -> x > 10) 5` évalue `5 > 10`, ce qui donne `False`.
     - `x2 = 12` : `(\x -> x > 10) 12` évalue `12 > 10`, ce qui donne `True`.
     - `x3 = 10` : `(\x -> x > 10) 10` évalue `10 > 10`, ce qui donne `False`.
   - Chaque application de la lambda produit un booléen, conformément à la signature de `biggerThan10 :: Int -> Bool`.

3. **Fonction `main`** :
   - La fonction `main :: IO ()` définit trois entiers `x1 = 5`, `x2 = 12`, et `x3 = 10` pour tester la lambda.
   - Chaque expression `(\x -> x > 10) xN` applique la lambda à un entier, produisant un résultat booléen.
   - `print $ (\x -> x > 10) xN` affiche chaque résultat (`False`, `True`, `False`) dans la console.
   - Cette approche montre directement l’effet de la lambda sur des entiers individuels, sans impliquer de liste ou de fonction comme `map` ou `any`.

4. **Pourquoi cette approche** :
   - Votre remarque « la fonction prend un entier et retourne un booléen » suggère que vous voulez voir la lambda utilisée dans son rôle de base : transformer un seul entier en un booléen.
   - Contrairement à la réponse précédente qui utilisait `map` pour appliquer la lambda à une liste entière, ici, je me concentre sur des applications individuelles pour respecter strictement la signature `Int -> Bool`.
   - Cela évite d’introduire une liste non demandée ou une fonction comme `map`, tout en démontrant l’utilisation de la lambda comme remplacement direct de `biggerThan10`.

5. **Avantages de la fonction lambda** :
   - La lambda `\x -> x > 10` est concise et élimine le besoin de définir une fonction nommée comme `biggerThan10`, ce qui est utile pour une fonction à usage unique.
   - Elle peut être appliquée directement à un argument, comme montré ici, ou passée à d’autres fonctions si nécessaire (mais ici, nous évitons `any` ou `map`).
   - Cette approche est claire et met en évidence la simplicité des lambdas pour des prédicats simples.

6. **Différence avec les réponses précédentes** :
   - Les réponses précédentes utilisaient `any` ou `map` pour appliquer la lambda à une liste, produisant soit un seul booléen (`any`) soit une liste de booléens (`map`).
   - Ici, je respecte votre clarification en appliquant la lambda directement à des entiers individuels, produisant un booléen par application, ce qui correspond exactement à la signature de `biggerThan10` (`Int -> Bool`).
   - J’ai choisi des entiers spécifiques (`5`, `12`, `10`) pour montrer des cas variés (inférieur, supérieur, et égal à 10), mais aucun contexte de liste n’est introduit au-delà de ce qui était implicite dans les échanges précédents.

7. **Approche fonctionnelle** :
   - L’utilisation de la lambda dans `main` illustre le style fonctionnel de Haskell : chaque application est pure (même entrée, même sortie) et déclarative.
   - Bien que Haskell soit paresseux, ici, chaque application de la lambda est évaluée immédiatement pour l’affichage avec `print`.

### Résumé des explications

La fonction `biggerThan10 x = x > 10` est réécrite comme une fonction lambda `\x -> x > 10`, appliquée directement à des entiers individuels (`5`, `12`, `10`) dans `main`. Chaque application produit un booléen (`False`, `True`, `False`), affiché avec `print`, respectant la signature `Int -> Bool`. Cette approche évite `any` et les listes non demandées, utilisant la lambda de manière concise pour remplacer une fonction nommée, conformément à son utilité pour des fonctions à usage unique. Le code est simple, fonctionnel, et met en évidence l’application directe de la lambda à des entiers.
