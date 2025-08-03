-- HC1T5 - Tâche 5 : Paresse en Haskell

```haskell
-- Génère une liste infinie de nombres entiers à partir de 1
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

-- Extrait les n premiers éléments d'une liste
takeN :: Int -> [a] -> [a]
takeN n xs = take n xs

-- Composition pour obtenir les n premiers nombres
getFirstN :: Int -> [Int]
getFirstN = takeN `flip` infiniteNumbers

-- Fonction principale pour tester
main :: IO ()
main = do
    let n = 5
    putStrLn $ "Nombre d'éléments à extraire: " ++ show n
    putStrLn $ "Les " ++ show n ++ " premiers nombres: " ++ show (getFirstN n)
```

### Explications :

1. **Fonction `infiniteNumbers`** :
   - **Signature** : `infiniteNumbers :: [Int]` indique que la fonction retourne une liste infinie d'entiers.
   - **Définition** : Utilise la syntaxe `[1..]` pour générer une liste infinie d'entiers commençant à 1 (1, 2, 3, ...). En Haskell, les listes infinies sont gérées grâce à l'évaluation paresseuse.
   - **Pureté** : La fonction est pure, car elle produit une liste déterministe sans effets de bord.

2. **Fonction `takeN`** :
   - **Signature** : `takeN :: Int -> [a] -> [a]` indique que la fonction prend un entier `n` et une liste de type polymorphe `[a]`, et retourne une liste des `n` premiers éléments.
   - **Définition** : Utilise la fonction standard `take` pour extraire les `n` premiers éléments de la liste donnée.
   - **Pureté** : La fonction est pure, dépendant uniquement de `n` et de la liste d'entrée.

3. **Fonction `getFirstN`** :
   - **Signature** : `getFirstN :: Int -> [Int]` indique que la fonction prend un entier `n` et retourne une liste des `n` premiers nombres entiers.
   - **Définition** : Utilise `flip` pour composer `takeN` avec `infiniteNumbers`. L'expression `takeN `flip` infiniteNumbers` applique `takeN n` à `infiniteNumbers`, équivalent à `take n infiniteNumbers`.
   - **Pureté** : La fonction est pure, combinant deux fonctions pures.

4. **Exemple d'exécution** :
   - Pour `n = 5` :
     - `infiniteNumbers` génère `[1, 2, 3, 4, 5, ...]`.
     - `getFirstN 5` applique `take 5`, donnant `[1, 2, 3, 4, 5]`.

Sortie dans la console :
```
Nombre d'éléments à extraire: 5
Les 5 premiers nombres: [1,2,3,4,5]
```

### Remarques :
- **Pureté** : Toutes les fonctions sont pures, ne dépendant que de leurs entrées et sans effets de bord.
- **Évaluation paresseuse** : Haskell évalue `infiniteNumbers` uniquement pour les éléments nécessaires (ici, les `n` premiers), rendant la gestion des listes infinies efficace.
- **Composition** : `getFirstN` utilise une composition implicite via `flip`, montrant la flexibilité de la programmation fonctionnelle en Haskell.
- **Gestion des cas limites** : Si `n <= 0`, `takeN` retourne une liste vide. La fonction fonctionne pour toute liste, pas seulement `infiniteNumbers`.
