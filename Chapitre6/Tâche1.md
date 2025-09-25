HC6T1 : Factorielle (récursif)

### Code Haskell

```haskell
-- Définition de la fonction factorielle récursive
factorielle :: Integer -> Integer
factorielle 0 = 1  -- Cas de base : 0! = 1
factorielle n = n * factorielle (n - 1)  -- Cas récursif : n! = n * (n-1)!

-- Fonction main pour tester la fonction factorielle
main :: IO ()
main = do
    let n = 5  -- Nombre à tester
    putStrLn $ "La factorielle de " ++ show n ++ " est : " ++ show (factorielle n)
```

### Explications détaillées

#### 1. **Signature de la fonction `factorielle`**
```haskell
factorielle :: Integer -> Integer
```
- **Type** : La fonction prend un `Integer` (nombre entier potentiellement grand) en entrée et retourne un `Integer` en sortie.
- **Pourquoi `Integer` ?** Contrairement à `Int`, qui est borné (taille limitée par l'architecture, par exemple 32 ou 64 bits), `Integer` en Haskell supporte des nombres arbitrairement grands, ce qui est idéal pour la factorielle, car les résultats croissent rapidement (par exemple, `20!` est très grand).
- La signature indique que la fonction est pure (pas d'effets de bord, elle retourne toujours le même résultat pour la même entrée).

#### 2. **Cas de base**
```haskell
factorielle 0 = 1
```
- En mathématiques, la factorielle de 0 est définie comme étant 1 (`0! = 1`). Cela sert de **cas de base** pour arrêter la récursion.
- Sans ce cas, la récursion continuerait indéfiniment (par exemple, pour `n = 0`, on appellerait `factorielle (-1)`, puis `factorielle (-2)`, etc.), ce qui provoquerait une erreur (stack overflow).

#### 3. **Cas récursif**
```haskell
factorielle n = n * factorielle (n - 1)
```
- Pour tout `n` non nul, la factorielle est définie par `n! = n * (n-1)!`.
- La fonction s'appelle elle-même avec `n - 1`, ce qui réduit progressivement l'argument jusqu'à atteindre le cas de base (`n = 0`).
- Exemple pour `n = 3` :
  - `factorielle 3 = 3 * factorielle 2`
  - `factorielle 2 = 2 * factorielle 1`
  - `factorielle 1 = 1 * factorielle 0`
  - `factorielle 0 = 1`
  - En remontant : `factorielle 1 = 1 * 1 = 1`, `factorielle 2 = 2 * 1 = 2`, `factorielle 3 = 3 * 2 = 6`.

#### 4. **Gestion des cas non définis**
- Le code ne gère pas explicitement les nombres négatifs. En Haskell, si `factorielle` est appelée avec un nombre négatif, la récursion ne s'arrêtera pas (car `n - 1` continue à décroître indéfiniment). Cela pourrait être amélioré en ajoutant une garde ou une erreur explicite, mais ici, on suppose que l'entrée est un entier non négatif pour simplifier.

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let n = 5
    putStrLn $ "La factorielle de " ++ show n ++ " est : " ++ show (factorielle n)
```
- **Rôle** : La fonction `main` est le point d'entrée du programme, utilisée pour exécuter des actions dans le monde réel (ici, afficher un résultat).
- **Type `IO ()`** : Indique que `main` effectue des entrées/sorties (affichage) et ne retourne rien de significatif (`()` est le type "unit", équivalent à "void" dans d'autres langages).
- **Explication des lignes** :
  - `let n = 5` : Définit une constante `n` pour tester la fonction avec `n = 5`.
  - `putStrLn` : Affiche une chaîne de caractères dans la console.
  - `show n` : Convertit l'entier `n` en une chaîne (par exemple, `5` devient `"5"`).
  - `show (factorielle n)` : Calcule `factorielle 5` (qui vaut `120`) et le convertit en chaîne (`"120"`).
  - `++` : Concatène les chaînes pour former le message final, par exemple : `"La factorielle de 5 est : 120"`.

#### 6. **Pourquoi récursif ?**
- La récursion est naturelle en Haskell, un langage fonctionnel qui privilégie les définitions mathématiques. La factorielle est un exemple classique où la définition récursive (`n! = n * (n-1)!`) se traduit directement en code.
- Une alternative itérative (avec une boucle) est possible, mais elle serait moins idiomatique en Haskell. De plus, Haskell optimise souvent la récursion (via l'**optimisation des appels terminaux**, bien que dans ce cas, elle ne s'applique pas directement car l'opération `*` est effectuée après l'appel récursif).

### Résumé
- **Fonction `factorielle`** : Calcule `n!` de manière récursive avec un cas de base (`0! = 1`) et un cas récursif (`n! = n * (n-1)!`).
- **Type** : Utilise `Integer` pour gérer des nombres arbitrairement grands.
- **Main** : Teste la fonction avec `n = 5` et affiche le résultat dans la console.
- **Points forts** : Code simple, clair, et fidèle à la définition mathématique. Limite : ne gère pas les entrées négatives.
- **Philosophie Haskell** : Le code illustre la puissance de la récursion et de la pureté fonctionnelle, avec une syntaxe concise et expressive.
