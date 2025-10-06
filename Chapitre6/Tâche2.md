HC6T2 : Suite de Fibonacci (récursif)

### Code Haskell

```haskell
-- Définition de la fonction Fibonacci récursive
fibonacci :: Integer -> Integer
fibonacci 0 = 0  -- Cas de base : F(0) = 0
fibonacci 1 = 1  -- Cas de base : F(1) = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  -- Cas récursif : F(n) = F(n-1) + F(n-2)

-- Fonction main pour tester la fonction Fibonacci
main :: IO ()
main = do
    let n = 7  -- Nombre à tester (F(7) = 13)
    putStrLn $ "Le " ++ show n ++ "ème nombre de Fibonacci est : " ++ show (fibonacci n)
```

### Explications détaillées

#### 1. **Signature de la fonction `fibonacci`**
```haskell
fibonacci :: Integer -> Integer
```
- **Type** : La fonction prend un `Integer` (nombre entier potentiellement grand) en entrée et retourne un `Integer` en sortie.
- **Pourquoi `Integer` ?** Comme pour la factorielle, les nombres de Fibonacci croissent rapidement (exponentiellement), et `Integer` permet de gérer des valeurs arbitrairement grandes sans débordement, contrairement à `Int` qui est limité.
- La fonction est pure : elle est déterministe et sans effets de seconde (pas d'entrées/sorties ou d'état mutable), ce qui est idéal en Haskell pour la récursion.

#### 2. **Cas de base**
```haskell
fibonacci 0 = 0
fibonacci 1 = 1
```
- La suite de Fibonacci est définie par **F(0) = 0** et **F(1) = 1**. Ces deux cas de base arrêtent la récursion et évitent une boucle infinie.
- Sans eux, pour `n = 0` ou `n = 1`, la fonction appellerait récursivement des valeurs plus petites indéfiniment (par exemple, `fibonacci (-1)` ou pire), menant à une erreur de stack overflow.
- Note : Il existe des variantes où F(0) = 0 et F(1) = 1, ou parfois F(1) = 1 et F(2) = 1 ; ici, on utilise la définition standard mathématique.

#### 3. **Cas récursif**
```haskell
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```
- Pour tout `n > 1`, le nième nombre de Fibonacci est la somme des deux précédents : **F(n) = F(n-1) + F(n-2)**.
- La fonction s'appelle elle-même deux fois : une pour `n-1` et une pour `n-2`, créant un arbre de récursion binaire.
- Exemple pour `n = 4` (trace de l'exécution) :
  - `fibonacci 4 = fibonacci 3 + fibonacci 2`
  - `fibonacci 3 = fibonacci 2 + fibonacci 1 = (fibonacci 1 + fibonacci 0) + 1 = (1 + 0) + 1 = 2`
  - `fibonacci 2 = fibonacci 1 + fibonacci 0 = 1 + 0 = 1`
  - Donc : `fibonacci 4 = 2 + 1 = 3`
- La suite complète pour les premiers termes : F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13, etc.

#### 4. **Gestion des cas non définis**
- Comme pour la factorielle, le code ne gère pas explicitement les nombres négatifs. Si `n < 0`, la récursion ne s'arrêtera pas (elle descendra indéfiniment). On pourrait ajouter une garde comme `otherwise = error "n doit être non négatif"`, mais pour cet exemple simple, on assume `n ≥ 0`.
- **Efficacité** : Cette implémentation est **exponentielle en temps** (O(2^n) appels récursifs), car chaque appel génère deux sous-appels. Elle est fine pour de petits `n` (jusqu'à ~40), mais lente pour de grands (ex. n=50). En Haskell, des optimisations comme la **récursion avec mémoïsation** (via `memoize` ou une table) ou une version itérative seraient plus efficaces pour des usages réels.

#### 5. **Fonction `main`**
```haskell
main :: IO ()
main = do
    let n = 7
    putStrLn $ "Le " ++ show n ++ "ème nombre de Fibonacci est : " ++ show (fibonacci n)
```
- **Rôle** : Point d'entrée du programme pour tester et afficher le résultat.
- **Type `IO ()`** : Indique des effets d'entrée/sortie (affichage) sans valeur de retour significative.
- **Explication** : `let n = 7` fixe le test à 7 (F(7)=13). `putStrLn` affiche le message concaténé via `++` et `show` (qui convertit les entiers en chaînes). Résultat affiché : `"Le 7ème nombre de Fibonacci est : 13"`.

#### 6. **Pourquoi récursif ?**
- La définition mathématique de Fibonacci est intrinsèquement récursive (`F(n) = F(n-1) + F(n-2)`), ce qui se traduit naturellement en Haskell, un langage fonctionnel qui encourage la récursion plutôt que les boucles impératives.
- Cela illustre la **récursion arborescente** (arbre de appels), un concept clé en programmation fonctionnelle. Cependant, pour des performances, une version tail-récursive (avec accumulation) ou itérative est préférable.

### Résumé
- **Fonction `fibonacci`** : Calcule F(n) récursivement avec deux cas de base (F(0)=0, F(1)=1) et un cas récursif (somme des deux précédents).
- **Type** : `Integer` pour supporter la croissance exponentielle.
- **Main** : Teste avec `n=7` et affiche le résultat (13).
- **Points forts** : Code concis et fidèle à la maths. Limites : Inefficace pour grands n ; pas de gestion des négatifs.
- **Philosophie Haskell** : Met en avant la récursion pure et expressive, mais rappelle l'importance d'optimisations pour l'usage pratique.
