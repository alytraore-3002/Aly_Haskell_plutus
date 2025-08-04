HC2T6 - Tâche 6 : Comprendre Int vs Integer

### Définition des variables

1. **Variable `smallNumber`**
   - **Type** : `Int`
   - **Valeur** : \( 2^{62} \)
   - **Définition** :
     ```haskell
     smallNumber :: Int
     smallNumber = 2 ^ 62
     ```
     - Explication : La valeur \( 2^{62} \) est \( 4611686018427387904 \), qui est dans les limites d'un `Int` sur la plupart des systèmes 64 bits (où `Int` est généralement borné entre \(-2^{63}\) et \(2^{63}-1\), soit environ \(-9.22 \times 10^{18}\) à \(9.22 \times 10^{18}\)). Cette définition est donc valide.

2. **Variable `bigNumber`**
   - **Type** : `Integer`
   - **Valeur** : \( 2^{127} \)
   - **Définition** :
     ```haskell
     bigNumber :: Integer
     bigNumber = 2 ^ 127
     ```
     - Explication : La valeur \( 2^{127} \) est \( 170141183460469231731687303715884105728 \), un très grand nombre. Le type `Integer` en Haskell n'a pas de limite de taille (hormis la mémoire disponible), donc cette valeur est parfaitement gérée.

### Code complet

Voici le code Haskell avec les définitions des variables et une fonction `main` pour afficher leurs valeurs :

```haskell
smallNumber :: Int
smallNumber = 2 ^ 62

bigNumber :: Integer
bigNumber = 2 ^ 127

main :: IO ()
main = do
    putStrLn "Valeur de smallNumber (2^62 :: Int) :"
    print smallNumber
    putStrLn "Valeur de bigNumber (2^127 :: Integer) :"
    print bigNumber
```

### Évaluation de `2^64 :: Int` dans GHCi

Maintenant, analysons ce qui se passe si nous évaluons `2^64 :: Int` dans GHCi.

- **Expression** : `2^64 :: Int`
- **Calcul** : La valeur de \( 2^{64} \) est \( 18446744073709551616 \).
- **Limites de `Int`** : Sur un système 64 bits, un `Int` est généralement codé sur 64 bits, avec une plage de \(-2^{63}\) à \(2^{63}-1\), soit de \(-9223372036854775808\) à \(9223372036854775807\). La valeur \( 2^{64} = 18446744073709551616 \) dépasse la limite maximale de \( 2^{63}-1 \approx 9.22 \times 10^{18} \).

- **Résultat dans GHCi** :
  Si vous tapez `:set -XNegativeLiterals` (pour permettre une évaluation correcte) et évaluez `2^64 :: Int` dans GHCi, vous obtiendrez un **débordement arithmétique** (overflow). En Haskell, un `Int` qui dépasse sa limite maximale est sujet à un comportement de débordement, où la valeur "recommence" à partir de la borne inférieure ou produit un résultat incorrect en raison de la représentation binaire.

  En pratique, pour \( 2^{64} \), le calcul donne :
  \[
  2^{64} = 18446744073709551616
  \]
  Cette valeur est exactement \( 2 \times 2^{63} \), et comme \( 2^{63} = 9223372036854775808 \), elle dépasse la limite de \( 2^{63}-1 \). Dans GHCi, sur un système 64 bits, évaluer `2^64 :: Int` provoque un débordement, et le résultat dépend de l'implémentation, mais typiquement, Haskell ne déclenche pas d'erreur d'exécution pour un débordement d'`Int`. À la place, la valeur est "enroulée" (wrap-around) en raison de l'arithmétique modulaire sur 64 bits.

  Si vous entrez dans GHCi :
  ```haskell
  2^64 :: Int
  ```
  Le résultat sera incorrect en raison du débordement. Par exemple, sur un système 64 bits, cela pourrait donner une valeur négative ou imprévisible, comme :
  ```haskell
  0
  ```
  ou une autre valeur incorrecte, car \( 2^{64} \) modulo \( 2^{64} \) (en tenant compte de la représentation binaire) revient à 0 dans certains cas, ou la tentative de forcer un `Int` peut produire un comportement indéfini selon l'implémentation.

- **Comparaison avec `Integer`** :
  Si vous évaluez `2^64 :: Integer`, il n'y a pas de débordement, car `Integer` n'a pas de limite. Vous obtiendriez :
  ```haskell
  18446744073709551616
  ```

### Résultat attendu du programme

```
Valeur de smallNumber (2^62 :: Int) :
4611686018427387904
Valeur de bigNumber (2^127 :: Integer) :
170141183460469231731687303715884105728
```

### Remarques sur `2^64 :: Int`

- **Comportement de débordement** : L'évaluation de `2^64 :: Int` dans GHCi donne un résultat incorrect (souvent `0` ou une valeur négative) à cause du débordement. Pour éviter cela, il est préférable d'utiliser `Integer` pour des calculs avec de très grands nombres.
- **Test dans GHCi** : Si vous voulez tester `2^64 :: Int` vous-même, chargez GHCi et tapez :
  ```haskell
  2^64 :: Int
  ```
  Vous observerez probablement un résultat comme `0` ou une autre valeur erronée, confirmant le débordement.

### Code pour tester `2^64 :: Int`

Pour illustrer cela dans le programme, ajoutons un test explicite pour `2^64 :: Int` :

```haskell
smallNumber :: Int
smallNumber = 2 ^ 62

bigNumber :: Integer
bigNumber = 2 ^ 127

main :: IO ()
main = do
    putStrLn "Valeur de smallNumber (2^62 :: Int) :"
    print smallNumber
    putStrLn "Valeur de bigNumber (2^127 :: Integer) :"
    print bigNumber
    putStrLn "Test de 2^64 :: Int :"
    print ((2 ^ 64) :: Int)  -- Débordement attendu
    putStrLn "Test de 2^64 :: Integer (pour comparaison) :"
    print ((2 ^ 64) :: Integer)
```

Sortie attendue :
```
Valeur de smallNumber (2^62 :: Int) :
4611686018427387904
Valeur de bigNumber (2^127 :: Integer) :
170141183460469231731687303715884105728
Test de 2^64 :: Int :
0  -- Débordement, résultat incorrect
Test de 2^64 :: Integer (pour comparaison) :
18446744073709551616
```

### Résumé

- `smallNumber :: Int = 2^62` est valide (\( 4611686018427387904 \)).
- `bigNumber :: Integer = 2^127` est valide (\( 170141183460469231731687303715884105728 \)).
- Évaluer `2^64 :: Int` dans GHCi provoque un débordement, donnant un résultat incorrect (souvent `0` ou une valeur négative), car \( 2^{64} \) dépasse la limite de `Int` (\( 2^{63}-1 \)).
- Utiliser `Integer` au lieu de `Int` évite ce problème pour les grands nombres.
