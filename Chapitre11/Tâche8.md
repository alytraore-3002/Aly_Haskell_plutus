HC11T8 : Dérivation de Eq et Ord pour PaymentMethod

```haskell
module Main where

-- Définition du type PaymentMethod
data PaymentMethod = Cash Double | CreditCard String | Crypto String deriving (Show)

-- Dérivation des instances Eq et Ord pour PaymentMethod
instance Eq PaymentMethod where
  (Cash a) == (Cash b) = a == b
  (CreditCard a) == (CreditCard b) = a == b
  (Crypto a) == (Crypto b) = a == b
  _ == _ = False

instance Ord PaymentMethod where
  compare (Cash a) (Cash b) = compare a b
  compare (CreditCard a) (CreditCard b) = compare a b
  compare (Crypto a) (Crypto b) = compare a b
  compare (Cash _) (CreditCard _) = LT
  compare (Cash _) (Crypto _) = LT
  compare (CreditCard _) (Cash _) = GT
  compare (CreditCard _) (Crypto _) = LT
  compare (Crypto _) (Cash _) = GT
  compare (Crypto _) (CreditCard _) = GT

-- Fonction main pour tester
main :: IO ()
main = do
  let pm1 = Cash 50.0
      pm2 = Cash 100.0
      pm3 = CreditCard "1234-5678"
      pm4 = CreditCard "1234-5678"
      pm5 = Crypto "BTC"
      pm6 = Crypto "ETH"

  putStrLn "Test de == (égalité) :"
  print $ pm1 == pm2  -- False
  print $ pm3 == pm4  -- True
  print $ pm5 == pm6  -- False
  print $ pm1 == pm3  -- False

  putStrLn "\nTest de compare (ordre) :"
  print $ compare pm1 pm2  -- LT (50.0 < 100.0)
  print $ compare pm3 pm4  -- EQ ("1234-5678" == "1234-5678")
  print $ compare pm1 pm3  -- LT (Cash < CreditCard)
  print $ compare pm3 pm5  -- LT (CreditCard < Crypto)
  print $ compare pm5 pm1  -- GT (Crypto > Cash)
```

### Explications détaillées

#### 1. Définition du type `PaymentMethod`
```haskell
data PaymentMethod = Cash Double | CreditCard String | Crypto String deriving (Show)
```
- **Description** : `PaymentMethod` est un type algébrique avec trois constructeurs :
  - `Cash Double` : Représente un paiement en espèces avec un montant (e.g., `Cash 50.0`).
  - `CreditCard String` : Représente un paiement par carte avec un numéro (e.g., `CreditCard "1234-5678"`).
  - `Crypto String` : Représente un paiement en cryptomonnaie avec un nom ou identifiant (e.g., `Crypto "BTC"`).
- **Dérivation** :
  - `Show` : Permet d'afficher les valeurs (e.g., `Cash 50.0`, `CreditCard "1234-5678"`).
- **Rôle** : Modélise différentes méthodes de paiement pour tester les instances `Eq` et `Ord`.

#### 2. Instance de `Eq` pour `PaymentMethod`
```haskell
instance Eq PaymentMethod where
  (Cash a) == (Cash b) = a == b
  (CreditCard a) == (CreditCard b) = a == b
  (Crypto a) == (Crypto b) = a == b
  _ == _ = False
```
- **Description** : Définit l'égalité entre deux `PaymentMethod`.
- **Fonctionnement** :
  - Compare les valeurs contenues uniquement si les constructeurs sont identiques :
    - `Cash a` et `Cash b` sont égaux si `a == b` (comparaison de `Double`).
    - `CreditCard a` et `CreditCard b` sont égaux si `a == b` (comparaison de `String`).
    - `Crypto a` et `Crypto b` sont égaux si `a == b` (comparaison de `String`).
  - Si les constructeurs diffèrent (e.g., `Cash` vs `CreditCard`), retourne `False` (cas par défaut `_ == _`).
- **Exemples** :
  - `Cash 50.0 == Cash 50.0` → `True`.
  - `CreditCard "1234-5678" == CreditCard "1234-5678"` → `True`.
  - `Cash 50.0 == CreditCard "1234-5678"` → `False`.

#### 3. Instance de `Ord` pour `PaymentMethod`
```haskell
instance Ord PaymentMethod where
  compare (Cash a) (Cash b) = compare a b
  compare (CreditCard a) (CreditCard b) = compare a b
  compare (Crypto a) (Crypto b) = compare a b
  compare (Cash _) (CreditCard _) = LT
  compare (Cash _) (Crypto _) = LT
  compare (CreditCard _) (Cash _) = GT
  compare (CreditCard _) (Crypto _) = LT
  compare (Crypto _) (Cash _) = GT
  compare (Crypto _) (CreditCard _) = GT
```
- **Description** : Définit un ordre total entre `PaymentMethod` en utilisant la fonction `compare :: a -> a -> Ordering` (retourne `LT`, `EQ`, ou `GT`).
- **Fonctionnement** :
  - **Comparaisons intra-constructeur** :
    - `Cash a` vs `Cash b` : Compare les montants (`compare a b`).
    - `CreditCard a` vs `CreditCard b` : Compare les numéros de carte (`compare a b`).
    - `Crypto a` vs `Crypto b` : Compare les noms de cryptomonnaies (`compare a b`).
  - **Comparaisons inter-constructeurs** : Définit un ordre hiérarchique arbitraire :
    - `Cash` est inférieur à `CreditCard` et `Crypto` (`LT`).
    - `CreditCard` est inférieur à `Crypto` (`LT`).
    - `Crypto` est supérieur à `Cash` et `CreditCard` (`GT`).
    - Cette hiérarchie reflète une préférence implicite (e.g., cryptomonnaies comme plus "modernes").
- **Exemples** :
  - `compare (Cash 50.0) (Cash 100.0)` → `LT` (car `50.0 < 100.0`).
  - `compare (CreditCard "1234") (CreditCard "1234")` → `EQ`.
  - `compare (Cash 50.0) (CreditCard "1234")` → `LT`.
  - `compare (CreditCard "1234") (Crypto "BTC")` → `LT`.
  - `compare (Crypto "BTC") (Cash 50.0)` → `GT`.

### Résumé
- **Type `PaymentMethod`** : `data PaymentMethod = Cash Double | CreditCard String | Crypto String`, représente différentes méthodes de paiement.
- **Instance `Eq` pour `PaymentMethod`** : Définit l'égalité basée sur les valeurs contenues si les constructeurs sont identiques, sinon `False`.
- **Instance `Ord` pour `PaymentMethod`** : Implémente `compare` avec ordre intra-constructeur (par valeur) et inter-constructeur (`Cash < CreditCard < Crypto`).
- **Fonction `main`** : Teste `==` et `compare` sur des `PaymentMethod` variés, affichant les résultats via IO.
- **Code** : Complet, testable, et respecte les concepts du cours (pureté, typeclasses, IO).
