HC10T7 : Classe de type Cabriolet :
```haskell
-- Définition du type PaymentMethod
data PaymentMethod = Cash | CreditCard String | PayPal String deriving (Show)

-- Définition de la classe de type Convertible
class Convertible a b where
  convert :: a -> b

-- Instance de Convertible pour PaymentMethod vers String
instance Convertible PaymentMethod String where
  convert Cash = "Cash Payment"
  convert (CreditCard number) = "Credit Card ending in " ++ lastFour number
    where lastFour s = if length s >= 4 then drop (length s - 4) s else s
  convert (PayPal email) = "PayPal account: " ++ email

-- Fonction main pour tester
main :: IO ()
main = do
  let payment1 = Cash                  -- Paiement en espèces
  let payment2 = CreditCard "1234567890123456"  -- Carte de crédit
  let payment3 = PayPal "user@example.com"      -- PayPal
  
  -- Test de convert
  putStrLn $ convert payment1  -- Devrait afficher : Cash Payment
  putStrLn $ convert payment2  -- Devrait afficher : Credit Card ending in 3456
  putStrLn $ convert payment3  -- Devrait afficher : PayPal account: user@example.com
```

### Explications :
1. **Type `PaymentMethod`** :
   - Défini comme un type de données avec trois constructeurs :
     - `Cash` : représente un paiement en espèces.
     - `CreditCard String` : représente une carte de crédit avec un numéro (stocké comme une chaîne).
     - `PayPal String` : représente un compte PayPal avec une adresse e-mail.
   - Le `deriving (Show)` permet d'afficher les valeurs pour le débogage.

2. **Classe `Convertible`** :
   - Déclare une méthode `convert :: a -> b`, qui prend une valeur de type `a` et la convertit en une valeur de type `b`.
   - Cette classe est polymorphe, mais en pratique, chaque instance doit spécifier une conversion concrète entre deux types spécifiques (par exemple, `PaymentMethod` vers `String`).

3. **Instance `Convertible PaymentMethod String`** :
   - Implémente `convert` pour convertir un `PaymentMethod` en une chaîne descriptive :
     - Pour `Cash`, retourne `"Cash Payment"`.
     - Pour `CreditCard number`, retourne `"Credit Card ending in "` suivi des quatre derniers chiffres du numéro (utilisant une fonction `lastFour` qui extrait les 4 derniers caractères si la chaîne est assez longue).
     - Pour `PayPal email`, retourne `"PayPal account: "` suivi de l'adresse e-mail.
   - La contrainte n'est pas nécessaire ici, car la conversion est unidirectionnelle et spécifique.

4. **Fonction `main`** :
   - Crée trois instances de `PaymentMethod` : `payment1` (Cash), `payment2` (CreditCard avec un numéro), et `payment3` (PayPal avec un e-mail).
   - Teste `convert` sur ces trois cas et affiche les résultats avec `putStrLn`.
   - Les sorties sont formatées selon la logique définie dans l'instance `Convertible`.
