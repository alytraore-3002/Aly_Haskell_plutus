HC10T1 : Classe de type ShowSimple

```haskell
-- Définition du type PaymentMethod
data PaymentMethod = Cash | CreditCard String | PayPal String deriving (Show)

-- Définition de la classe de type ShowSimple
class ShowSimple a where
  showSimple :: a -> String

-- Instance de ShowSimple pour PaymentMethod
instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash"
  showSimple (CreditCard number) = "Credit Card ending in " ++ lastFour number
    where lastFour s = if length s >= 4 then drop (length s - 4) s else s
  showSimple (PayPal email) = "PayPal account: " ++ email

-- Fonction main pour tester
main :: IO ()
main = do
  let cash = Cash
  let card = CreditCard "1234567890123456"
  let paypal = PayPal "user@example.com"
  
  -- Test de showSimple pour chaque PaymentMethod
  putStrLn $ showSimple cash      -- Devrait afficher : Cash
  putStrLn $ showSimple card      -- Devrait afficher : Credit Card ending in 3456
  putStrLn $ showSimple paypal    -- Devrait afficher : PayPal account: user@example.com
```

### Explications :
1. **Type `PaymentMethod`** :
   - Défini comme un type de données avec trois constructeurs :
     - `Cash` : représente un paiement en espèces.
     - `CreditCard String` : représente une carte de crédit avec un numéro (stocké comme une chaîne).
     - `PayPal String` : représente un compte PayPal avec une adresse e-mail.
   - Le `deriving (Show)` est ajouté pour faciliter le débogage, bien que non strictement nécessaire ici.
2. **Classe de type `ShowSimple`** :
   - Déclare une méthode `showSimple :: a -> String` que chaque instance doit implémenter pour fournir une représentation simplifiée sous forme de chaîne.
3. **Instance de `ShowSimple` pour `PaymentMethod`** :
   - Pour `Cash`, retourne simplement `"Cash"`.
   - Pour `CreditCard number`, affiche `"Credit Card ending in "` suivi des quatre derniers chiffres du numéro (ou la chaîne entière si elle est plus courte que 4 caractères).
   - Pour `PayPal email`, affiche `"PayPal account: "` suivi de l'adresse e-mail.
4. **Fonction `main`** :
   - Crée trois exemples de `PaymentMethod` : un paiement en espèces, une carte de crédit avec un numéro, et un compte PayPal avec un e-mail.
   - Utilise `putStrLn` pour afficher le résultat de `showSimple` pour chaque exemple.
   - 
