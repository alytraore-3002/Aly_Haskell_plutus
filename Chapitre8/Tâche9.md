HC8T9 : Type enregistrement Transaction et fonction associée
```haskell
-- Définitions des synonymes de type pour plus de clarté
type Address = String
type Value = Float

-- Définition du type Transaction avec la syntaxe d'enregistrement
data Transaction = Transaction { from :: Address, to :: Address, amount :: Value, transactionId :: String }
  deriving (Show)

-- Fonction pour créer une transaction et retourner son ID
createTransaction :: Address -> Address -> Value -> String -> String
createTransaction fromAddr toAddr amt txId = 
  let _ = Transaction { from = fromAddr, to = toAddr, amount = amt, transactionId = txId }
  in txId

-- Fonction main pour tester createTransaction
main :: IO ()
main = do
  let txId1 = createTransaction "Alice" "Bob" 100.0 "TX001"
  let txId2 = createTransaction "Bob" "Charlie" 50.5 "TX002"
  putStrLn $ "ID de la première transaction : " ++ txId1
  putStrLn $ "ID de la deuxième transaction : " ++ txId2
```

**Explications :**
- Les synonymes de type `Address` (pour `String`) et `Value` (pour `Float`) sont définis pour améliorer la lisibilité.
- Le type `Transaction` est défini avec la syntaxe d'enregistrement, contenant les champs `from` (Address), `to` (Address), `amount` (Value), et `transactionId` (String).
- `deriving (Show)` permet d'afficher les instances de `Transaction` si nécessaire (non utilisé directement ici).
- La fonction `createTransaction` prend un `Address` (expéditeur), un `Address` (destinataire), un `Value` (montant), et un `String` (ID de transaction). Elle crée une `Transaction` et retourne son `transactionId`.
- La fonction `main` teste `createTransaction` avec deux exemples :
  - Une transaction de "Alice" à "Bob" de 100.0 avec l'ID "TX001".
  - Une transaction de "Bob" à "Charlie" de 50.5 avec l'ID "TX002".
- Les IDs des transactions sont affichés avec `putStrLn`.
- 
