 HC8T1 : Synonymes de type et fonction de base:
```haskell
-- Définir les synonymes de type
type Address = String
type Value = Int

-- Fonction generateTx
generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr value = fromAddr ++ " -> " ++ toAddr ++ ": " ++ show value

-- Fonction main pour tester
main :: IO ()
main = do
  let from = "Addr1" :: Address
      to = "Addr2" :: Address
      val = 100 :: Value
  putStrLn $ generateTx from to val
```

### Explications :
1. **Synonymes de type** :
   - `type Address = String` crée un synonyme de type où `Address` représente un `String`. Cela améliore la lisibilité et la clarté du code.
   - `type Value = Int` crée un synonyme de type où `Value` représente un `Int`.

2. **Fonction `generateTx`** :
   - La fonction prend trois paramètres : `fromAddr` (type `Address`), `toAddr` (type `Address`), et `value` (type `Value`).
   - Elle concatène les deux adresses et la valeur en utilisant l'opérateur `++` pour joindre les chaînes. La valeur est convertie en chaîne avec `show` pour pouvoir être concaténée.
   - Le format de sortie est `fromAddr -> toAddr: value` (par exemple, "Addr1 -> Addr2: 100").

3. **Fonction `main`** :
   - Définit des valeurs de test : `from = "Addr1"`, `to = "Addr2"`, et `val = 100`.
   - Utilise `putStrLn` pour afficher le résultat de `generateTx from to val`.
