HC1T7 - Tâche 7 : Conversion Fahrenheit/Celsius

```haskell
-- Convertit les degrés Fahrenheit en Celsius
fToC :: Double -> Double
fToC f = (f - 32) * 5 / 9

-- Fonction principale pour tester
main :: IO ()
main = do
    let tempF = 68.0
    putStrLn $ "Température en Fahrenheit: " ++ show tempF
    putStrLn $ "Température en Celsius: " ++ show (fToC tempF)
```

### Explications :

1. **Fonction `fToC`** :
   - **Signature** : `fToC :: Double -> Double` indique que la fonction prend un nombre à virgule flottante (`Double`) représentant la température en Fahrenheit et retourne la température en Celsius.
   - **Formule** : La conversion de Fahrenheit en Celsius utilise la formule `(F - 32) * 5/9`, où `F` est la température en Fahrenheit.
   - **Pureté** : La fonction est pure, car elle dépend uniquement de son argument `f`, produit toujours le même résultat pour la même entrée, et n'a pas d'effets de bord.

2. **Exemple d'exécution** :
   - Pour `fToC 68.0` :
     - `(68.0 - 32) * 5 / 9` = `36 * 5 / 9` ≈ `20.0`.

Sortie dans la console :
```
Température en Fahrenheit: 68.0
Température en Celsius: 20.0
```

### Remarques :
- **Pureté** : `fToC` est pure et ne dépend d'aucun état externe.
- **Simplicité** : La fonction est concise, implémentant directement la formule de conversion.
- **Type `Double`** : Utilise `Double` pour gérer les températures avec précision, car les conversions impliquent des fractions.
