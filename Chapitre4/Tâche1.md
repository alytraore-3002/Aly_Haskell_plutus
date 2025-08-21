HC4T1 - Tâche 1 : définir une fonction WeatherReport

### Code Haskell
```haskell
weatherReport :: String -> String
weatherReport condition = case condition of
    "sunny"  -> "Il fait beau et ensoleillé !"
    "rainy"  -> "N'oublie pas ton parapluie !"
    "cloudy" -> "Un peu gris, mais pas de pluie pour l'instant !"
    _        -> "Météo inconnue"

-- Tests
main :: IO ()
main = do
    putStrLn $ "weatherReport \"sunny\": " ++ weatherReport "sunny"    -- Affiche "weatherReport \"sunny\": Il fait beau et ensoleillé !"
    putStrLn $ "weatherReport \"rainy\": " ++ weatherReport "rainy"    -- Affiche "weatherReport \"rainy\": N'oublie pas ton parapluie !"
    putStrLn $ "weatherReport \"cloudy\": " ++ weatherReport "cloudy"  -- Affiche "weatherReport \"cloudy\": Un peu gris, mais pas de pluie pour l'instant !"
    putStrLn $ "weatherReport \"snowy\": " ++ weatherReport "snowy"    -- Affiche "weatherReport \"snowy\": Météo inconnue"
```

### Explications
- **Définition de la fonction** : La fonction `weatherReport` a la signature de type `String -> String`, prenant une chaîne représentant une condition météo et retournant un message descriptif.
- **Logique du pattern matching** :
  - L'expression `case condition of` effectue le pattern matching sur la valeur de `condition` :
    - `"sunny"` : Retourne `"Il fait beau et ensoleillé !"`.
    - `"rainy"` : Retourne `"N'oublie pas ton parapluie !"`.
    - `"cloudy"` : Retourne `"Un peu gris, mais pas de pluie pour l'instant !"`.
    - `_` : Le caractère générique capture toute autre entrée et retourne `"Météo inconnue"`.
- **Remarque** : La fonction est sensible à la casse (par exemple, `"Sunny"` retournera `"Météo inconnue"`). Une conversion en minuscules pourrait rendre la fonction insensible à la casse, mais ce n'est pas requis ici.

### Résumé
La fonction `weatherReport` utilise le pattern matching pour associer des conditions météo spécifiques (`"sunny"`, `"rainy"`, `"cloudy"`) à des messages descriptifs prédéfinis. Toute autre entrée est capturée par le motif générique `_`, retournant `"Météo inconnue"`. La fonction est concise, lisible, et respecte strictement les cas spécifiés.
