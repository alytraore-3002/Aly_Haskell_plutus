HC4T2 - Tâche 2 : définir une fonction dayType

### Code Haskell
```haskell
dayType :: String -> String
dayType day = case day of
    "Saturday" -> "C'est le week-end !"
    "Sunday"   -> "C'est le week-end !"
    "Monday"   -> "C'est un jour de semaine."
    "Tuesday"  -> "C'est un jour de semaine."
    "Wednesday" -> "C'est un jour de semaine."
    "Thursday" -> "C'est un jour de semaine."
    "Friday"   -> "C'est un jour de semaine."
    _          -> "Jour invalide"

-- Tests
main :: IO ()
main = do
    putStrLn $ "dayType \"Saturday\": " ++ dayType "Saturday"    -- Affiche "dayType \"Saturday\": C'est le week-end !"
    putStrLn $ "dayType \"Monday\": " ++ dayType "Monday"        -- Affiche "dayType \"Monday\": C'est un jour de semaine."
    putStrLn $ "dayType \"Friday\": " ++ dayType "Friday"        -- Affiche "dayType \"Friday\": C'est un jour de semaine."
    putStrLn $ "dayType \"Invalid\": " ++ dayType "Invalid"      -- Affiche "dayType \"Invalid\": Jour invalide"
```

### Explications détaillées

1. **Définition de la fonction** :
   - La fonction `dayType` a la signature de type `String -> String`. Elle prend en entrée une chaîne de caractères représentant un jour de la semaine (par exemple, `"Monday"`, `"Saturday"`) et retourne une chaîne décrivant si c'est un jour de semaine, un jour de week-end, ou un jour invalide.
   - La signature indique que l'entrée et la sortie sont toutes deux des chaînes, ce qui correspond à la spécification de la tâche.

2. **Logique du pattern matching** :
   - L'expression `case day of` est utilisée pour effectuer un pattern matching sur la valeur de l'argument `day`. Le pattern matching est une fonctionnalité puissante de Haskell qui permet de comparer une valeur contre une série de motifs (patterns) et d'exécuter le code correspondant au motif qui correspond.
   - Les motifs sont définis comme suit :
     - `"Saturday"` : Retourne `"C'est le week-end !"`, car samedi est considéré comme un jour de week-end selon la spécification.
     - `"Sunday"` : Retourne également `"C'est le week-end !"`, car dimanche est aussi un jour de week-end.
     - `"Monday"`, `"Tuesday"`, `"Wednesday"`, `"Thursday"`, `"Friday"` : Chacun retourne `"C'est un jour de semaine."`, car ce sont les jours de la semaine de travail.
     - `_` : Le caractère générique (wildcard) capture toute entrée qui ne correspond à aucun des jours de la semaine listés (par exemple, `"Invalid"`, `"sunday"`, ou une chaîne vide). Dans ce cas, la fonction retourne `"Jour invalide"`.
   - Le pattern matching est évalué dans l'ordre des motifs : Haskell vérifie chaque motif de haut en bas et exécute la première correspondance trouvée.

3. **Sensibilité à la casse** :
   - La fonction est sensible à la casse, ce qui signifie que `"saturday"` ou `"SUNDAY"` ne correspondront pas aux motifs `"Saturday"` ou `"Sunday"`. Par exemple, `dayType "saturday"` retournera `"Jour invalide"`, car il ne correspond à aucun motif explicite.
   - Cette sensibilité est une caractéristique standard des comparaisons de chaînes en Haskell. Pour rendre la fonction insensible à la casse, on pourrait convertir l'entrée en minuscules (par exemple, en utilisant `Data.Char.toLower` avec `map`), mais cela n'est pas requis par la spécification.

4. **Robustesse et gestion des cas invalides** :
   - L'utilisation du motif générique `_` garantit que la fonction gère toutes les entrées possibles, même celles qui ne sont pas des jours de la semaine valides. Cela rend la fonction robuste face à des entrées inattendues comme `"Invalid"`, `"123"`, ou une chaîne vide.
   - Sans le motif `_`, la fonction provoquerait une erreur d'exécution (exception de non-exhaustivité) pour les entrées non couvertes par les motifs.

5. **Tests** :
   - La fonction `main` est de type `IO ()`, ce qui signifie qu'elle effectue des actions d'entrée/sortie (ici, afficher du texte dans la console) et ne retourne aucune valeur significative.
   - Les tests vérifient quatre cas :
     - `dayType "Saturday"` : Vérifie que le samedi est correctement identifié comme un jour de week-end.
     - `dayType "Monday"` : Vérifie qu'un jour de semaine (lundi) est correctement identifié.
     - `dayType "Friday"` : Vérifie un autre jour de semaine (vendredi) pour confirmer que tous les jours de semaine sont bien traités.
     - `dayType "Invalid"` : Vérifie que les entrées non valides sont correctement détectées et renvoient `"Jour invalide"`.
   - L'utilisation de `putStrLn` avec la concaténation (`++`) permet d'afficher les résultats de manière lisible. La fonction `show` n'est pas nécessaire ici, car `dayType` retourne déjà une `String`.

6. **Avantages du pattern matching** :
   - Le pattern matching rend le code clair et expressif, car chaque cas est explicitement défini avec son résultat correspondant.
   - Comparé à une série de conditions `if-then-else` ou de gardes, le pattern matching est souvent plus lisible pour ce type de problème, où les entrées sont des valeurs discrètes (jours de la semaine).
   - Il permet également une maintenance facile : ajouter un nouveau cas (par exemple, un jour spécial) nécessiterait simplement d'ajouter un nouveau motif dans l'expression `case`.

7. **Alternatives possibles** :
   - Une alternative serait d'utiliser des gardes (`|`) au lieu du `case`. Par exemple :
     ```haskell
     dayType day
         | day == "Saturday" || day == "Sunday" = "C'est le week-end !"
         | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "C'est un jour de semaine."
         | otherwise = "Jour invalide"
     ```
     Cependant, le pattern matching est plus idiomatique et direct dans ce cas, car il évite les comparaisons explicites avec `==` pour chaque jour.
   - Une autre approche pourrait consister à stocker les jours de semaine et de week-end dans des listes et à utiliser `elem` pour vérifier l'appartenance, mais cela serait moins explicite et potentiellement moins performant pour un petit ensemble de cas fixes.

8. **Limitations** :
   - La fonction ne valide pas si l'entrée est une chaîne vide ou contient des espaces (par exemple, `" Monday "`). Cela retournera `"Jour invalide"`, ce qui est cohérent avec la spécification.
   - Elle ne gère pas les variations orthographiques ou linguistiques (par exemple, `"samedi"` en français ou `"sunday"` en minuscules). Une version plus robuste pourrait normaliser l'entrée (par exemple, en convertissant en minuscules ou en supprimant les espaces).

### Résumé
La fonction `dayType` utilise le pattern matching pour associer des jours de la semaine spécifiques à des messages prédéfinis : `"Saturday"` et `"Sunday"` sont classés comme week-end, `"Monday"` à `"Friday"` comme jours de semaine, et toute autre entrée comme invalide. Le pattern matching garantit une correspondance claire et efficace, avec une gestion robuste des cas invalides via le motif générique `_`. La fonction est concise, lisible, et respecte strictement les exigences spécifiées, tout en étant sensible à la casse.
