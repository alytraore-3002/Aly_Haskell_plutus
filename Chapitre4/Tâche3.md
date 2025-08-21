HC4T3 - Tâche 3 : définir une fonction gradeComment

### Code Haskell
```haskell
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent !"
    | grade >= 70 && grade <= 89  = "Bon travail !"
    | grade >= 50 && grade <= 69  = "Tu as réussi."
    | grade >= 0  && grade <= 49  = "Peut mieux faire."
    | otherwise                   = "Note invalide."

-- Tests
main :: IO ()
main = do
    putStrLn $ "gradeComment 95: " ++ gradeComment 95    -- Affiche "gradeComment 95: Excellent !"
    putStrLn $ "gradeComment 75: " ++ gradeComment 75    -- Affiche "gradeComment 75: Bon travail !"
    putStrLn $ "gradeComment 60: " ++ gradeComment 60    -- Affiche "gradeComment 60: Tu as réussi."
    putStrLn $ "gradeComment 45: " ++ gradeComment 45    -- Affiche "gradeComment 45: Peut mieux faire."
    putStrLn $ "gradeComment 101: " ++ gradeComment 101  -- Affiche "gradeComment 101: Note invalide."
    putStrLn $ "gradeComment (-1): " ++ gradeComment (-1) -- Affiche "gradeComment (-1): Note invalide."
```

### Explications détaillées

1. **Définition de la fonction** :
   - La fonction `gradeComment` a la signature de type `Int -> String`. Elle prend un entier représentant une note et retourne une chaîne contenant un commentaire basé sur la tranche dans laquelle la note se situe.
   - La signature indique que l'entrée est un entier et la sortie une chaîne, ce qui correspond aux besoins de la tâche.

2. **Logique des gardes** :
   - Les gardes (`|`) sont utilisées pour évaluer la note selon les tranches spécifiées :
     - `| grade >= 90 && grade <= 100` : Si la note est entre 90 et 100 inclus, retourne `"Excellent !"`.
     - `| grade >= 70 && grade <= 89` : Si la note est entre 70 et 89 inclus, retourne `"Bon travail !"`.
     - `| grade >= 50 && grade <= 69` : Si la note est entre 50 et 69 inclus, retourne `"Tu as réussi."`.
     - `| grade >= 0 && grade <= 49` : Si la note est entre 0 et 49 inclus, retourne `"Peut mieux faire."`.
     - `| otherwise` : Pour toute autre valeur (négative ou supérieure à 100), retourne `"Note invalide."`.
   - Les gardes sont évaluées dans l'ordre, et la première condition satisfaite détermine le résultat retourné. L'utilisation de `otherwise` garantit que toutes les entrées possibles sont couvertes.

3. **Robustesse** :
   - La garde `otherwise` capture les cas où la note est hors de la plage valide (par exemple, négative comme `-1` ou supérieure à 100 comme `101`). Cela rend la fonction robuste face à des entrées inattendues.
   - Sans `otherwise`, une note non couverte par les tranches provoquerait une erreur de non-exhaustivité en Haskell, ce qui est évité ici.

4. **Tests** :
   - La fonction `main` est de type `IO ()`, utilisée pour effectuer des actions d'entrée/sortie, ici l'affichage des résultats dans la console.
   - Les tests couvrent les cas suivants :
     - `gradeComment 95` : Teste la tranche 90-100.
     - `gradeComment 75` : Teste la tranche 70-89.
     - `gradeComment 60` : Teste la tranche 50-69.
     - `gradeComment 45` : Teste la tranche 0-49.
     - `gradeComment 101` : Teste une note invalide (supérieure à 100).
     - `gradeComment (-1)` : Teste une note invalide (négative).
   - L'utilisation de `putStrLn` avec la concaténation (`++`) permet un affichage clair des résultats. Comme `gradeComment` retourne une `String`, la fonction `show` n'est pas nécessaire.

5. **Avantages des gardes** :
   - Les gardes offrent une syntaxe claire et lisible pour exprimer des conditions multiples basées sur des intervalles.
   - Elles permettent de structurer la logique de manière séquentielle, chaque garde correspondant à une tranche spécifique, ce qui rend le code facile à comprendre et à maintenir.
   - Comparé à une série d'instructions `if-then-else`, les gardes sont plus concises et idiomatiques en Haskell pour ce type de classification.

6. **Alternatives possibles** :
   - Une alternative serait d'utiliser le pattern matching avec une expression `case`, mais les gardes sont plus adaptées ici car elles permettent de tester des conditions sur des intervalles numériques.
   - Une autre approche pourrait consister à utiliser des tuples ou une structure de données pour associer les tranches aux commentaires, mais cela compliquerait inutilement le code pour ce problème simple.
   - Exemple avec `case` (moins idiomatique ici) :
     ```haskell
     gradeComment grade = case () of
         _ | grade >= 90 && grade <= 100 -> "Excellent !"
         _ | grade >= 70 && grade <= 89  -> "Bon travail !"
         _ | grade >= 50 && grade <= 69  -> "Tu as réussi."
         _ | grade >= 0  && grade <= 49  -> "Peut mieux faire."
         _                               -> "Note invalide."
     ```

7. **Limitations** :
   - La fonction ne gère pas les cas où l'entrée serait d'un type incorrect (par exemple, un flottant ou une chaîne), mais comme la signature spécifie `Int`, Haskell garantit que l'entrée est un entier.
   - Elle est conçue pour des notes entières. Si des notes décimales (par exemple, 89.5) étaient nécessaires, il faudrait changer la signature en `Float -> String` et ajuster les comparaisons.

### Résumé
la fonction `gradeComment` utilise des gardes pour classifier une note entière dans des tranches prédéfinies (9La0-100, 70-89, 50-69, 0-49) et retourne un commentaire approprié pour chaque tranche, ou `"Note invalide."` pour les valeurs hors plage. Les gardes offrent une structure claire et robuste, garantissant que toutes les entrées possibles sont gérées. La fonction est simple, efficace, et respecte strictement les exigences spécifiées.
