HC3T7 - Tâche avancée 7 : Déterminer la saison en fonction du mois avec des gardes

### Voici la définition de la fonction `season` en Haskell qui détermine la saison correspondant à un mois donné (représenté par un entier de 1 à 12) en utilisant des gardes, suivie des tests demandés.

### Code Haskell
```haskell
season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2  = "Hiver"
    | month >= 3  && month <= 5               = "Printemps"
    | month >= 6  && month <= 8               = "Été"
    | month >= 9  && month <= 11              = "Automne"
    | otherwise                               = "Mois invalide"

-- Tests
main :: IO ()
main = do
    putStrLn $ "season 3: " ++ season 3   -- Affiche "season 3: Printemps"
    putStrLn $ "season 7: " ++ season 7   -- Affiche "season 7: Été"
    putStrLn $ "season 11: " ++ season 11 -- Affiche "season 11: Automne"
```

### Explications
- **Définition de la fonction** : La fonction `season` a la signature de type `Int -> String`, prenant un entier représentant un mois (1 à 12) et retournant une chaîne indiquant la saison correspondante.
- **Logique des gardes** :
  - `| month == 12 || month == 1 || month == 2` : Si le mois est décembre (12), janvier (1) ou février (2), retourne `"Hiver"`.
  - `| month >= 3 && month <= 5` : Si le mois est mars (3), avril (4) ou mai (5), retourne `"Printemps"`.
  - `| month >= 6 && month <= 8` : Si le mois est juin (6), juillet (7) ou août (8), retourne `"Été"`.
  - `| month >= 9 && month <= 11` : Si le mois est septembre (9), octobre (10) ou novembre (11), retourne `"Automne"`.
  - `| otherwise` : Pour tout autre valeur (mois invalide, comme 0 ou 13), retourne `"Mois invalide"`.
- **Tests** : La fonction `main` teste `season` avec les mois 3, 7 et 11. Les sorties attendues sont :
  - `season 3: Printemps` (mars est au printemps).
  - `season 7: Été` (juillet est en été).
  - `season 11: Automne` (novembre est en automne).
- **Remarque** : La garde `otherwise` gère les entrées invalides, assurant que la fonction retourne un message clair si un mois hors de la plage 1-12 est fourni.

### Résultats des tests
- `season 3: Printemps`
- `season 7: Été`
- `season 11: Automne`
