HC3T8 - Tâche avancée 8 : Calculer l'IMC et retourner la catégorie avec où

### Voici la définition de la fonction `bmiCategory` en Haskell qui calcule l'Indice de Masse Corporelle (IMC) à partir du poids (en kg) et de la taille (en mètres) en utilisant une clause `where` pour calculer l'IMC, et des gardes pour classifier la catégorie correspondante, suivie des tests demandés.

### Code Haskell
```haskell
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5          = "Insuffisance pondérale"
    | bmi >= 18.5 && bmi <= 24.9 = "Normal"
    | bmi >= 25 && bmi <= 29.9   = "Surpoids"
    | bmi >= 30           = "Obésité"
    where bmi = weight / (height * height)

-- Tests
main :: IO ()
main = do
    putStrLn $ "bmiCategory 70 1.75: " ++ bmiCategory 70 1.75  -- Affiche "bmiCategory 70 1.75: Normal"
    putStrLn $ "bmiCategory 90 1.8: " ++ bmiCategory 90 1.8   -- Affiche "bmiCategory 90 1.8: Surpoids"
```

### Explications
- **Définition de la fonction** : La fonction `bmiCategory` a la signature de type `Float -> Float -> String`, prenant le poids (en kg) et la taille (en mètres) comme arguments, et retournant une chaîne indiquant la catégorie d'IMC.
- **Calcul de l'IMC** : Une clause `where` définit `bmi = weight / (height * height)`, ce qui correspond à la formule de l'IMC : \( \text{poids} / \text{taille}^2 \).
- **Logique des gardes** :
  - `| bmi < 18.5` : Retourne `"Insuffisance pondérale"` si l'IMC est inférieur à 18.5.
  - `| bmi >= 18.5 && bmi <= 24.9` : Retourne `"Normal"` si l'IMC est entre 18.5 et 24.9 inclus.
  - `| bmi >= 25 && bmi <= 29.9` : Retourne `"Surpoids"` si l'IMC est entre 25 et 29.9 inclus.
  - `| bmi >= 30` : Retourne `"Obésité"` si l'IMC est supérieur ou égal à 30.
- **Tests** : La fonction `main` teste `bmiCategory` avec les paires `(70, 1.75)` et `(90, 1.8)`. Les sorties attendues sont calculées comme suit :
  - Pour `(70, 1.75)` : \( \text{IMC} = 70 / (1.75^2) = 70 / 3.0625 \approx 22.86 \), qui est dans la plage [18.5, 24.9], donc `"Normal"`.
  - Pour `(90, 1.8)` : \( \text{IMC} = 90 / (1.8^2) = 90 / 3.24 \approx 27.78 \), qui est dans la plage [25, 29.9], donc `"Surpoids"`.
- **Résultats des tests** :
  - `bmiCategory 70 1.75: Normal`
  - `bmiCategory 90 1.8: Surpoids`
- **Remarque** : La fonction suppose que les entrées sont valides (poids et taille positifs). Pour plus de robustesse, des vérifications pourraient être ajoutées, mais elles ne sont pas requises ici.

### Résultats des tests
- `bmiCategory 70 1.75: Normal`
- `bmiCategory 90 1.8: Surpoids`
