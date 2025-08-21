HC4T4 - Tâche 4 : Réécrire spécialAnniversaire avec le pattern matching

### Code Haskell
```haskell
specialBirthday :: Int -> [Char]
specialBirthday age = case age of
    1  -> "Premier anniversaire !"
    18 -> "Tu es adulte !"
    60 -> "Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
    _  -> "Rien de spécial"

-- Tests
main :: IO ()
main = do
    putStrLn $ "specialBirthday 1: " ++ specialBirthday 1   -- Affiche "specialBirthday 1: Premier anniversaire !"
    putStrLn $ "specialBirthday 18: " ++ specialBirthday 18 -- Affiche "specialBirthday 18: Tu es adulte !"
    putStrLn $ "specialBirthday 60: " ++ specialBirthday 60 -- Affiche "specialBirthday 60: Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
    putStrLn $ "specialBirthday 25: " ++ specialBirthday 25 -- Affiche "specialBirthday 25: Rien de spécial"
```

### Explications
- **Définition de la fonction** :
  - La fonction `specialBirthday` a la signature de type `Int -> [Char]`, où `[Char]` est synonyme de `String` en Haskell. Elle prend un entier représentant un âge et retourne une chaîne décrivant si l'âge est spécial.
  - La signature est identique à celle de la fonction originale, garantissant la compatibilité.
- **Logique du pattern matching** :
  - L'expression `case age of` effectue le pattern matching sur la valeur de `age`.
  - Les motifs sont :
    - `1` : Retourne `"Premier anniversaire !"`.
    - `18` : Retourne `"Tu es adulte !"`.
    - `60` : Retourne `"Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"`.
    - `_` : Le caractère générique capture tous les autres âges et retourne `"Rien de spécial"`.
  - Le pattern matching est évalué dans l'ordre des motifs, exécutant le premier motif correspondant.
- **Équivalence avec la version originale** :
  - La version originale utilisait des `if-then-else` imbriqués pour vérifier si `age` est égal à 1, 18, ou 60. Le pattern matching remplace cela par une correspondance directe, rendant le code plus concis et idiomatique.
  - Les messages retournés sont identiques à ceux de la version originale.
- **Avantages du pattern matching** :
  - Plus lisible et direct que les `if-then-else` imbriqués, surtout pour des valeurs spécifiques.
  - Réduit le risque d'erreurs dans la logique conditionnelle.
  - Facilite la maintenance : ajouter un nouvel âge spécial nécessite juste un nouveau motif.
- **Remarque** :
  - La fonction gère toutes les entrées entières via le motif `_`, équivalent au `else "Rien de spécial"`.
  - Elle suppose des entrées entières valides, les âges négatifs retournant `"Rien de spécial"`.

### Résumé
La fonction `specialBirthday` utilise le pattern matching avec `case` pour remplacer les `if-then-else` imbriqués, associant les âges 1, 18, et 60 à leurs messages respectifs et utilisant `_` pour les autres âges. Le code est plus concis, idiomatique en Haskell, et conserve exactement la même fonctionnalité que la version originale.
