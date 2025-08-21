HC4T5 - Tâche 5 : Ajouter un cas générique avec un message personnalisé

### Code Haskell
```haskell
specialBirthday :: Int -> [Char]
specialBirthday age = case age of
    1  -> "Premier anniversaire !"
    18 -> "Tu es adulte !"
    60 -> "Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
    _  -> "Rien de spécial à " ++ show age ++ " ans"

-- Tests
main :: IO ()
main = do
    putStrLn $ "specialBirthday 1: " ++ specialBirthday 1   -- Affiche "specialBirthday 1: Premier anniversaire !"
    putStrLn $ "specialBirthday 18: " ++ specialBirthday 18 -- Affiche "specialBirthday 18: Tu es adulte !"
    putStrLn $ "specialBirthday 60: " ++ specialBirthday 60 -- Affiche "specialBirthday 60: Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"
    putStrLn $ "specialBirthday 25: " ++ specialBirthday 25 -- Affiche "specialBirthday 25: Rien de spécial à 25 ans"
```

### Explications
- **Définition de la fonction** :
  - La fonction `specialBirthday` conserve la signature `Int -> [Char]` (équivalent à `Int -> String`), prenant un entier représentant un âge et retournant une chaîne décrivant si l'âge est spécial ou non.
  - La signature reste inchangée par rapport à la version précédente, assurant la compatibilité.

- **Logique du pattern matching** :
  - L'expression `case age of` effectue le pattern matching sur la valeur de `age` :
    - `1` : Retourne `"Premier anniversaire !"`.
    - `18` : Retourne `"Tu es adulte !"`.
    - `60` : Retourne `"Enfin, je peux arrêter de suivre les nouvelles expressions à la mode !"`.
    - `_` : Le motif générique capture tous les autres âges et retourne `"Rien de spécial à " ++ show age ++ " ans"`, où `show age` convertit l'âge (entier) en chaîne pour l'inclure dans le message.
  - Le pattern matching est évalué dans l'ordre, exécutant le premier motif correspondant.

- **Modification pour inclure l'âge** :
  - Dans le cas générique (`_`), le message a été modifié de `"Rien de spécial"` à `"Rien de spécial à " ++ show age ++ " ans"`. La fonction `show` est utilisée pour convertir l'entier `age` en une chaîne, qui est ensuite concaténée avec `"Rien de spécial à "` et `" ans"` pour former un message dynamique.
  - Par exemple, pour `age = 25`, le message devient `"Rien de spécial à 25 ans"`.

- **Avantages du pattern matching** :
  - Le pattern matching reste clair et concis, facilitant la lecture et la maintenance.
  - L'ajout de l'âge dans le message du cas générique est intégré de manière naturelle via la concaténation de chaînes.
  - La structure permet d'ajouter facilement d'autres âges spéciaux en insérant de nouveaux motifs.

- **Remarque** :
  - La fonction gère toutes les entrées entières grâce au motif `_`. Les âges négatifs ou très grands seront inclus dans le message générique (par exemple, `specialBirthday (-1)` retourne `"Rien de spécial à -1 ans"`).
  - La fonction est sensible à la structure des messages, qui respectent exactement les cas spéciaux de la version précédente tout en ajoutant l'âge dans le cas par défaut.

### Résumé
La fonction `specialBirthday` a été modifiée pour utiliser le pattern matching avec `case`, comme dans la version précédente, mais le cas générique `_` inclut désormais l'âge dans le message via `"Rien de spécial à " ++ show age ++ " ans"`. Les âges spéciaux (1, 18, 60) conservent leurs messages d'origine, tandis que tout autre âge génère un message personnalisé avec l'âge. Le code reste concis, idiomatique, et répond aux exigences de la modification.
