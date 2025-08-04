HC2T1 - Tâche 1 : Vérification des types dans GHCi

---

### Types attendus
1. **42** : Entier littéral, type attendu : `Integer` ou `int`
2. **3.14** : Nombre à virgule flottante, type attendu : `Double` (ou `float`).
3. **"Haskell"** : Chaîne de caractères, type attendu : `String` (ou `[Char]`).
4. **'Z'** : Caractère littéral, type attendu : `Char`.
5. **Vrai et faux** : Valeurs booléennes, types attendus : `Bool` pour `True` et `Bool` pour `False`.

### Vérification dans GHCi (simulation)
Voici ce que GHCi retournerait avec la commande `:t` :

1. **42** :
   ```haskell
   :t 42
   42 :: Num p => p
   ```
   - Explication : Littéral numérique polymorphe dans la classe `Num`, souvent `Integer`.

2. **3.14** :
   ```haskell
   :t 3.14
   3.14 :: Fractional p => p
   ```
   - Explication : Flottant polymorphe dans la classe `Fractional`, souvent `Double`.

3. **"Haskell"** :
   ```haskell
   :t "Haskell"
   "Haskell" :: [Char]
   ```
   - Explication : Chaîne de caractères, équivalent à une liste de `String`.

4. **'Z'** :
   ```haskell
   :t 'Z'
   'Z' :: Char
   ```
   - Explication : Caractère littéral, type `Char`.

5. **True et False** :
   ```haskell
   :t True
   True :: Bool
   :t False
   False :: Bool
   ```
   - Explication : `True` et `False` sont des valeurs du type `Bool`.

### Résumé des types
- `42` : `Num a => a` (souvent `Integer`)
- `3.14` : `Fractional a => a` (souvent `Double`)
- `"Haskell"` : `[Char]` (ou `String`)
- `'Z'` : `Char`
- `True` : `Bool`
- `False` : `Bool`

---
