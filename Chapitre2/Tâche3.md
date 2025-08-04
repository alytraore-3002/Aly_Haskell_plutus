HC2T3 - Tâche 3 : Variables immuables

### En Haskell, les variables sont immuables par défaut, ce qui signifie qu'une fois définies, leurs valeurs ne peuvent pas être modifiées. Je vais définir les variables avec les types spécifiés (`Int`, `Double`, `String`, `Bool`), puis tenter de modifier l'une d'entre elles pour démontrer l'erreur qui en résulte.

### Définition des variables

Voici les définitions des variables avec leurs types explicites :

- `myAge` : Un `Int` représentant un âge.
- `piValue` : Un `Double` représentant la constante π.
- `salut` : Un `String` contenant une salutation.
- `isHaskellFun` : Un `Bool` indiquant si Haskell est amusant.

### Code complet avec tentative de modification

```haskell
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

salut :: String
salut = "Bonjour, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main :: IO ()
main = do
    putStrLn "Valeur de myAge :"
    print myAge  -- Affiche 25
    putStrLn "Valeur de piValue :"
    print piValue  -- Affiche 3.14159
    putStrLn "Valeur de salut :"
    putStrLn salut  -- Affiche "Bonjour, Haskell!"
    putStrLn "Valeur de isHaskellFun :"
    print isHaskellFun  -- Affiche True
```

### Tentative de modification

Maintenant, essayons de modifier l'une des variables, par exemple `myAge`, pour voir ce qui se passe. Ajoutons une tentative de redéfinition de `myAge` dans le même module :

```haskell
myAge :: Int
myAge = 25

myAge :: Int  -- Tentative de redéfinition
myAge = 30    -- Essaie de changer la valeur

piValue :: Double
piValue = 3.14159

salut :: String
salut = "Bonjour, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main :: IO ()
main = do
    putStrLn "Valeur de myAge :"
    print myAge
    putStrLn "Valeur de piValue :"
    print piValue
    putStrLn "Valeur de salut :"
    putStrLn salut
    putStrLn "Valeur de isHaskellFun :"
    print isHaskellFun
```

### Résultat de la tentative de modification

En Haskell, définir deux fois la même variable dans le même module (même portée) provoque une erreur de compilation. Si vous essayez de compiler ce code (par exemple, avec `ghc` ou dans GHCi), vous obtiendrez une erreur comme celle-ci :

```
Multiple declarations of ‘myAge’
Declared at:
  Main.hs:1:1
  Main.hs:4:1
```

Cette erreur se produit parce que Haskell interdit la redéfinition d'une variable dans la même portée. L'immuabilité garantit qu'une fois qu'une variable comme `myAge` est définie avec une valeur (ici `25`), elle ne peut pas être modifiée ou redéfinie dans la même portée.

### Explications

- **Immuabilité** : En Haskell, les variables ne sont pas des conteneurs modifiables comme dans les langages impératifs. Une variable est un nom lié à une valeur, et cette liaison est fixe dans sa portée.
- **Solution pour "modifier" une variable** : Si vous avez besoin de travailler avec une nouvelle valeur, vous devez définir une nouvelle variable avec un nom différent ou utiliser une portée différente (par exemple, dans une fonction ou une clause `let`).

Par exemple, pour "modifier" `myAge` dans une nouvelle portée, vous pourriez écrire :

```haskell
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

salut :: String
salut = "Bonjour, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main :: IO ()
main = do
    putStrLn "Valeur de myAge :"
    print myAge  -- Affiche 25
    let myAge = 30  -- Nouvelle liaison dans une portée locale
    putStrLn "Nouvelle valeur de myAge (dans une portée locale) :"
    print myAge  -- Affiche 30
    putStrLn "Valeur de piValue :"
    print piValue
    putStrLn "Valeur de salut :"
    putStrLn salut
    putStrLn "Valeur de isHaskellFun :"
    print isHaskellFun
```

Dans ce cas, la clause `let myAge = 30` crée une nouvelle liaison locale pour `myAge` dans le bloc `do`, sans affecter la définition globale de `myAge`. La sortie serait :

```
Valeur de myAge :
25
Nouvelle valeur de myAge (dans une portée locale) :
30
Valeur de piValue :
3.14159
Valeur de salut :
Bonjour, Haskell!
Valeur de isHaskellFun :
True
```

### Résumé

- Les variables `myAge :: Int`, `piValue :: Double`, `salut :: String`, et `isHaskellFun :: Bool` sont définies comme immuables.
- Toute tentative de redéfinition dans la même portée provoque une erreur de compilation.
- Pour "modifier" une variable, il faut créer une nouvelle liaison dans une portée différente (par exemple, avec `let` ou dans une fonction).
