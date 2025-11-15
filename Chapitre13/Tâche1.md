HC13T1 : Lister les fichiers du répertoire courant :

```haskell
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)

main :: IO ()
main = do
  -- Obtient la liste des fichiers et dossiers dans le répertoire courant
  contents <- listDirectory "."
  
  -- Affiche chaque élément de la liste
  putStrLn "Fichiers et dossiers dans le répertoire courant :"
  forM_ contents $ \item -> do
    putStrLn item
```

### Explication :

1. **Imports** :
   - `System.Directory (listDirectory)` : Importe la fonction `listDirectory` du module `System.Directory`, qui retourne une liste des entrées (fichiers et dossiers) dans un répertoire donné.
   - `System.FilePath ((</>))` : Importe l'opérateur `</>` pour manipuler les chemins de fichiers, bien qu'il ne soit pas utilisé directement ici (inclus pour une éventuelle extension).
   - `Control.Monad (forM_)` : Importe `forM_`, une fonction utilitaire qui applique une action `IO` à chaque élément d'une liste et ignore le résultat (utile pour l'affichage).

2. **Fonction `main`** :
   - `contents <- listDirectory "."` : Utilise `listDirectory` avec `"."` pour lister le contenu du répertoire courant. Le résultat est stocké dans `contents`, qui est une liste de `FilePath` (chaînes représentant les noms des fichiers/dossiers).
   - `putStrLn "Fichiers et dossiers dans le répertoire courant :"` : Affiche un titre pour la sortie.
   - `forM_ contents $ \item -> do ...` : Itère sur chaque élément de `contents` et affiche son nom avec `putStrLn item`.
   - 
