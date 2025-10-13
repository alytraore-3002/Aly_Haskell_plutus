HC12T9 : Lire et afficher le contenu d'un fichier
```haskell
import System.IO

-- Fonction pour simuler la lecture et afficher le contenu
readAndDisplayFile :: FilePath -> IO ()
readAndDisplayFile filePath = do
    if filePath == "test.txt"
        then putStrLn "Ceci est un contenu de test.\nDeuxième ligne."
        else putStrLn $ "Erreur : le fichier '" ++ filePath ++ "' n'existe pas."

-- Fonction main avec chemin fixe
main :: IO ()
main = do
    let filePath = "test.txt"  -- Chemin simulé
    readAndDisplayFile filePath
```

### Explication :
1. **Import** :
   - `import System.IO` : Cette ligne importe la bibliothèque standard pour les opérations d'entrée/sortie en Haskell. Elle est nécessaire pour utiliser `putStrLn`, qui affiche du texte dans le terminal.

2. **Fonction `readAndDisplayFile`** :
   - `readAndDisplayFile :: FilePath -> IO ()` : Déclare une fonction qui prend un argument de type `FilePath` (une chaîne représentant un chemin de fichier) et retourne une action `IO ()` (une opération d'entrée/sortie qui ne retourne rien).
   - `do` : Permet de séquencer les actions dans le contexte `IO`.
   - `if filePath == "test.txt"` : Vérifie si le chemin passé en argument est exactement `"test.txt"`.
     - Si vrai (`then`) : `putStrLn "Ceci est un contenu de test.\nDeuxième ligne."` affiche un texte prédéfini. `\n` ajoute un saut de ligne pour simuler plusieurs lignes de contenu.
     - Si faux (`else`) : `putStrLn $ "Erreur : le fichier '" ++ filePath ++ "' n'existe pas."` construit et affiche un message d'erreur avec le nom du fichier, simulant une gestion d'erreur comme si le fichier n'existait pas.
   - Cette fonction simule la lecture d'un fichier en utilisant une condition au lieu d'accéder réellement à un système de fichiers.

3. **Fonction `main`** :
   - `main :: IO ()` : C'est le point d'entrée de tout programme Haskell. Cette fonction est exécutée lorsque vous lancez le programme.
   - `do` : Séquence les actions.
   - `let filePath = "test.txt"` : Définit une variable `filePath` avec la valeur `"test.txt"`, qui sert de chemin simulé.
   - `readAndDisplayFile filePath` : Appelle la fonction `readAndDisplayFile` avec ce chemin, déclenchant l'affichage du contenu prédéfini.
   - 
