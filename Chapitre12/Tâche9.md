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
