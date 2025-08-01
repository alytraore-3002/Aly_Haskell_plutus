-- HC1T4 - Tâche 4 : Composer une fonction pour traiter des données de joueurs
Voici un programme Haskell bien détaillé avec toutes les fonctions demandées :

extractPlayers

sortByScore

topThree

et leur composition dans getTopThreePlayers.

import Data.List (sortBy)
import Data.Ord (comparing)

-- Type alias pour plus de clarté
type Player = (String, Int)

-- 1. extractPlayers : extrait les noms des joueurs à partir d'une liste de (nom, score)
extractPlayers :: [Player] -> [String]
extractPlayers players = [name | (name, _) <- players]

-- 2. sortByScore : trie les joueurs par score décroissant
sortByScore :: [Player] -> [Player]
sortByScore players = sortBy (flip (comparing snd)) players
-- 'snd' extrait le score, 'flip' inverse l'ordre croissant en décroissant

-- 3. topThree : retourne les trois premiers éléments d'une liste
topThree :: [Player] -> [Player]
topThree = take 3

-- 4. getTopThreePlayers : compose les fonctions pour obtenir les noms des 3 meilleurs joueurs
getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Exemple d'utilisation
main :: IO ()
main = do
    let players = [ ("Alice", 42)
                  , ("Bob", 58)
                  , ("Charlie", 37)
                  , ("Diana", 63)
                  , ("Eve", 50)
                  ]

    putStrLn "Liste initiale des joueurs :"
    print players

    putStrLn "\nLes trois meilleurs joueurs (noms uniquement) :"
    print (getTopThreePlayers players)


---

✅ Ce que fait chaque fonction :

Fonction	Rôle

extractPlayers	Extrait les noms des joueurs ([String]) à partir d'une liste de (String, Int)
sortByScore	Trie les tuples (nom, score) en ordre décroissant de score
topThree	Garde les trois premiers éléments de la liste
getTopThreePlayers	Combine tout : trie → prend 3 meilleurs → extrait noms



---

📌 Résultat pour l’exemple donné :

Avec la liste :

[("Alice", 42), ("Bob", 58), ("Charlie", 37), ("Diana", 63), ("Eve", 50)]

Le programme affichera :

["Diana","Bob","Eve"]
