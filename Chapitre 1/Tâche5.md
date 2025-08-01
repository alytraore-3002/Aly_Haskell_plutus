-- HC1T5 - Tâche 5 : Paresse en Haskell

---

-- Génère une liste infinie de nombres et en extrait les n premiers

-- 1. infiniteNumbers : génère les entiers naturels à partir de 1
infiniteNumbers :: [Integer]
infiniteNumbers = [1..]  -- ou enumFrom 1

-- 2. takeFirstN : extrait les n premiers nombres de la liste infinie
takeFirstN :: Int -> [Integer]
takeFirstN n = take n infiniteNumbers

-- Exemple d'utilisation
main :: IO ()
main = do
    let n = 10  -- nombre d'éléments à extraire
    putStrLn ("Les " ++ show n ++ " premiers nombres :")
    print (takeFirstN n)


---

✅ Explication des fonctions :

Fonction	Rôle

infiniteNumbers	Génère la suite infinie : [1, 2, 3, 4, ...]
takeFirstN n	Utilise take pour obtenir les n premiers éléments de la liste


🧠 Note :

Haskell évalue paresseusement (lazy evaluation), donc on peut manipuler des listes infinies sans planter, tant qu’on n’en demande qu’une partie.


---

📌 Exemple de sortie pour n = 10 :

[1,2,3,4,5,6,7,8,9,10]
