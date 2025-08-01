HC1T6 - Tâche 6 : Utilisation de signatures de type
---

-- Fonction addNumbers : additionne deux entiers

addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Exemple d'utilisation
main :: IO ()
main = do
    let a = 7
    let b = 5
    putStrLn ("La somme de " ++ show a ++ " et " ++ show b ++ " est : " ++ show (addNumbers a b))


---

✅ Détails :

Élément	Description

addNumbers	Nom de la fonction
Int -> Int -> Int	Type : prend deux Int, retourne un Int
x + y	Addition des deux entiers



---

🔢 Exemple de sortie :

La somme de 7 et 5 est : 12
