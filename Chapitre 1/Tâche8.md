HC1T8 - Tâche 8 : Fonctions d'ordre supérieur
---

-- applyTwice : applique une fonction deux fois à une valeur

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Exemple d'utilisation
double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
    let result = applyTwice double 3  -- double (double 3) = double 6 = 12
    putStrLn ("Résultat de applyTwice double 3 : " ++ show result)


---

✅ Détails :

Élément	Description

applyTwice	Prend une fonction f et une valeur x, et renvoie f (f x)
(a -> a) -> a -> a	Signature : une fonction de type a -> a, une valeur a, retourne un a
double	Fonction d'exemple : multiplie par 2



---

🔢 Exemple de sortie :

Résultat de applyTwice double 3 : 12
