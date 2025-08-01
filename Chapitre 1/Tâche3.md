HC1T3 - Tâche 3 : Vérifier si un nombre est supérieur à 18
Voici une fonction Haskell simple appelée greaterThan18 qui vérifie si un nombre est supérieur à 18 :

-- Vérifie si un nombre est supérieur à 18

greaterThan18 :: (Ord a, Num a) => a -> Bool
greaterThan18 x = x > 18

-- Exemple d'utilisation
main :: IO ()
main = do
    let n = 20
    putStrLn ("Le nombre " ++ show n ++ (if greaterThan18 n then " est" else " n'est pas") ++ " supérieur à 18.")

Détails :

Signature : (Ord a, Num a) => a -> Bool signifie que la fonction accepte tout type numérique qui peut être comparé (Ord).

x > 18 renvoie un Bool (True ou False).

La fonction est pure, car elle ne dépend que de son entrée.
