HC1T2 - Tâche 2 : Exemple de fonction pure
-- Fonction pure pour calculer l'aire d'un cercle à partir du rayon

circleArea :: Floating a => a -> a
circleArea r = pi * r ^ 2

-- Exemple d'utilisation
main :: IO ()
main = do
    let rayon = 3.0
    putStrLn ("Aire du cercle de rayon " ++ show rayon ++ " : " ++ show (circleArea rayon))

Détails :

circleArea est pure, car elle :

dépend uniquement de son argument r,

n'utilise aucun état externe,

n’a aucun effet de bord.


Floating a => a -> a rend la fonction générique pour tout type flottant (Float, Double, etc.).

L'aire d’un cercle est donnée par la formule : π × r², soit pi * r ^ 2 en Haskell.


Souhaite-tu que je commente chaque ligne ou que je t’aide à tester cela dans GHCi ?


