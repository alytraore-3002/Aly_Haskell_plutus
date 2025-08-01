-- HC1T1 - Tâche 1 : Composition de fonctions

-- double : multiplie un nombre par 2
double :: Int -> Int
double x = x * 2

-- increment : augmente un nombre de 1
increment :: Int -> Int
increment x = x + 1

-- doubleThenIncrement : applique double puis increment en utilisant la composition de fonctions
doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- Exemple d'utilisation
main :: IO ()
main = do
    let x = 5
    putStrLn ("Résultat de doubleThenIncrement " ++ show x ++ " : " ++ show (doubleThenIncrement x))

Explication :

double x = x * 2 : multiplie l'entrée par 2.

increment x = x + 1 : ajoute 1 à l'entrée.

doubleThenIncrement = increment . double : applique d'abord double, puis increment grâce à la composition.
