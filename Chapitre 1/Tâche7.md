HC1T7 - Tâche 7 : Conversion Fahrenheit/Celsius

---

-- Fonction fToC : convertit Fahrenheit en Celsius

fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

-- Exemple d'utilisation
main :: IO ()
main = do
    let fahrenheit = 98.6
    let celsius = fToC fahrenheit
    putStrLn ("Température en Fahrenheit : " ++ show fahrenheit)
    putStrLn ("Température en Celsius : " ++ show celsius)


---

✅ Détails :

Élément	Description

fToC	Nom de la fonction
Floating a => a -> a	Type générique (fonctionne avec Float, Double, etc.)
(f - 32) * 5 / 9	Formule de conversion : (F − 32) × 5⁄9



---

🔢 Exemple de sortie :

Température en Fahrenheit : 98.6
Température en Celsius : 37.0
