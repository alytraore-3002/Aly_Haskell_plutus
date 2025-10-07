HC7T1 : Implémenter une instance Eq pour un type personnalisé

```haskell
-- Définition du type de données Color
data Color = Red | Green | Blue deriving (Show, Eq)

-- Fonction principale
main :: IO ()
main = do
    let c1 = Red
    let c2 = Green
    let c3 = Red
    print (c1 == c2)  -- Affiche False (Red != Green)
    print (c1 == c3)  -- Affiche True (Red == Red)
```

### Explications :
1. **Définition du type de données `Color`** :
   - `data Color = Red | Green | Blue` : Définit un type algébrique `Color` avec trois constructeurs : `Red`, `Green`, et `Blue`. Ces constructeurs représentent les trois couleurs possibles.
   - `deriving (Show, Eq)` : 
     - `Show` permet d'afficher les valeurs du type (par exemple, avec `print`).
     - `Eq` permet de comparer les valeurs pour l'égalité (par exemple, avec `==`). En dérivant `Eq`, Haskell génère automatiquement une implémentation d'égalité qui considère deux couleurs comme égales si elles ont le même constructeur (par exemple, `Red == Red`).

2. **Fonction `main`** :
   - `main :: IO ()` : Point d'entrée du programme, gérant les opérations d'entrée/sortie.
   - `let c1 = Red` : Définit une variable `c1` comme étant de couleur `Red`.
   - `let c2 = Green` : Définit une variable `c2` comme étant de couleur `Green`.
   - `let c3 = Red` : Définit une variable `c3` comme étant de couleur `Red`.
   - `print (c1 == c2)` : Compare `c1` et `c2`. Puisque `Red != Green`, cela affiche `False`.
   - `print (c1 == c3)` : Compare `c1` et `c3`. Puisque `Red == Red`, cela affiche `True`.
