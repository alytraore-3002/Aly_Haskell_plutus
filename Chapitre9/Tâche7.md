HC9T7 : Fonction pour calculer l'engagement
```haskell
-- Définition du type de données récursif Tweet
data Tweet = Tweet
  { content :: String        -- Contenu du tweet
  , likes :: Int            -- Nombre de likes
  , comments :: [Tweet]     -- Liste de commentaires (eux-mêmes des tweets)
  } deriving (Show)

-- Fonction engagement qui calcule l'engagement total d'un tweet
engagement :: Tweet -> Int
engagement (Tweet _ likes comments) = likes + sum (map engagement comments)

-- Fonction main pour tester
main :: IO ()
main = do
  -- Création d'un tweet avec des commentaires
  let comment1 = Tweet "Great post!" 5 []
  let comment2 = Tweet "Interesting!" 3 []
  let mainTweet = Tweet "Hello, world!" 10 [comment1, comment2]
  
  -- Calcul et affichage de l'engagement
  print $ engagement mainTweet  -- Devrait afficher 18 (10 + 5 + 3)
```

### Explications :
1. **Type `Tweet`** :
   - Comme précédemment, `Tweet` est un type récursif avec trois champs : `content` (String), `likes` (Int), et `comments` (liste de `Tweet`).
   - Le `deriving (Show)` permet d'afficher les tweets pour le débogage.
2. **Fonction `engagement`** :
   - Prend un `Tweet` en entrée.
   - Extrait le nombre de `likes` du tweet courant.
   - Utilise `map engagement comments` pour calculer l'engagement de chaque commentaire (récursivement).
   - Utilise `sum` pour additionner les engagements des commentaires.
   - Retourne la somme des `likes` du tweet courant et de l'engagement des commentaires.
3. **Fonction `main`** :
   - Crée deux commentaires : `comment1` (5 likes, pas de commentaires) et `comment2` (3 likes, pas de commentaires).
   - Crée un tweet principal (`mainTweet`) avec 10 likes et les deux commentaires.
   - Calcule et affiche l'engagement total, qui est `10 (mainTweet) + 5 (comment1) + 3 (comment2) = 18`.
   - 
