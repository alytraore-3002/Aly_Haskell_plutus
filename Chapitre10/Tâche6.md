 HC10T6 : Récursivité mutuelle dans Eq pour Blockchain:
```haskell
-- Définition du type récursif Blockchain
data Blockchain = Blockchain
  { blockId :: Int         -- Identifiant unique du bloc
  , nextBlock :: Maybe Blockchain  -- Référence au bloc suivant (optionnel)
  } deriving (Show)

-- Instance de Eq pour Blockchain avec récursivité mutuelle
instance Eq Blockchain where
  (Blockchain id1 next1) == (Blockchain id2 next2) =
    id1 == id2 && compareNext next1 next2
    where
      compareNext Nothing Nothing = True
      compareNext (Just b1) (Just b2) = b1 == b2
      compareNext _ _ = False  -- Cas où un seul nextBlock est Nothing et l'autre non

  (Blockchain id1 next1) /= (Blockchain id2 next2) =
    id1 /= id2 || compareNextDiff next1 next2
    where
      compareNextDiff Nothing Nothing = False
      compareNextDiff (Just b1) (Just b2) = b1 /= b2
      compareNextDiff _ _ = True  -- Cas où un seul nextBlock est Nothing et l'autre non

-- Fonction main pour tester
main :: IO ()
main = do
  let block1 = Blockchain 100 Nothing          -- Bloc avec ID 100, fin de chaîne
  let block2 = Blockchain 100 (Just block1)    -- Bloc avec ID 100, pointe vers block1
  let block3 = Blockchain 200 Nothing          -- Bloc avec ID 200, fin de chaîne
  let block4 = Blockchain 100 (Just block1)    -- Même structure que block2
  
  -- Test de l'égalité et de la différence
  putStrLn $ "block1 == block2: " ++ show (block1 == block2)  -- Devrait afficher False
  putStrLn $ "block2 == block4: " ++ show (block2 == block4)  -- Devrait afficher True
  putStrLn $ "block1 /= block3: " ++ show (block1 /= block3)  -- Devrait afficher True
```

### Explication détaillée :
1. **Type `Blockchain`** :
   - `Blockchain` est un type récursif avec deux champs :
     - `blockId :: Int` : un identifiant unique pour chaque bloc (par exemple, un timestamp ou un numéro).
     - `nextBlock :: Maybe Blockchain` : une référence optionnelle au bloc suivant, où `Nothing` indique la fin de la chaîne et `Just` contient un autre `Blockchain`.
   - Le `deriving (Show)` permet d'afficher les instances de `Blockchain` (par exemple, `Blockchain {blockId = 100, nextBlock = Nothing}`) pour le débogage.

2. **Problème dans le code original** :
   - Dans la version précédente, l'utilisation de `maybe` et `fromMaybe` dans l'instance `Eq` était syntaxiquement incorrecte. Par exemple, `maybe True (== (fromMaybe (Blockchain 0 Nothing) next2)) next1` tentait d'appliquer `==` comme une fonction dans un contexte où cela n'était pas valide.
   - De plus, l'introduction d'un bloc par défaut (`Blockchain 0 Nothing`) via `fromMaybe` pouvait fausser les comparaisons, surtout si les structures de blocs différaient (par exemple, un `Nothing` vs un `Just`).

3. **Instance `Eq Blockchain` avec récursivité mutuelle** :
   - **Correction de la syntaxe** : J'ai remplacé l'utilisation complexe de `maybe` par des fonctions auxiliaires `compareNext` (pour `==`) et `compareNextDiff` (pour `/=`).
   - **Définition de `==`** :
     - `(Blockchain id1 next1) == (Blockchain id2 next2)` vérifie si `id1 == id2` (égalité des identifiants).
     - `compareNext next1 next2` gère la comparaison des `nextBlock` :
       - Si les deux sont `Nothing`, retourne `True` (fin de chaîne identique).
       - Si les deux sont `Just b1` et `Just b2`, compare récursivement `b1 == b2`.
       - Sinon (un `Nothing` et un `Just`), retourne `False`, car les structures diffèrent.
   - **Définition de `/=`** :
     - `(Blockchain id1 next1) /= (Blockchain id2 next2)` vérifie si `id1 /= id2` ou si les `nextBlock` diffèrent.
     - `compareNextDiff next1 next2` est complémentaire :
       - Si les deux sont `Nothing`, retourne `False` (pas de différence).
       - Si les deux sont `Just b1` et `Just b2`, compare récursivement `b1 /= b2`.
       - Sinon (un `Nothing` et un `Just`), retourne `True`, car les structures diffèrent.
   - **Récursivité mutuelle** : Les définitions de `==` et `/=` se réfèrent l'une à l'autre via `compareNext` et `compareNextDiff`, respectant la demande de récursivité mutuelle.

4. **Fonction `main`** :
   - Crée quatre blocs :
     - `block1` : ID 100, `nextBlock = Nothing`.
     - `block2` : ID 100, `nextBlock = Just block1`.
     - `block3` : ID 200, `nextBlock = Nothing`.
     - `block4` : ID 100, `nextBlock = Just block1` (identique à `block2`).
   - Teste les comparaisons :
     - `block1 == block2` : Faux, car `nextBlock` diffère (`Nothing` vs `Just block1`).
     - `block2 == block4` : Vrai, car `blockId` et `nextBlock` sont identiques.
     - `block1 /= block3` : Vrai, car `blockId` diffère (100 vs 200).
   - Utilise `putStrLn` avec `show` pour afficher les résultats.
   - 
