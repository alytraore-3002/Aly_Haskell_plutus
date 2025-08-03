HC1T1 - Tâche 1 : Composition de fonctions

```haskell
double :: Int -> Int
double x = x * 2

incrément :: Int -> Int
incrément x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = incrément . double
```

Explications :
- `double` prend un entier et le multiplie par 2
- `incrément` prend un entier et ajoute 1
- `doubleThenIncrement` utilise la composition de fonctions (opérateur `.`) pour appliquer d'abord `double` puis `incrément`
- L'opérateur `.` combine les fonctions de sorte que `(f . g) x = f (g x)`

Exemple d'utilisation :
```haskell
> doubleThenIncrement 5
11  -- car (5 * 2) + 1 = 11
```
