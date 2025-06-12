# Haskell
Składnia podstawowego programu
```haskell
square :: Float -> Float
square x = x * x

main :: IO ()
main = do
  print (square 12)
  print (square 145)
  print (square 1451234)
```

Operacje logiczne
| Operator | Znaczenie              | Przykład    | Wynik   |
|----------|------------------------|-------------|---------|
| `==`     | równość                | `5 == 5`    | `True`  |
| `/=`     | nierówność             | `5 /= 3`    | `True`  |
| `<`      | mniejsze niż           | `3 < 4`     | `True`  |
| `>`      | większe niż            | `5 > 2`     | `True`  |
| `<=`     | mniejsze lub równe     | `3 <= 3`    | `True`  |
| `>=`     | większe lub równe      | `4 >= 5`    | `False` |

| Operator | Znaczenie              | Przykład        | Wynik    |
|----------|------------------------|-----------------|----------|
| `&&`     | logiczne "i" (AND)     | `True && False` | `False`  |
| `\|\|`     | logiczne "lub" (OR)    | `True \|\| False` | `True`   |
| `not`    | negacja (NOT)          | `not True`      | `False`  |

Operacje na listach
```haskell
-- Konkatenacja list
listaA = [1, 2] ++ [3, 4]        -- [1,2,3,4]

-- Dodanie elementu na początek listy (operator `:`)
listaB = 0 : [1, 2, 3]           -- [0,1,2,3]

-- Dostęp do elementu o indeksie
x = [10, 20, 30] !! 1            -- 20 (indeks 0-based)

-- Długość listy
len = length [1, 2, 3, 4]        -- 4

-- Czy lista jest pusta?
empty = null []                 -- True

-- Odwracanie listy
rev = reverse [1, 2, 3]          -- [3,2,1]
```

List comprehension
```haskell
-- Kwadraty liczb od 1 do 5
squares = [x*x | x <- [1..5]]     -- [1,4,9,16,25]

-- Liczby parzyste mniejsze niż 10
evens = [x | x <- [1..10], even x]  -- [2,4,6,8,10]

-- Tabliczka mnożenia jako lista trójek (x, y, x*y)
multiplicationTable = [(x, y, x*y) | x <- [1..3], y <- [1..3]]
-- [(1,1,1),(1,2,2),(1,3,3), ..., (3,3,9)]
```

Przykładowe funkcje
```haskell
square x = x * x
cube x = x * x * x
average x y = (x + y) `div` 2

-- Równanie kwadratowe
roots :: Float -> Float -> Float -> Float
roots a b c =
  let d = b*b - 4*a*c in
  if d < 0 then "Brak pierwiastków"
  else if d == 0 then show ((-b) `div` (2*a))
  else show ((-b + sqrt (fromIntegral d)) / (2 * fromIntegral a))

-- Silnia
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Ciąg Fibonacciego
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Różnica max i min
minmax :: Int -> Int -> Int -> Int
minmax a b c = maximum [a,b,c] - minimum [a,b,c]

-- Suma kwadratów
sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x*x + y*y
```

# Prolog
