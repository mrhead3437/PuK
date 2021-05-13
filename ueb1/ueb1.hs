--Programmiersprachen und Konzepte WS2021
--Gruppe SeBa

--Eigene Listenfunktionen

myhead :: [a] -> a
myhead (x:xs) = x

mytail :: [a] -> [a]
mytail (x:xs) = xs

mylast :: [a] -> a
mylast [x] = x 
mylast (x:xs) = mylast xs

myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = x : myinit xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

--FizzBuzz
var x
    | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
    | mod x 3  == 0 = "Fizz"
    | mod x 5  == 0 = "Buzz"
    | otherwise  = show x

fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz n = [var x| x <- [1..n]]

--Quellen: https://www.futurelearn.com/courses/functional-programming-haskell/0/steps/27226

--Fibonacci-Folge
fibonacci :: Num a => [a]
fibonacci = [0,1] ++ [ a + b | (a, b) <- zip fibonacci (mytail fibonacci) ]

--Quellen: https://mail.haskell.org/pipermail/beginners/2016-June/016969.html

--Primzahlen
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime a | (length [x | x <- [2 .. a-1], mod a x == 0]) > 0 = False
            | otherwise = True

primes :: [Int]
primes = [x | x <- [1..], isPrime x]

--Primfaktorzerlegung
primeFactors n =  factor n primes
    where
        factor n (x:xs)
            | n < 2       = []
            | mod n x == 0 = x : factor (div n x) (x:xs)
            | otherwise      = factor n xs

--Quelle: https://stackoverflow.com/questions/21276844/prime-factors-in-haskell