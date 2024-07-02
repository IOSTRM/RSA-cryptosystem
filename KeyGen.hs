module KeyGen (getPrvKey, getPubKey, primeList, getFactors) where
import System.IO
import System.Random
import Util

-- Returns the string (n,e)
getPubKey :: (Integer, Integer) -> String
getPubKey (p, q) =  show (n,e)
    where n = p * q
          e = 3

-- Returns the string (n, d)
getPrvKey :: (Integer, Integer) -> String
getPrvKey (p,q) = show (n,d) 
    where n = p * q
          d = getD $ getPhi (p,q)

-- Extended Euclidean Algoritm, returns (x,y) from Bezout's identity
extendedGCD :: (Integer,Integer) -> (Integer, Integer)
extendedGCD (a,b)
    | (b == 0) = (1,0)
    | otherwise = (t, z)
    where (s,t) = extendedGCD(b, a `mod` b)
          z = s - ( (a `div` b) * t)

-- Returns a number phi - Euler totient function 
getPhi :: (Integer, Integer) -> Integer
getPhi (p, q) = (p-1) * (q-1)

-- Returns d so that (d*e) `mod` phi(N) == 1
getD :: Integer -> Integer
getD phi = if y < 0 then y + phi else y
    where (_, y) = extendedGCD (phi, e)
          e = 3

-- Returns a list of prime numbers between 2^511 and 2^513,
-- which we need to find large p, q
primeList :: StdGen -> [Integer]
primeList gen = filter (isRabinMillerPrime gen) randomList
    where randomList = randomRs ( (2^511), (2^513) ) gen

-- Find a factor p so that p is prime and p `mod` e /= 1	
findFactor :: [Integer] -> Int -> (Integer, Int)
findFactor primes x = if ( current `mod` e /= 1 ) 
              then (current,x)
              else findFactor primes (x + 1)
    where current = primes !! x
          e = 3

-- Returns (p,q)
getFactors :: [Integer] -> (Integer, Integer)
getFactors primes = (p, fst $ findFactor primes (pi + 1))
            where (p, pi) = findFactor primes 0

-- Checks whether an Integer is a Rabin-Miller prime
isRabinMillerPrime :: StdGen -> Integer -> Bool
isRabinMillerPrime gen n = odd n && result
    where result = all (millerRabinTest s n) (millerRabinList n 30 gen)
          (_, s) = millerRabinForm n

-- Returns (d,s) where (2 ^ s) * d = n - 1
millerRabinForm :: (Integral a) => a -> (a, a)
millerRabinForm n = (d, fromIntegral s)
    where pn = pred n
          factorList = iterate (`div` 2) pn
          (evenFactors, d : _) = span even factorList
          s = length evenFactors

-- Creates the list of ((a^d) mod n)s where a is random. 1s and (n - 1)s are discarded
millerRabinList :: Integer -> Int -> StdGen -> [Integer]
millerRabinList _ 0 _ = []
millerRabinList n k gen  = 
    let (d,s) = millerRabinForm n
        (a, newGen) = randomR (2, (n - 2)) gen
        rabinTest = powerMod a d n
        result = if (rabinTest == 1 || rabinTest == (n-1)) 
            then millerRabinList n (k - 1) newGen
            else rabinTest : millerRabinList n (k - 1) newGen  
    in result

-- Inner loop of the Rabin-Miller algorithm
millerRabinTest :: (Integral a, Integral b) => b-> a -> a -> Bool
millerRabinTest s n x
    | t == 1 = False
    | s == 1 = False 
    | t == (n - 1) = True
    | otherwise  = millerRabinTest (s - 1) n t
    where t = powerMod x 2 n