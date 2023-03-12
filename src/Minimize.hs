module Minimize where
import Types
import System.Random
import Data.Bits

--
-- Support function for curve aritmetic
--
toBits :: Integer -> [Integer]
toBits n = reverse (recurseCalc n)
  where
    recurseCalc :: Integer -> [Integer]
    recurseCalc 0 = []
    recurseCalc x = (x `mod` 2) : recurseCalc (x `div` 2)

dropFirst :: [Integer] -> [Integer]
dropFirst (_:xs) = xs
dropFirst []     = []

modularInverse :: Integer -> Integer -> Integer
modularInverse a m = (findY 0 1 aPos m) `mod` m
  where
    aPos :: Integer
    aPos  = if a < 0 then a `mod` m else a   -- make sure a is positive
        
    findY :: Integer -> Integer -> Integer -> Integer -> Integer
    findY prevY y an mn = if an > 1 then findY y (prevY - (mn `div` an) * y) (mn `mod` an) an else y

double :: Point -> Curve -> Point 
double point cr = (Point xn yn)
  where
    --slope = (3x₁² + a) / 2y₁
    slope :: Integer
    slope = ((3 * (x point) * (x point) + (a cr)) * (modularInverse (2 * (y point)) (p cr))) `mod` (p cr)

    -- x = slope² - 2x₁
    xn :: Integer
    xn = (slope * slope - (2 * (x point))) `mod` (p cr)

    -- y = slope * (x₁ - x) - y₁
    yn :: Integer
    yn = (slope * ((x point) - xn) - (y point)) `mod` (p cr)

add :: Point -> Point -> Curve -> Point
add point1 point2 cr = if point1 == point2 then double point1 cr else (Point xn yn)
  where
    --slope = (y₁ - y₂) / (x₁ - x₂)
    --slope = ((point1[:y] - point2[:y]) * inverse(point1[:x] - point2[:x], $p)) % $p
    slope :: Integer
    slope = (((y point1) - (y point2)) * (modularInverse ((x point1) - (x point2)) (p cr))) `mod` (p cr)

    -- x = slope² - x₁ - x₂
    -- x = (slope ** 2 - point1[:x] - point2[:x]) % $p
    xn :: Integer
    xn = (slope * slope - (x point1) - (x point2)) `mod` (p cr)

    -- y = slope * (x₁ - x) - y₁
    -- y = ((slope * (point1[:x] - x)) - point1[:y]) % $p
    yn :: Integer
    yn = (slope * ((x point1) - xn) - (y point1)) `mod` (p cr)


multipy :: Point -> Integer -> Curve -> Point
multipy g i cr = multipyBin g (dropFirst (toBits i)) 
  where
    multipyBin :: Point -> [Integer] -> Point
    multipyBin point (1:xs) = multipyBin (add (double point cr) g cr)  xs
    multipyBin point (0:xs) = multipyBin (double point cr) xs
    multipyBin point _      = point

--
-- Ecdsa functions
--

generateKey :: Config -> Integer -> Key
generateKey cfg privateKey = (Key privateKey (multipy (g (curve cfg)) privateKey (curve cfg)))

generatePrivateKey :: Config -> IO Integer
generatePrivateKey cfg = randomRIO (1, (n (curve cfg)) - 1)

generateNonce :: Config -> IO Integer
generateNonce cfg = randomRIO (1, (n (curve cfg)) - 1)

generateSign :: Config -> Integer -> Integer -> Signature