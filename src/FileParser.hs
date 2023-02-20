--
-- ECDSA haskell implemention
-- module FileParser
-- This file implementing File parsing
-- @author Lukáš Plevač <at> BUT FIT 
--
module FileParser where

data Point = Point { 
  x :: Integer,
  y :: Integer
} deriving (Show)

data Curve = Curve {
    p :: Integer,
    a :: Integer,
    b :: Integer,
    g :: Point,
    n :: Integer,
    h :: Integer
} deriving (Show)

testCurve = Curve {
    p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F,
    a = 0,
    b = 7,
    
    g = Point {
        x = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
        y = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    },

    n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141,
    h = 1
}
