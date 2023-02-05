-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
{-
The Bounded class is used to name the upper and lower limits of a type. Ord is not a superclass of Bounded since types that are not totally ordered may also have upper and lower bounds.
Some instances: Bool, Char, Ordering, Int, Tuple
Behaviours:
    minBound: returns the minimum value of a given data type.
    maxBound: returns the maximum value of a given data type.
  VD:
    maxBound::Int --> 9223372036854775807
    minBound::Char --> '\NUL'
    maxBound::Bool --> True
-}

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
{-
Int and Word types are both members of the Integral typeclass. 
Int is a signed integer type with a range of -9223372036854775808 to 9223372036854775807 (64 bit)
  minBound::Int   --> -9223372036854775808
  maxBound::Int   --> 9223372036854775807
Word is an unsigned integer type with a range of 0 to 18446744073709551615  (64 bit)
  minBound::Word  --> 0
  maxBound::Word  --> 18446744073709551615 
-}

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?
{-
Class Enum defines operations on sequentially ordered types.
Some instances: Bool, Char, Ordering, Int, Integer, Float, Double
Behaviors:
    succ           :: a -> a  -- the successor of a value.  For numeric
                                 types, 'succ' adds 1.
    pred           :: a -> a  -- the predecessor of a value.  For numeric
                                 types, 'pred' subtracts 1.
    toEnum         :: Int -> a  --converts a value from Int to value in any Enum's type
    fromEnum       :: a -> Int  --converts a value from any type in Enum to Int
    enumFrom       :: a -> [a]            -- [n..]  
    enumFromThen   :: a -> a -> [a]       -- [n,n'..]  
    enumFromTo     :: a -> a -> [a]       -- [n..m]  
    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m] 

    succ 1  ---> 2
    pred 'e' --> 'd'
    succ maxBound::Int  --> error
    pred minBound::Int  --> error
-}

-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.
f1 :: (Show a, Fractional a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Eq a, Bounded a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x

-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
{-
Converting from and between integral types
  fromIntegral :: (Integral a, Num b) => a -> b
  fromInteger :: Num a => Integer -> a
  toInteger:: Integral a => a -> Integer

Converting from real and between real-fractional types
  realToFrac:: (Real a, Fractional b) => a -> b
  fromRational :: Fractional a => Rational -> a
  toRational :: Real a => a -> Rational

Converting from real-fractional numbers to integral numbers
  ceiling  :: (RealFrac a, Integral b) => a -> b
  floor    :: (RealFrac a, Integral b) => a -> b
  truncate :: (RealFrac a, Integral b) => a -> b
  round    :: (RealFrac a, Integral b) => a -> b

Converting between different floating-point precisions (GHC.Float module):
 float2Double :: Float -> Double
 double2Float :: Double -> Float

Reference: https://wiki.haskell.org/Converting_numbers
-}