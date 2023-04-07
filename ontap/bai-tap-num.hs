import Data.List (iterate,nub)

-- Exercise 1
-----------------------------------------------------------------------------------------------------------
{- 
Create function fib that returns n'th element of Fibonacci sequence (classic programming task).
-}
-----------------------------------------------------------------------------------------------------------
--Code here
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--cách tính nhanh hơn
fib' :: Int -> Int
fib' n = round $ phi ^ n / sqrt 5
  where phi = (1 + sqrt 5) / 2

fib'' :: Int -> Int
fib'' = (fibs !!)
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- Exercise 2
-----------------------------------------------------------------------------------------------------------
{- 
Give the summation of all even numbers in a Fibonacci sequence up to, but not including, 
the number passed to your function. 
Or, in other words, sum all the even Fibonacci numbers that are lower than the given number n 
(n is not the nth element of Fibonnacci sequence) without including n.
The Fibonacci sequence is a series of numbers where the next value is the addition of the previous two values. 
The series starts with 0 and 1:
0 1 1 2 3 5 8 13 21...
For example:
fibSum 0 -> 0
fibSum 33 -> 10
fibSum 25997544 -> 19544084
-}
-----------------------------------------------------------------------------------------------------------
--Code here
fibSum :: Int -> Int
--fibSum 0 = 0
fibSum n = sum . filter even . takeWhile (< n) $ fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)




-- Exercise 3
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that generates factors for a given number.
The function takes an integer on the standard input and returns a list of integers 
(ObjC: array of NSNumbers representing integers). 
That list contains the prime factors in numerical sequence.
Examples
1  ==>  []
3  ==>  [3]
8  ==>  [2, 2, 2]
9  ==>  [3, 3]
12 ==>  [2, 2, 3]
-}
-----------------------------------------------------------------------------------------------------------
--Code here
primeFactors :: Integer -> [Integer]
primeFactors n | n<2 = []
primeFactors n = findFactors n $ sieveEratosthenes [2..n]
  where 
    findFactors :: Integer -> [Integer] -> [Integer]
    findFactors 1 _ = []
    --findPrimes m [] = [m]
    findFactors m (p:ps)
          | m `mod` p == 0 = p : findFactors (m `div` p) (p:ps)
          | otherwise = findFactors m ps

    --Tìm list số nguyên tố <= n bằng sàng Eratosthenes
    sieveEratosthenes :: [Integer] -> [Integer]
    sieveEratosthenes (p:ps)
        | p^2 > n = p:ps
        | otherwise = p : sieveEratosthenes (filter (\x -> x `mod` p /= 0) ps)


-- Exercise 4
-----------------------------------------------------------------------------------------------------------
{- Numbers ending with zeros are boring.
They might be fun in your world, but not here.
Get rid of them. Only the ending ones.
1450 -> 145
960000 -> 96
1050 -> 105
-1050 -> -105
Zero alone is fine, don't worry about it. Poor guy anyway
-}
-----------------------------------------------------------------------------------------------------------
--Code here
rmEndingZeros :: Integer -> Integer
rmEndingZeros 0 = 0
rmEndingZeros n 
    | n `mod` 10 /= 0 = n
    | otherwise = rmEndingZeros $ n `div` 10




-- Exercise 5
-----------------------------------------------------------------------------------------------------------
{-
Write a function that takes an integer as input, 
and returns the number of bits that are equal to one in the binary representation of that number. 
You can guarantee that input is non-negative.
Example: The binary representation of 1234 is 10011010010, so the function should return 5 in this case
-}
-----------------------------------------------------------------------------------------------------------
--Code here
countBits :: Int -> Int
countBits 0 = 0
countBits n = b + countBits m
 where (m, b) = n `divMod` 2




-- Exercise 6
-----------------------------------------------------------------------------------------------------------
{- 
Write a function bell that will receive a positive integer and return an array. 
That's all splaining you receive; what needs to be done you'll have to figure out with the examples below.
 n => resulting array
 1 => [1]
 2 => [2, 2]
 3 => [3, 4, 3]
 4 => [4, 6, 6, 4]
 5 => [5, 8, 9, 8, 5]
 6 => [6, 10, 12, 12, 10, 6]
 7 => [7, 12, 15, 16, 15, 12, 7]
 8 => [8, 14, 18, 20, 20, 18, 14, 8]
 9 => [9, 16, 21, 24, 25, 24, 21, 16, 9]
10 => [10, 18, 24, 28, 30, 30, 28, 24, 18, 10]
11 => [11, 20, 27, 32, 35, 36, 35, 32, 27, 20, 11]
12 => [12, 22, 30, 36, 40, 42, 42, 40, 36, 30, 22, 12]
-}
-----------------------------------------------------------------------------------------------------------
bellFunc :: Int -> [Int] 
bellFunc n = zipWith (*) [1..n] [n,n-1..1]




--Code here





-- Exercise 7
-----------------------------------------------------------------------------------------------------------
{- 
In a small town the population is p0 = 1000 at the beginning of a year. 
The population regularly increases by 2 percent per year 
and moreover 50 new inhabitants per year come to live in the town. 
How many years does the town need to see its population greater or equal to p = 1200 inhabitants?
At the end of the first year there will be: 
1000 + 1000 * 0.02 + 50 => 1070 inhabitants
At the end of the 2nd year there will be: 
1070 + 1070 * 0.02 + 50 => 1141 inhabitants (** number of inhabitants is an integer **)
At the end of the 3rd year there will be:
1141 + 1141 * 0.02 + 50 => 1213
It will need 3 entire years.
More generally given parameters:
p0, percent, aug (inhabitants coming or leaving each year), p (population to surpass)
the function nb_year should return n number of entire years needed to get a population greater or equal to p.
aug is an integer, percent a positive or null floating number, p0 and p are positive integers (> 0)
Examples:
nb_year(1500, 5, 100, 5000) -> 15
nb_year(1500000, 2.5, 10000, 2000000) -> 10
Note:
Don't forget to convert the percent parameter as a percentage in the body of your function: 
if the parameter percent is 2 you have to convert it to 0.02.
-}
-----------------------------------------------------------------------------------------------------------
nbYear :: Int -> Float -> Int -> Int -> Int
nbYear p0 percent aug p = length . takeWhile (< p) $ inhabitants
  where 
    inhabitants = iterate pEndYear p0
    pEndYear pBegin = floor $ fromIntegral pBegin * (1 + percent / 100) + fromIntegral aug





--Code here





-- Exercise 8
-----------------------------------------------------------------------------------------------------------
{- 
You're saying good-bye your best friend , See you next happy year .
Happy Year is the year with only distinct digits , (e.g) 2018
Task
Given a year, Find The next happy year or The closest year You'll see your best friend!alt!alt
Notes
Year Of Course always Positive .
Have no fear , It is guaranteed that the answer exists .
It's not necessary that the year passed to the function is Happy one .
Input Year with in range (1000 ≤ y ≤ 9000)
Input >> Output Examples:
nextHappyYear (7712) ==> return (7801)
Explanation:
As the Next closest year with only distinct digits is 7801 .
nextHappyYear (8989) ==> return (9012)
Explanation:
As the Next closest year with only distinct digits is 9012 .
nextHappyYear (1001) ==> return (1023)
Explanation:
As the Next closest year with only distinct digits is 1023 .
-}
-----------------------------------------------------------------------------------------------------------
--Code here
nextHappyYear :: Int -> Int
nextHappyYear n = if isHappyYear (n+1) then n+1 else nextHappyYear (n+1)
  where isHappyYear y = (\x -> x == nub x) $ show y




-- Exercise 9
-----------------------------------------------------------------------------------------------------------
{- 
You might know some pretty large perfect squares. But what about the NEXT one?
Complete the findNextSquare method that finds the next integral perfect square after the one passed as a parameter. 
Recall that an integral perfect square is an integer n such that sqrt(n) is also an integer.
If the parameter is itself not a perfect square then -1 should be returned. 
You may assume the parameter is non-negative.
Examples:(Input --> Output)
121 --> 144
625 --> 676
114 --> -1 since 114 is not a perfect square
-}
-----------------------------------------------------------------------------------------------------------
--Code here
findNextSquare :: Integer -> Integer
findNextSquare n = if sqrtN ^ 2 == n then (sqrtN + 1) ^ 2 else -1
    where sqrtN = round $ sqrt $ fromInteger n


