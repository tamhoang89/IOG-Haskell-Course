import Data.List ( nub, sort )

-- Exercise 1
-----------------------------------------------------------------------------------------------------------
{- 
Given a list. Let's sort it from min to max

Example:

Given [3, 2, 1, 4, 6, 5, 9, 7, 8], your function should return [1, 2, 3, 4, 5, 6, 7, 8, 9].
-}
-----------------------------------------------------------------------------------------------------------
--Code here
sortList :: Ord a => [a] -> [a]
sortList = sort




-- Exercise 2
-----------------------------------------------------------------------------------------------------------
{- 
Given a two-dimensional array of integers, 
return the flattened version of the array with all the integers in the sorted (ascending) order.

Example:

Given [[3, 2, 1], [4, 6, 5], [], [9, 7, 8]], your function should return [1, 2, 3, 4, 5, 6, 7, 8, 9].
-}
-----------------------------------------------------------------------------------------------------------
flatAndSort :: [[Int]] -> [Int]
flatAndSort = sort . concat
--Code here


-- Exercise 3
-----------------------------------------------------------------------------------------------------------
{- 
Given a list of digits, return the smallest number that could be formed from these digits, 
using the digits only once (ignore duplicates).

Notes:
Only positive integers will be passed to the function (> 0 ), no negatives or zeros.
Input >> Output Examples
minValue ({1, 3, 1})  ==> return (13)
Explanation:
(13) is the minimum number could be formed from {1, 3, 1} , Without duplications

minValue({5, 7, 5, 9, 7})  ==> return (579)
Explanation:
(579) is the minimum number could be formed from {5, 7, 5, 9, 7} , Without duplications

minValue({1, 9, 3, 1, 7, 4, 6, 6, 7}) return  ==> (134679)
Explanation:
(134679) is the minimum number could be formed from {1, 9, 3, 1, 7, 4, 6, 6, 7} , Without duplications
-}
-----------------------------------------------------------------------------------------------------------
--Code here
-- Từ danh sách digits, loại bỏ duplicate rồi sort theo thứ tự tăng dần
-- Ghép chúng lại với nhau thu đc số cần tìm

minValue :: [Int] -> Int
minValue digits = foldl ((+) . (*10)) 0 sortedDigits
    where sortedDigits = sort . nub $ digits

minValue' :: [Int] -> Int
minValue' = read . concatMap show . sort . nub 

-- Exercise 4
-----------------------------------------------------------------------------------------------------------
{-
Given an array/list [] of integers , Find the product of the k maximal numbers

Notes
Array/list size is at least 3 .

Array/list's numbers Will be mixture of positives , negatives and zeros

Repetition of numbers in the array/list could occur.

Input >> Output Examples

maxProduct ({4, 3, 5}, 2) ==>  return (20)
Explanation:
Since the size (k) equal 2 , then the subsequence of size 2 whose gives product of maxima is 5 * 4 = 20 .

maxProduct ({8, 10 , 9, 7}, 3) ==>  return (720)
Explanation:
Since the size (k) equal 3 , then the subsequence of size 3 whose gives product of maxima is  8 * 9 * 10 = 720 .

maxProduct ({10, 8, 3, 2, 1, 4, 10}, 5) ==> return (9600)
Explanation:
Since the size (k) equal 5 , then the subsequence of size 5 whose gives product of maxima is  10 * 10 * 8 * 4 * 3 = 9600 .

maxProduct ({ -4, -27, -15, -6, -1}, 2) ==> return (4)
Explanation:
Since the size (k) equal 2 , then the subsequence of size 2 whose gives product of maxima is  -4 * -1 = 4 .

maxProduct ({10, 3, -1, -27} , 3)  return (-30)
Explanation:
Since the size (k) equal 3 , then the subsequence of size 3 whose gives product of maxima is 10 * 3 * -1 = -30 .
-}
-----------------------------------------------------------------------------------------------------------
--Code here
maxProduct :: [Int] -> Int -> Int
maxProduct numbers n = product . take n . reverse . sort $ numbers

