import Data.List ( (\\), nub )
import Data.Char ( isUpper )

-- Exercise 1
-----------------------------------------------------------------------------------------------------------
{- Define a function that removes duplicates from an array of numbers and returns it as a result.
The order of the sequence has to stay the same.
-}
-----------------------------------------------------------------------------------------------------------
--Code here
removeDuplicates :: (Num a, Eq a) => [a] -> [a]
removeDuplicates = nub

-- Exercise 2
-----------------------------------------------------------------------------------------------------------
{-
There is an list with some numbers. All numbers are equal except for one. Try to find it!
getUnique [1, 1, 1, 2, 1, 1] -- Result is 2
getUnique [0, 0, 0.55, 0, 0] -- Result is 0.55
It’s guaranteed that list contains at least 3 numbers.
The tests contain some very huge arrays, so think about performance.
-}
-----------------------------------------------------------------------------------------------------------
--Code here
getUnique :: (Num a, Eq a) => [a] -> a
getUnique (x1:x2:x3:xs) = case (x1==x2, x1==x3) of
    (True, False)  -> x3
    (False, True)  -> x2
    (False, False) -> x1
    _              -> head $ filter (/= x1) xs

-- Exercise 3
-----------------------------------------------------------------------------------------------------------
{-
Modify the spacify function so that it returns the given string with spaces inserted between each character.
spacify "hello world" -- returns "h e l l o   w o r l d"
-}
-----------------------------------------------------------------------------------------------------------
--Code here
spacify :: String -> String
spacify xs = if null xs then "" else init . concatMap (: " ") $ xs

-- Exercise 4
-----------------------------------------------------------------------------------------------------------
{-
Write a function that takes an array of consecutive (increasing) letters as input
and that returns the missing letter in the array.
You will always get an valid array.
And it will be always exactly one letter be missing.
The length of the array will always be at least 2.
The array will always contain letters in only one case.
Example:
['a','b','c','d','f'] -> 'e' ['O','Q','R','S'] -> 'P'
["a","b","c","d","f"] -> "e"
["O","Q","R","S"] -> "P"
-}
-----------------------------------------------------------------------------------------------------------

--Code here
findMissingLetter :: String -> Char
findMissingLetter str = head (fullStr \\ str)
   where fullStr = enumFromTo (head str) (last str)

-- Exercise 5
-----------------------------------------------------------------------------------------------------------
{-
Complete the solution so that the function will break up camel casing, using a space between words.
Example
"camelCasing"  =>  "camel Casing"
"identifier"   =>  "identifier"
""             =>  ""
-}
-----------------------------------------------------------------------------------------------------------

--Code here
breakCamelCase :: String -> String
breakCamelCase = foldr (\x acc -> if isUpper x then ' ':x:acc else x:acc) ""


-- Exercise 6
-----------------------------------------------------------------------------------------------------------
{-
Count the number of occurrences of each character and return it as a list of tuples in order of appearance.
For empty output return an empty list.
Example:
orderedCount "abracadabra" == [('a', 5), ('b', 2), ('r', 2), ('c', 1), ('d', 1)]
-}
-----------------------------------------------------------------------------------------------------------
--Code here
orderedCount :: String -> [(Char, Int)]
orderedCount str = foldr (\x -> ((x, countChar x str) :)) [] listChars
    where listChars = nub str
          countChar c = length . filter (==c)


-- Exercise 7
-----------------------------------------------------------------------------------------------------------
{-
Your task is very simple.
Just write a function takes an input string of lowercase letters
and returns true/false depending on whether the string is in alphabetical order or not.
Examples (input -> output)
"kata" -> false ('a' comes after 'k')
"ant" -> true (all characters are in alphabetical order)
Good luck :)
-}
-----------------------------------------------------------------------------------------------------------
--Code here
isAlphabetOrder :: String -> Bool
isAlphabetOrder [] = True
isAlphabetOrder [_] = True
isAlphabetOrder (x:y:ys) = x < y && isAlphabetOrder (y:ys)
