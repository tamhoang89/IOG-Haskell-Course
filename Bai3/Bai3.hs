-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly.
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
checkConsumption :: Double -> Double -> Double -> String
checkConsumption p h maxAllow
  | monthlyUsage == maxAllow = "OK but enough, you are hit the limit"
  | monthlyUsage > maxAllow = "Consume too much"
  | otherwise = "It's very good usage"
  where
    monthlyUsage = p * h * 30

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
checkConsumption' :: Double -> Double -> Double -> String
checkConsumption' p h maxAllow
  | monthlyUsage == maxAllow = "OK but enough, you are hit the limit"
  | monthlyUsage > maxAllow = "Consume too much. Excess: " ++ show (monthlyUsage - maxAllow) ++ "kWh"
  | otherwise = "It's very good usage. Savings: " ++ show (maxAllow - monthlyUsage) ++ "kWh"
  where
    monthlyUsage = p * h * 30

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
shenPriceInUsd :: Double -> Double -> Double -> Double -> Double
shenPriceInUsd nDjed nShen nReverseAda adaPriceInUsd =
  let reverseInUsd = nReverseAda * adaPriceInUsd
      libiatyInUsd = nDjed
      equityInUsd = reverseInUsd - libiatyInUsd
  in  equityInUsd / nShen

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.
f :: Double -> Double -> String
f a b
  |a == 0 && b == 0 = "0/0 be undefined"
  |a == 0 || b == 0 = "Result = 0"
  |a/b <= 1 = "Result: a/b = " ++ show (a/b)
  |otherwise = "Result: b/a = " ++ show (b/a)


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
f2 :: Double -> Double -> String
f2 a b
  |b == 0 = "Cannot divide by 0"
  |True = 
    let sqA = square a
         where square x = x*x 
    in show (sqA*sqB + sqA/sqB)
         where sqB = let square' x = x^2 
                     in square' b