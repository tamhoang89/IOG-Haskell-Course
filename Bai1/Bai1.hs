-- Question 1
-- Write a multiline comment below.
{-
comment
in
multiline
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
multiplyBy3 x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
circleArea r = r ^ 2 * pi

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder.
cylVolume r h = circleArea r * h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
checkCylVol r h = cylVolume r h >= 42