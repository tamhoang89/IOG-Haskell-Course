{-
**************************** IMPORTANT ****************************
Solve this homework after completing and checking the "Maze" one.
*******************************************************************
We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.
It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.
So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).
The data types and functions are pretty much the same, with a few caveats:
- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.
Using GHCi, like the "Maze" game, this game should look like this:
*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

data Move = GoLeft | GoForward | GoRight

data Forest a = Exit | Go { staNeed :: a,
                            left    :: Forest a,
                            forward :: Forest a, 
                            right   :: Forest a } 
                deriving Show

move :: Num a => (a, Forest a) -> Move -> (a, Forest a)
move (sta, Exit) _ = (sta, Exit)
move (sta, forest) GoLeft = (sta - staNeed forest, left forest)
move (sta, forest) GoForward = (sta - staNeed forest, forward forest)
move (sta, forest) GoRight = (sta - staNeed forest, right forest)

testForest = 
    Go { 
        staNeed = 3,
        left    = Go {
                    staNeed = 4,
                    left    = Exit,
                    forward = Exit,
                    right   = Exit },
        forward = Go {
                    staNeed = 5,
                    left    = Exit,
                    forward = Go {
                                staNeed = 4,
                                left    = Exit,
                                forward = Exit,
                                right   = Exit },
                    right   = Go {
                                staNeed = 6,
                                left    = Exit,
                                forward = Exit,
                                right   = Exit }},
        right   = Exit
    }
--       ____
--      | 4  |___
--   ___| 5   _6_|
--  |_4___3^_|

resolveForest :: (Num a, Ord a, Show a) => a -> Forest a -> [Move] -> String
resolveForest sta forest mvs = showCurrentChoice $ foldl move (sta, forest) mvs

showCurrentChoice :: (Num a, Ord a, Show a) => (a, Forest a) -> String
showCurrentChoice (sta, forest) 
    | sta <= 0 = "You ran out of stamina and died -.-!"
showCurrentChoice (_, Exit) = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice (sta, _) = "You have " ++ show sta ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."