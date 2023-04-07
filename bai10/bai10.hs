{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

class Container c where
    isEmpty :: c a -> Bool
    contains :: Eq a =>c a -> a -> Bool
    replace :: c a -> b -> c b
    unwrap :: c a -> Maybe a

data Box a = Empty | Has a deriving Show
instance Container Box where
    isEmpty Empty = True
    isEmpty _     = False

    contains (Has x) y = x == y
    contains _ _       = False

    replace _ = Has

    unwrap Empty   = Nothing
    unwrap (Has x) = Just x

data Present t a = EmptyPresent t | PresentFor t a deriving Show
instance Container (Present t) where
    isEmpty (EmptyPresent _) = True
    isEmpty _                = False

    contains (PresentFor _ x) y = x == y
    contains _ _                = False

    replace (EmptyPresent t) = PresentFor t
    replace (PresentFor t _) = PresentFor t

    unwrap (EmptyPresent _) = Nothing
    unwrap (PresentFor _ x) = Just x

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a deriving Show
instance Container (MailedBox t d) where
    isEmpty (EmptyMailBox _ _) = True
    isEmpty _                  = False

    contains (MailBoxTo _ _ x) y  = x == y
    contains _ _                  = False

    replace (EmptyMailBox t d) = MailBoxTo t d
    replace (MailBoxTo t d _)  = MailBoxTo t d

    unwrap (EmptyMailBox _ _) = Nothing
    unwrap (MailBoxTo _ _ x)  = Just x


-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double deriving (Show)
rate = 1.09  -- EUR to USD exchange rate 
instance Eq Salary where
    USD x == USD y = x == y
    EUR x == EUR y = x == y
    USD x == EUR y = x == y * rate
    EUR x == USD y = x * rate == y
instance Ord Salary where
    USD x <= USD y = x <= y
    EUR x <= EUR y = x <= y
    USD x <= EUR y = x <= y * rate
    EUR x <= USD y = x * rate <= y 


data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Show, Eq, Ord)

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Show, Eq)

instance Ord Pokemon where
    compare p1 p2 = pPokeDexNum p1 `compare` pPokeDexNum p2

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6
venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3



-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show, Num)
-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0