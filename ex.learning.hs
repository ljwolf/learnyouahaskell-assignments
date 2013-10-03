import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as DC
data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point | Triangle Point Point Point deriving (Show)

data Person = Person	{ firstName :: String
						, lastName :: String
						, age :: Int
						, height :: Float
						, phoneNumber :: String
						, flavor :: String
						} deriving (Show)
					
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
data Maybe a = Nothing | Just a

data Vector a = Vector a a a deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

doubleMe x = x*2
doubleUs x y = doubleMe x + doubleMe y
doubleSmallStuff x = if x > 100
						then x
						else x*2
--asdf what a nice comment.
asdf = "herp a derp derp"
fdsa = "what a nice day we're having"
dasRacist = [8,5,4,3,5,6,1,5]
rasDacist = [5..9]

boombangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

verbs = ["run", "walk", "juggle", "waddle", "slap", "advertize"]
adverbs = ["quickly", "lazily", "enthusiastically", "sleepily" , "dreamily", "happily"]
--length' xs = sum [1 | _ <- xs]
extractEven xxs = [[x | x <- xs, even x] | xs <- xxs]
triangles = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a]]
rtriangles = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]
rtriangles' = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]

--sayme
sayMe :: (Integral a) => a -> String
sayMe 1 = "ONE!"
sayMe 2 = "TWO!"
sayMe 3 = "THREE!"
sayMe 4 = "FOUR!"
sayMe 5 = "FIVE!"
sayMe x = "Not that big, you stupid git!"

--factorial
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

--advectors
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--fuckery
capital :: String -> String
capital "" = "Empty string, you fuck!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--bmi
--bmiTell :: (RealFloat a) => a -> String
--bmiTell bmi
--	| bmi <= 18.5 = "Lolsoskinny"
--	| bmi <= 25.0  = "normasaurus rex"
--	| bmi <= 30.0 = "fatty!"
--	| otherwise = "Ya dingus!"
--	

--bmi-mod h/w
--bmiTell' :: (RealFloat a) => a -> a -> String
--bmiTell' height weight
--	| bmi <= 18.5 = "Lolsoskinny"
--	| bmi <= 25.0  = "normasaurus rex"
--	| bmi <= 30.0 = "fatty!"
--	| otherwise = "Ya dingus!"
--	where bmi = weight / height ^ 2

--initials
initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

--full bmi
--calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
--calcBMIs xs = [bmi w h | (w, h) <- xs]
--	where bmi weight height = weight / height ^2

--cyl
cylV :: (RealFloat a) => a -> a-> a
cylV r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

--What is Seven, even?
lucky :: (Integral a) => a -> String
lucky 7 = "lucky biatch"
lucky 13 = "yous gonnas die"
lucky x = "Sorry, you're out of luck, pal!"

--length?
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

--sum
summ :: (Num a) => [a] -> a
summ [] = 0
summ (x:xs) = x + summ (xs)

--calcbmis with a let clause
calculateBMIs :: (RealFloat a) => [(a, a)] -> [a]
calculateBMIs xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

--new head'
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

--maxalg
maxalg :: (Ord a) => [a] -> a
maxalg [] = error "max of empty list"
maxalg [x] = x
maxalg (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maxalg xs

--replications!
repalg :: (Num i, Ord i) => i -> a -> [a]
repalg n x
	| n <= 0 	 = []
	| otherwise	 = x:repalg (n-1) x

--take! Take! TAKE!
tt :: (Num i, Ord i) => i -> [a] -> [a]
tt n _
	| n <= 0 = []
tt _ []		 = []
tt n (x:xs)  = x : tt (n-1) xs

--esrever
backwards :: [a] -> [a]
backwards [] = []
backwards (x:xs) = backwards xs ++ [x]

--zipper
zipper :: [a] -> [b] -> [(a, b)]
zipper _ [] = []
zipper [] _ = []
zipper (x:xs) (y:ys) = (x,y):zipper xs ys

--elemele
eleme :: (Eq a) => a -> [a] -> Bool
eleme a [] = False
eleme a (x:xs)
	| a == x 	 = True
	| otherwise = a `eleme` xs

--quicksort!
--quicksort :: (Ord a) => [a] -> [a]
--quicksort [] = []
--quicksort (x:xs) =
	--let smallerSorted = quicksort [a | a <- xs, a <= x]
	  --  biggerSorted = quicksort [a | a <- xs, a > x]
	--in smallerSorted ++ [x] ++ biggerSorted
	
--greedy knapsack
--gnap :: (Num a) => [(a,a)] -> [a]
--gnap [x:xs]

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

--apply twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--apply three times
--applyThree :: ((a -> a) -> a) -> a -> a
--applyThree f x = f (f (f x))

--zipwith implementation
zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys

--flip implementation
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallersorted = quicksort (filter (<=x) xs)
	    biggersorted = quicksort (filter (>x) xs)
	in	smallersorted ++ [x] ++ biggersorted
	
--largest divisible integer under 1000000 divisible by some x.
largestdivisible :: (Integral a) => a
largestdivisible = head(filter p [1000000, 999999..])
    where p x = x `mod` 3829 == 0

-- collatz chain
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain (n `div` 2)
	| odd n = n:chain (n*3 + 1)
--longchains :: Int
--longchains = length (filter isLong (map chain [1..100]))
--where isLong xs = length xs > 15
longchains :: Int
longchains = length (filter (\xs -> length xs > 15)(map chain [1..10]))

--sum with explicit recursion
sumre :: (Num a) => [a] -> a
sumre = foldl (+) 0

--elemf
elemf :: (Eq a) => a -> [a] -> Bool
elemf y ys = foldl (\acc x -> if x == y then True else acc) False ys

--backmap
backmap :: (a -> b) -> [a] -> [b]
backmap f xs = foldr (\x acc -> f x : acc) [] xs

---intersperse '.' "MONKEY"
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
	let	nlen = length needle
	in	foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack)

--caesar cipher
encode :: Int -> String -> String
encode shift msg= 
	let	ords = map DC.ord msg
		shifted = map (+ shift) ords
	in map DC.chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

--findkey
--findKey :: (Eq k)=> k -> [(k,v)] -> Maybe v
--findKey key [] = Nothing
--findKey key ((k,v):xs) = if key == k
--							then Just v
--							else findKey key xs
--findkeyfold :: (Eq k) => k -> [(k,v)] -> Maybe v
--findkeyfold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

--GEOMETRY!!!
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
--surface (Triangle (Point x1 y1) (Point x2 y3) (Point x3 y3)) = (sqrt $ (a^2 + b^2 + c^2)^2 - 2 * (a^4 + b^4 + c^4)) * (0.25)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectmult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectmult` m = Vector (i*m) (j*m) (k*m)

scalarmult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarmult` (Vector l m n) = (i*1) + (j*m) + (k*n)
