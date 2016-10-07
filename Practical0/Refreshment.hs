module Refreshment where

-- Comparisons:
-- &&, ||, not, ==, /=, and, or
exampleAnd :: Bool
exampleAnd = True && and [True, True]

-- String and Chars
exampleTakeElement :: String -> Char
exampleTakeElement x = if (length x > 6) then x!!6 else ' '

-- if and Guards
exampleIf :: Int -> Int
exampleIf x = if x > 100 then x else x + 2

exampleGuard :: Int -> Bool
exampleGuard x | x > 3 = True
               | otherwise = False

-- where and guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
        | bmi <= 18 = "skinny bitch"
        | otherwise = "fat whore"
        where bmi = weight / height ^ 2

-- let .. in
-- let <bindings> in <expression>
letExample :: Int
letExample = 4 * (let a = 9 in a + 1) + 2

letExample2 :: (Int, Int, Int)
letExample2 = let square x = x * x in (square 5, square 3, square 2)

-- case expressions
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists"
                      (x:_) -> x

-- Lists               
exampleList :: [Int]
exampleList = 5:[1..10] ++ [12, 14..20]

exampleListsofLists :: [[Int]]
exampleListsofLists = [[1,2,3], [5,6,7]]

-- Priority (succ x) + (max y z) + 1
examplePriority :: Int -> Int -> Int -> Int
examplePriority x y z = succ x + max y z + 1

-- Standard functions
-- head, tail, last, init, length, reverse, take, drop, elem
exampleStandard :: [a] -> a
exampleStandard x = head (tail x)

-- List comprehensions
exampleListcomprehension :: [Int] -> [Int]
exampleListcomprehension xs = [x*2 | x <- xs, x*2 >= 6]

exampleListcomprehensions2 :: [Int]
exampleListcomprehensions2 = [x * y | x <- [1,2,3], y <- [10, 20, 30]] -- [10, 20, 30, 40, 60, 80, 30, 60, 90]

removeNonUpperCase :: String -> String
removeNonUpperCase xs = [c | c <- xs, c `elem` ['A'..'Z']]

-- Tuples
-- tuple function: zip
exampleTuple :: (String, Int)
exampleTuple =  ("hallo", 5)

-- curried functions
maxWithFour :: Int -> Int
maxWithFour x = max 4 x -- partial applied function

applyTwice :: (a-> a) -> a -> a
applyTwice f x = f (f x)

-- high order functions
-- map, filter, foldl, foldr, foldl'
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- lambdas
--exampleLambda = map (\(a,b) -> 

-- foldr example
sum :: Num a => [a] -> a
sum xs = foldr(+) 0 xs

-- Types
-- example types: Int, Intger, Float, Double, Bool, Char
-- example typeclasses: Eq, Num, Ord, Show, Read, Enum, Bounded
exampleRead :: Int
exampleRead = (read "5" :: Int) * 3

exampleRead2 :: Float
exampleRead2 = read "8" + 4.0 -- works for other types as well

exampleShow :: String
exampleShow = show 3 -- "3"

-- Create Types
type Point = (Int, Int)

-- Pattern Matching
lucky :: Int -> String
lucky 7 = "Lucky!"
lucky 6 = "Almost!"
lucky x = "no luck today.."

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1)(x2, y2) = (x1 + x2, y1 + y2)