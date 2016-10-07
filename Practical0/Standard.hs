module Standard

where

head' :: [a] -> a
head' [] = error "no head"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

init' :: [a] -> [a]
init' [] = []
init' (x:xs) = init' xs

last' :: [a] -> a
last' [] = []
last' (x:[]) = x
last' (x:xs) = last' xs

(!-!) :: [a] -> Int ->  a
n !-! xs = head (take n xs)

repeat' :: a -> [a]
repeat' x = x: repeat' x

replicate' :: a -> Int -> [a]
replicate' n x = take n (repeat x)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

reverse' :: [a] -> [a]
reverse' =  foldl (flip (:)) [] 

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 xs = []
take' n (x:xs) = x: take' (n -1) xs

drop' :: [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n -1) xs

(+-+) :: [a] -> [a] -> [a]
ys +-+ [] = ys
(y:ys) +-+ xs = y:(ys +-+ xs)

concat' :: [[a] ] -> [a]
concat' xs = foldr (++) [] xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' _ [] = []
splitAt' x xs =  (x: xs', xs'')
	where 
        (xs', xs'') = splitAt' (x-1) xs

elem' :: a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) | n == x = True
                        | otherwise = elem' n xs

null' :: [a] -> Bool
null' [] = True
null' x = False 

takeWhile' :: (a-> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x: takeWhile' p xs
                    |otherwise = []

dropWhile' :: (a-> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = xs

-- Useful for using in high order function such as fold
id' :: a -> a
id' x = x

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n -1)

not' :: Bool -> Bool
not' True = False
not' False = True

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = y x

even' :: Int -> Bool 
even' x | mod x 2 == 0 = True
        | otherwise = False

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x: map' f xs

iterate' :: (a -> a) -> a -> [a]
iterate' f n = n : iterate' f (f n)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = x 'f' foldr' f e xs  

foldl1' :: (a -> b -> a) -> a -> [b] -> a
foldl1' f e [] = e
foldl1' f e (x:xs) = foldl1' f (f e x) xs

strict :: (a -> b) -> a -> b
strict f x = x `seq` f x 

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f e [] = []
foldl' f e (x:xs) = strict (foldl' f) (f e x) xs

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0 

product' :: Num a => [a] -> a
product' = foldl' (*) 1

and' :: [Bool] -> Bool
and' = foldr (&&) True

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x | p x = x
             | otherwise = until p f (f x)

enumFrom' :: Num a => a -> [a]
enumFrom' x = x: enumFrom' (x + 1)

enumFromTo' :: Num a => a -> a -> [a]
enumFromTo' x n | x < n = x: enumFromTo (x + 1) n
                | otherwise = []

-- Mergesort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x: merge xs (y:ys)
                    | otherwise = y: merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort xs | length <= 1 = xs
         | otherwise = (msort ys) `merge` (msort zs)
	where 
        ys = take half xs
        zs = drop half xs
        half = length xs / 2

-- Put a 'var' in between every element of the list
between :: a -> [a] -> [[a] ]
between n [] = [[]]
between n (x:xs) = (n:x:xs) : map (n:) (between n xs)

lookUp :: Eq a => a -> [(a,b)] -> Maybe b
lookUp k [] = Nothing
lookUp k ((x, y), zs) | k == x = Just y
                      | otherwise = lookUp k zs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\a rest -> if p a then  a: rest else rest) [] xs

inits :: [a] -> [[a] ] 
inits [] = [[] ]
inits (x:xs) = [] : map (x:) (inits xs)

tails :: [a] -> [[a] ]
tails [] = [[] ]
tails (x:xs) = (x:xs) : tails xs

subs :: [a] -> [[a] ]
subs [] = [[ ]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

segs :: [a] -> [[a ]]
segs [] = [[ ]]
segs (x:xs) = segs xs ++ map (x:) (inits xs) -- inefficient