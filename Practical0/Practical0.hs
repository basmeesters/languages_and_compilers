module Practical0 where

    import Data.Char

-- 1.
    unwords1 :: [String] -> String
    unwords1 [] = []
    unwords1 (x:xs) = x ++ " " ++ unwords xs

    unwords2 :: [String] -> String
    unwords2 xs = foldr (addd) "" (xs)
        where addd x y = if y /= [] then x ++ " " ++ y else x

    word :: String -> String
    word [] = []
    word (x:xs) = if (x /= ' ') then x: word xs  else []

    words1 [] = []
    words1 xs = [b] ++ words1 (drop ( length ( b) + 1) xs)
        where b = words xs

    words2 :: String -> [String]
    words2 = foldr (\x acc -> if x == ' ' || head acc == "" || (head ( head acc)) /= ' '  
                                 then (x : head acc) : tail acc
                                 else [x] : acc) [""]
                                                              
    example = ["hallo", "hoe", "is", "het?"]
    example2 = "Hallo hoe is het?"

-- 2.
    foldrBas :: (a -> b -> b) -> b  -> [a] -> b
    foldrBas f e [] = e
    foldrBas f e (x:xs) = x `f` foldrBas f e xs

    foldlBas :: (a -> b -> a) -> a -> [b] -> a
    foldlBas f e [] = e
    foldlBas f e (x:xs) = foldlBas f (f e x) xs

    -- foldr works from the last element of the list and the initial state and uses the function on it and 
    -- uses the function again on the result and the pre last element and works it way like that to the first 
    -- element. foldl does it the other way around

--3. 
    -- without foldr
    convert :: [Int] -> Int
    convert [] = 0
    convert xs = convert' xs (length xs -1)
        where
            convert' [] _ = 0
            convert' (x:xs) n = (10 ^ n * x) + convert' xs (length xs - 1)
    
    
    -- with foldr
    
    -- String to Int
    toInt :: String -> Int
    toInt [] = 0
    toInt (x:xs) = (ord x - 48) * 10^(length xs) + toInt xs

--4. 
    data Tree a = Bin (Tree a) (Tree a) | Tip a | Empty
    
    information :: Tree a -> [a]
    information (Tip e) = [e]
    information (Bin l r) = information l ++ information r
    
    exampleTree :: Tree Int
    exampleTree = Bin (Bin ( Tip 1) (Tip 2)) (Tip 3)
    
    et :: Tree Int
    et = Bin (Bin (Bin (Tip 1) (Tip 2)) (Tip 3)) (Bin ( Tip 4) (Tip 5))

--5. 
    pack :: Tree Int -> String
    pack (Tip e) = show e
    pack (Bin l r) = "{" ++ pack l ++ "," ++ pack r ++ "}"
    
--6.
    unpack :: String -> Tree Int
    unpack [] = Tip 0
    unpack (x:xs) | x == '{' = Bin (unpack xs) (unpack xs)
                  | x == ',' = unpack xs 
                  | otherwise = Tip (ord x - 48)
                  
    es :: String
    es = "{{{1,2},3},{4,5}}"
    
  
    
    giveIndex (x:xs) | x /= '}'