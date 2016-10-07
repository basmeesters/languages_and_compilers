module FunctorTutorial where

import Prelude hiding (Functor, fmap)

--------- FUNCTORS ----------

-- Functor is typeclass for things that can be mapped over
-- List is part of Functor typeclass

-- Class declaration
-- Function from a to b and 'something' which holds an a and returns a 'something' b
class Functor f where
	fmap :: (a-> b) -> f a -> f b 
	
-- List instance of functor
instance Functor [] where
	fmap = map

-- Maybe instance of functor	
instance Functor Maybe where
	fmap f (Just x) = Just (f x)
	fmap f Nothing = Nothing

-- IO instance of functor	
instance Functor IO where
	fmap f action = do
		result <- action
		return (f result)
		
--------- APPLICATIVE FUNCTORS ----------
-- Used to map functions inside a something to the inside of a same something

class (Functor f) => Applicative f where
	pure :: a -> f a -- Take a value and wrap it in an applicative functor
	(<*>) :: f (a -> b) -> f a -> f b
	
instance Applicative Maybe where
	pure = Just
	Nothing <*> _ = Nothing
	(Just f) <*> something = fmap f something

-- Way to map normal functions to the 'insides' of somethings
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- Use newtype when you have a datatype with only one constructor and one value
	-- > Faster, because no wrapping and unwrapping
	-- > 

newtype CharList = CharList {getCharList :: String} deriving (Eq, Show)
newtype Pair b a = Pair {getPair :: (a,b) }
instance Functor (Pair c) where
	fmap f (Pair (x,y)) = Pair (f x, y) -- apply f to first argument only
	
--------- MONOIDS ----------
-- When you have an associative binary function (a -> a -> a) and a value which acts as 
-- an identify with respect to that function

class Monoid m where
	mempty :: m
	mappend :: m ->m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
	
instance Monoid [a] where
	mempty = []
	mappend = (++)

instance Monoid CharList where
	mempty = CharList ""  
	CharList x `mappend` CharList y = CharList (x ++ y)
		
exampleMonoids = getCharList $ CharList "aa" `mappend` CharList "bb"
