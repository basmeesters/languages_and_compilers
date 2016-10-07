module Instances where
-- This module possesses instances for Ord for Dates, Times and DateTimes so they can be compared with each other, it makes use of the parsers defined in Parser.hs

import Printer
import Parser
import Types

-- DateTimes are compared by comparing dates first and if they are the same check the times
instance Ord DateTime where
    (DateTime a b c) > (DateTime d e f) = (a > d) || (a == d && c > f)
    (DateTime a b c) < (DateTime d e f) = (a < d) || (a == d && c < f)  

-- Dates are compared by making Ints from them, greater Int means later date
instance Ord Date where
	x > y = (read (printDate x) :: Int) > (read (printDate y) :: Int)
	x < y = (read (printDate x) :: Int) < (read (printDate y) :: Int)
 
-- Times are compared by creating total seconds out of them and compare them afterwards  
instance Ord Time where
	(Time a b c d) > (Time e f g h) = (read (printHour a) :: Int) * 60 * 60+ 
									  (read (printMinute b) :: Int) * 60 +
									  (read (printSecond c) :: Int)
									  > 
									  (read (printHour e) :: Int) * 60 * 60 + 
									  (read (printMinute f) :: Int) * 60 +
									  (read (printSecond g) :: Int)
	(Time a b c d) < (Time e f g h) = (read (printHour a) :: Int) * 60 * 60+ 
									  (read (printMinute b) :: Int) * 60+
									  (read (printSecond c) :: Int)
									  < 
									  (read (printHour e) :: Int) * 60 * 60+ 
									  (read (printMinute f) :: Int) * 60+
									  (read (printSecond g) :: Int)