module Printer 
(
    printDigit,
    showDigits,
    printDateTime,
    printDate,
    printYear,
    printMonth,
    printDay,
    printTime,
    printHour,
    printMinute,
    printSecond,
    printUTC,
    printSep,
    parsePrint,
    printCalendar
)

where
-- This module contains functions for printing the datatypes defined in Types.hs

import ParseLib.Abstract
import Types
import Parser

-- printDigit                                      
printDigit :: Digit -> String
printDigit d = show d

-- print two Digits at once
showDigits :: Digit -> Digit -> String
showDigits a b = printDigit a ++ printDigit b

-- printDateTime makes strings from DateTimes by printing the individual parts
-- Note: The individual printers cannot be set into a where- clause because they are needed again in Instances.hs for comparing DateTimes with each other
printDateTime :: DateTime -> String
printDateTime (DateTime a b c) = printDate a ++ printSep b ++ printTime c

printDate :: Date -> String
printDate (Date a b c) = printYear a ++ printMonth b ++ printDay c

printYear :: Year -> String
printYear (Year a b c d) = showDigits a b ++ showDigits c d 

printMonth :: Month -> String
printMonth (Month a b) = showDigits a b

printDay :: Day -> String 
printDay (Day a b) = showDigits a b

printTime :: Time -> String
printTime (Time a b c d) = printHour a ++ printMinute b ++ printSecond c ++ printUTC d

printHour :: Hour -> String
printHour (Hour a b) = showDigits a b 

printMinute :: Minute -> String
printMinute (Minute a b) = showDigits a b

printSecond :: Second -> String
printSecond (Second a b) = showDigits a b

printUTC :: TimeUTC -> String
printUTC (Empty) = ""
printUTC (Z) = "Z"

printSep :: Datesep -> String
printSep T = "T"

-- parsePrint parses a string to a DateTime and returns the result as a Maybe String
parsePrint :: String -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- printCalendar makes a string from a Calendar by printing each part individually
printCalendar :: Calendar ->  String
printCalendar (Calendar x y) = "BEGIN:VCALENDAR\r\n" ++ 
                               printCalprops x ++ printEvents y ++ 
                               "END:VCALENDAR" 
        where          
            printCalprops [] = "\r\n"
            printCalprops (Prodid x :xs) = "PRODID: " ++ x ++ printCalprops xs 
            printCalprops (Version :xs) = "VERSION:2.0\r\n" ++ printCalprops xs
            printEvents [] = "\n"
            printEvents (Event x :xs) = "BEGIN:VEVENT\r\n" ++ printEventprops x ++ "END:VEVENT\r\n"    
            
            -- Print all event properties until the list is empty
            printEventprops [] = "\n"
            printEventprops (Dtstamp x:xs)= "DTSTAMP: " ++ printDateTime x ++ "\r\n" ++ printEventprops xs
            printEventprops (Uid x:xs)= "UID: " ++ x ++ "\r\n" ++ printEventprops xs
            printEventprops (Dtstart x:xs)= "DTSTART: " ++ printDateTime x ++ "\r\n" ++ printEventprops xs 
            printEventprops (Dtend x:xs)= "DTEND: " ++ printDateTime x ++ "\r\n" ++ printEventprops xs
            printEventprops (Description x:xs)= "DESCRIPTION: " ++ x ++ printEventprops xs
            printEventprops (Summary x:xs)= "SUMMARY: " ++ x ++ printEventprops xs
            printEventprops (Location x:xs)= "LOCATION: " ++ x ++ printEventprops xs

