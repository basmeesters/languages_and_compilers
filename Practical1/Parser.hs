module Parser 
(
    parseDateTime,
    parseDigit,
    makeInt,
    parseCheck,
    parseCalendar,
    parseCheckCalendar,
    run
)

where
-- This module is responsible for parsing all the different datatypes defined in Types.hs from strings

import ParseLib.Abstract
import Types

-- parseDateTime Parses DateTimes by parsing each part individually, since most are constructors with Digits the parsing for each one is very similar
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseDatesep <*> parseTime
        where
            parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
            parseDatesep = const T <$> symbol 'T'
            parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond <*> parseTimeUTC
            parseYear = Year <$> parseDigit <*> parseDigit <*> parseDigit <*> parseDigit
            parseMonth = Month <$> parseDigit <*> parseDigit
            parseDay = Day <$> parseDigit <*> parseDigit
            parseHour = Hour <$> parseDigit <*> parseDigit
            parseMinute = Minute <$> parseDigit <*> parseDigit
            parseSecond = Second <$> parseDigit <*> parseDigit
            parseTimeUTC = (const Z <$> symbol 'Z')  <|> const Empty <$> epsilon

-- Digits are parsed from the symbols 0 to 9            
parseDigit :: Parser Char Digit
parseDigit = const Zero     <$> symbol '0' 
         <|> const One      <$> symbol '1'
         <|> const Two      <$> symbol '2'
         <|> const Three    <$> symbol '3'
         <|> const Four     <$> symbol '4'
         <|> const Five     <$> symbol '5'
         <|> const Six      <$> symbol '6'
         <|> const Seven    <$> symbol '7'
         <|> const Eight    <$> symbol '8'
         <|> const Nine     <$> symbol '9'
         
-- MakeInt turns two Digits into a two digit Int                                  
makeInt :: Digit -> Digit -> Int
makeInt x y = (read (show x) :: Int) * 10 + (read (show y) :: Int)

-- parseCheck checks if the parsed DateTime is truly a valid DateTime by checking each datatype individually    
parseCheck :: String -> Maybe Bool   
parseCheck s = checkDateTime <$> run parseDateTime s
    where
        checkDateTime (DateTime a b c) = checkDate a && checkTime c
        checkDate (Date a b c) = checkMonth b && checkDay a b c
        checkMonth (Month a b) | makeInt a b < 12 = True
                               | otherwise = False
        
        -- First check if the day 
        checkDay (Year e f g h) (Month c d) (Day a b) | day < 29 = True
                                                      | (day < 31 && is30 (month )) = True
                                                      | (day < 32 && is31 (month )) = True
                                                      | (day == 29 && (makeInt g h `mod` 4 == 0)) = True
                                                      | otherwise = False
                                        where
                                            day = makeInt a b
                                            month  = makeInt c d
                                            is30 m = any (==m) [1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
                                            is31 m = any (==m) [1, 3, 5, 7, 8, 10, 12]
                                           
        checkTime (Time a b c _) = checkHour a && checkMinutes b && checkSeconds c                               
        checkHour (Hour a b) | makeInt a b <24 = True
                             | otherwise = False                    
        checkMinutes (Minute a b) | makeInt a b < 60 = True
                                   | otherwise = False
        checkSeconds :: Second -> Bool                           
        checkSeconds (Second a b) | makeInt a b < 60 = True
                                   | otherwise = False
                                   

-- parseCalendar parses, again, a Calendar by parsing all part individually
parseCalendar :: Parser Char Calendar
parseCalendar = Calendar <$ token "BEGIN:VCALENDAR" <* parseCrlf <*> 
                             parseCalprops <*> parseEvents <* 
                             token "END:VCALENDAR" <* parseCrlf <* eof
        where 
            parseCrlf = token "\r\n"
            
            -- parseText recognizes any symbol until a Crlf is recognized, an empty string is returned in that case and the parsing of the text is stopped
            parseText = ("" <$ token "\r\n") <<|> (:) <$> anySymbol <*> parseText
            parseCalprops = many (parseProdid <|> parseVersion)
            parseProdid = Prodid <$ token "PRODID:"  <*> parseText
            parseVersion = Version <$ token "VERSION:2.0" <* parseCrlf
            parseEvents = many (Event <$>(Event <$ token "BEGIN:VEVENT" <* parseCrlf *> parseEventprops 
                                     <* token "END:VEVENT" <* parseCrlf))	
                                     
            -- parseEventprops parses zero or more properties an event can posses                                
            parseEventprops = many (parseDtstamp 
                          <|> parseDtstart
                          <|> parseDtend
                          <|> parseUid
                          <|> parseDescription
                          <|> parseSummary
                          <|> parseLocation)              
            parseDtstamp = Dtstamp <$> (Dtstamp <$ token "DTSTAMP:" *> parseDateTime <* parseCrlf)
            parseDtstart = Dtstart <$> (Dtstart <$ token "DTSTART:" *> parseDateTime <* parseCrlf)
            parseDtend = Dtend <$> (Dtend <$ token "DTEND:" *> parseDateTime <* parseCrlf)
            parseUid = Uid <$> (Uid <$ token "UID:" *> parseText)
            parseDescription = Description <$> (Description <$ token "DESCRIPTION:" *> parseText)
            parseSummary = Summary <$> (Summary <$ token "SUMMARY:" *> parseText)
            parseLocation = Location <$> (Location <$ token "LOCATION:" *> parseText)

-- parseCheckCalendar checks if the Calendar is valid            
parseCheckCalendar :: Calendar -> Bool            
parseCheckCalendar (Calendar a b) = checkCalprops a && and (checkEvents b)
    where
        checkCalprops props | length props == 2 && bothOnce props = True
                            | otherwise = False
                            
        -- Check if both Calendar properties are present exactly once                   
        bothOnce (Version : (Prodid x) : []) = True
        bothOnce (Prodid x : Version : []) = True
        bothOnce _ = False
        
        -- Check if all required properties appear at least once
        checkEvents [] = [True]
        checkEvents ((Event props) : xs) = checkStart props props : checkEvents xs
        
        checkStart [] _ = False
        checkStart (Dtstart x : xs) props = checkEnd props props
        checkStart (x:xs) props = checkStart xs props
        checkEnd [] _ = False
        checkEnd (Dtend x: xs) props = checkStamp props props
        checkEnd (x:xs) props = checkEnd xs props 
        checkStamp [] _ = False
        checkStamp (Dtstamp x: xs) props = checkUid props props
        checkStamp(x:xs) props = checkStamp xs props
        checkUid [] _ = False
        checkUid (Uid x: xs) props = True
        checkUid(x:xs) props = checkStamp xs props
        
        -- TODO.. check if properties appear not more than once
        -- checkIfNotDouble prop [] = True
        -- checkIfNotDouble prop (x:xs) = checkIfNotDouble prop xs
        
-- run parses a list of a's (mostly a string) and returns if the parse succeeded in the form of a Maybe b
run :: Parser a b -> [a] -> Maybe b
run _ []  = Nothing
run p xs = check (parse p xs)
    where
        -- If the parse is successful and the rest string is empty return Just result, otherwise return Nothing
        check (x:xs) | emptyList (snd x) = Just (fst x)
                     | otherwise = check xs
        check _ = Nothing
        emptyList [] = True
        emptyList _ = False
