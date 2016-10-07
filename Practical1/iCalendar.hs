module Calendar 
(
    readCalendar,
    eventCount,
    eventMatch,
    eventNoOverlap,
    eventTime,
    ppMonth
)

where
-- Main module which contains functions for getting information from a 
-- Calendar datatype and drawing a calendar in the form of a string

import ParseLib.Abstract
import Text.PrettyPrint -- The one made by HughesPJ
import Types
import Parser
import Printer
import System.IO
import Instances

-- Used for testing purposes only and does not contain function needed for the application
import Testing 

-- readCalendar reads a file and creates a Maybe Calendar from it
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar x = do
			o <- openFile x ReadMode
			i <- hSetNewlineMode o noNewlineTranslation
			h <- hGetContents o
			return $ run parseCalendar h
            
-- resultFromString parses the parser on a given list of a's and returns only the result b
resultFromString :: Parser a b -> [a] -> b           
resultFromString parser string = fst $ head $ (parse parser string)
            
-- eventCount returns the amount of events in a Calendar
eventCount :: Calendar -> Int
eventCount (Calendar a b) = length b

-- eventMatch returns all events in a Calendar which occur during a given DateTime
eventMatch :: Calendar -> DateTime -> [Event]
eventMatch (Calendar a b) t@(DateTime d e f) = results b t
		where
            -- The function results goes through a list of events and returns all events which 
            -- occur during the given DateTime, or in other words when the given DateTime is 
            -- greater than the event's start time and lower then the the event's end time.
			results [] _ = []
			results ((c@(Event props)) : xs) t | start props t && end props t = c: results xs t 
                                             | otherwise = results xs t
                                             
            -- Check if there is a property Dtstart, and if so, check if it is lower than the given DateTime
			start [] _ = False
			start (Dtstart x :xs) datetime = x < datetime 
            
            -- If the current property is not a Dtstart go through with the rest of the properties
			start (x : xs) datetime = start xs datetime 
           
			end [] _ = False
			end (Dtend x :xs) datetime = datetime < x 
			end (x:xs) datetime = end xs datetime

-- The not so efficient eventNoOverlap returns True if there are no events overlapping, 
-- otherwise it returns False. It compares each event with all other events and checks 
-- if Events start when other events are not finished yet.
eventNoOverlap :: Calendar -> Bool
eventNoOverlap (Calendar a events) = and (compareAll (makeList events))
		where
            -- makeList makes a list of TimeSpans and filters all "00000101T000000" DateTimes
            makeList e = filterTimes (listOfTimes e)  
            
            -- compareToEach checks if an element e overlaps with all other elements of the list
            compareToEach _ [] = True
            compareToEach e (x:xs) | (fst e > fst x) && (fst e < snd x) = False -- Event e is partly 'consumed' in Event x and therefore overlaps
                                   | (fst x > fst e) && (fst x < snd e) = False -- Event x is partly 'consumed' in Event e and therefore overlaps
                                   | (fst e == fst x) = False -- Event e and Event x have the same start time and therefore overlaps
                                   | (snd e == snd x) = False -- Event e and Event x have the same end time and therefore overlaps
                                   | otherwise = compareToEach e xs
                                   
            compareAll [] = [True] -- All elements are checked and non overlap, so return True                    
            compareAll (x:xs) = compareToEach x xs : compareAll xs

-- Filters all "00000101T000000" DateTimes out of the given list     
filterTimes :: [TimeSpan] -> [TimeSpan]
filterTimes [] = []
filterTimes (x:xs) | fst x  == resultFromString parseDateTime "00000101T000000" = filterTimes xs 
                   | snd x  == resultFromString parseDateTime "00000101T000000" = filterTimes xs 
                   | otherwise = x : filterTimes xs

-- Create TimeSpans from a list of Events                   
listOfTimes :: [Event] -> [TimeSpan]     
listOfTimes [] = []              
listOfTimes (event@(Event props) : events) = (getStart props, getEnd props) : listOfTimes events    
        where
            -- Return non interesting DateTime when the list of properties is empty
            getStart [] = resultFromString parseDateTime "00000101T000000"
            getStart (Dtstart x : xs) = x
            getStart (x : xs) = getStart xs
            
            getEnd [] = resultFromString parseDateTime "00000101T000000"
            getEnd (Dtend x : xs) = x
            getEnd (x : xs) = getEnd xs


-- eventTime calculates the total time in seconds for events with a given summary					
eventTime (Calendar a b) x = checkWhichEvents b x
		where
            checkWhichEvents [] _ = []
            checkWhichEvents (event@(Event props) : events) x | (countProps props x) = event : (checkWhichEvents events x)
                                                         | otherwise = checkWhichEvents events x
            countProps (Summary s : xs) x | s == x = True
                                          | otherwise = False
            countProps (y: xs) x = countProps xs x
            giveTimespans events = filterTimes (listOfTimes events)
            calculateTotalTime [] = 0
            calculateTotalTime (x:xs) = 1
            calculateDate date@(DateTime (Date a (Month b1 b2) (Day c1 c2)) d e)
                          date2@(DateTime (Date a2 (Month b3 b4) (Day c3 c4)) d2 e2) = 
                          
                          -- From here on unfinished..
                                (makeIntFromDate a - makeIntFromDate a2) * 365 * 24 * 60 * 60 +
                                (makeInt b1 b2 - makeInt b3 b4) * 10
            makeIntFromDate (Year a b c d) = 0	

---------- DRAWING FRAMEWORK ----------
-- The ppMonth draws a calendar and shows the month and the amount of events in that 
-- month on top. It only shows one event per day, so it just shows the first one it finds
-- furthermore, for events it only shows the start and end time.
----------------------------------------
-- Example used to check if the printing is correct
example :: IO ()
example = putStr $ ppMonth 11 2013 ec -- ec is declared in Testing.hs

ppMonth :: Int -> Int -> Calendar -> String
ppMonth month year calendar = render $ drawCalendar calendar year month

-- drawCalendar is a very BIG function which step by step draws the Calendar
-- It works by first calculating all events for a given year and month and saving 
-- it as a list of tuples with the days (as strings) and the corresponding events. 
-- It draws the calendar row by row and checking for each day if there is a matching 
-- tuple. It stops at the first founded match. And then draws the name (Description or
-- Summary) and start and end time (even if they are not on the same day). 
drawCalendar :: Calendar -> Int -> Int -> Doc
drawCalendar calendar year month  = drawHeader month year calendar <> 
                                drawRow 9 calendar year month 1 <> 
                                drawRowc calendar year month 8 <>
                                drawRow 8 calendar year month 15<> 
                                drawRow 8 calendar year month 22 <> 
                                drawRoof <>
                                drawTill31 8 calendar year month 29<> 
                                drawRoof
        where
            -- Draw the top 'panel' with the month, year and event amount          
            drawHeader :: Int -> Int -> Calendar -> Doc
            drawHeader month year calendar = drawSuperRoof <> 
                                        text "                       " <>
                                        drawMonth month <> 
                                        text " " <>
                                        int year <>
                                        text ", event amount:" <> 
                                        text (show (length $ getAllEvents calendar (show year) 
                                        (returnMonth))) <> 
                                        text "\r\n"
                                        
            -- Month should always be represented as a two character string
            returnMonth | length (show month) == 1 = "0" ++ show month
                        | otherwise = show month      
                                              
            drawMonth month  | month == 1 = text "January"
                             | month == 2 = text "February"
                             | month == 3 = text "March"
                             | month == 4 = text "April"
                             | month == 5 = text "May"
                             | month == 6 = text "June"
                             | month == 7 = text "July"
                             | month == 8 = text "August"
                             | month == 9 = text "September"
                             | month == 10 = text "October"
                             | month == 11 = text "November"
                             | month == 12 = text "December"
                             | otherwise = text "No month"
            
            -- drawRow is responsible for drawing each row of panels
            drawRow l calendar year month day = drawRoof <> drawContent l calendar year month day         
            drawRowc calendar year month day = drawRoof <> drawContentb 9 calendar year month day
            
            -- The SuperRoof, Roof and Line which draw the borders of the events
            drawSuperRoof = text "|----------------------------------------------------------------------------|\r\n"
            drawRoof = text "|----------|----------|----------|----------|----------|----------|----------|\r\n"
            drawLine = text "|          |          |          |          |          |          |          |\r\n"
                        
            -- drawContent and drawContentb draw the inside of each box, so the day number and
            -- if there is an event, also the name (only first 9 characters) and the time of the event
            drawContent l c year month number = text "|" <> int number   <> text (replicate l ' ') <> 
                                  text "|" <> int (number + 1) <> text (replicate l ' ') <>  
                                  text "|" <> int (number + 2) <> text (replicate l ' ') <> 
                                  text "|" <> int (number + 3) <> text (replicate l ' ') <> 
                                  text "|" <> int (number + 4) <> text (replicate l ' ') <> 
                                  text "|" <> int (number + 5) <> text (replicate l ' ') <>  
                                  text "|" <> int (number + 6) <> text (replicate l ' ') <> text "|\r\n" <>
                                  drawDescription c year month number <> drawDate c year month number <> drawLine <> drawLine <> drawLine
            
            -- drawContentb is needed because from day 10 on the amount of spaces is reduced by 1
            drawContentb l c year month number = text "|" <> int number   <> text (replicate l ' ') <> 
                                  text "|" <> int (number + 1) <> text (replicate l ' ') <>  
                                  text "|" <> int (number + 2) <> text (replicate (l - 1) ' ') <> 
                                  text "|" <> int (number + 3) <> text (replicate (l - 1) ' ') <> 
                                  text "|" <> int (number + 4) <> text (replicate (l - 1) ' ') <> 
                                  text "|" <> int (number + 5) <> text (replicate (l - 1) ' ') <>  
                                  text "|" <> int (number + 6) <> text (replicate (l - 1) ' ') <> text "|\r\n" <>
                                  drawDescription c year month number <> drawDate c year month number <> drawLine <> drawLine <> drawLine
            
            -- The calendar draws all days until 31, so the boxes after should have extra spaces
            -- for the layout to stay clean.
            drawTill31 l c year month number =        text "|" <> int 29      <> text (replicate l ' ') <> 
                                  text "|" <> int 30      <> text (replicate l ' ') <>  
                                  text "|" <> int 31      <> text (replicate l ' ') <> 
                                  text "|"                <> text (replicate (l + 2) ' ') <> 
                                  text "|"                <> text (replicate (l + 2) ' ') <> 
                                  text "|"                <> text (replicate (l + 2) ' ') <>  
                                  text "|"                <> text (replicate (l + 2) ' ') <> text "|\r\n" <>
                                  drawDescription c year month number<> drawDate c year month number <> drawLine <> drawLine <> drawLine 
            
            -- drawDate draws the start and end time of an event on the calendar
            drawDate calendar year month day = text "|" <> text (drawCurrentDay ge day  )     <> 
                           text " |" <> text (drawCurrentDay ge (day + 1))  <> 
                           text " |" <> text (drawCurrentDay ge (day + 2)) <> 
                           text " |" <> text (drawCurrentDay ge (day + 3)) <> 
                           text " |" <> text (drawCurrentDay ge (day + 4)) <> 
                           text " |" <> text (drawCurrentDay ge (day + 5)) <> 
                           text " |" <> text (drawCurrentDay ge (day + 6)) <> text " |\r\n" 

            ge = getAllEvents calendar (show year) (returnMonth)      
            
            -- drawDescription draws the summary of the event on the calendar
            drawDescription calendar year month day = text "|" <> text (drawCurrentDescription ge day  )     <> 
                           text " |" <> text (drawCurrentDescription ge (day + 1))  <> 
                           text " |" <> text (drawCurrentDescription ge (day + 2)) <> 
                           text " |" <> text (drawCurrentDescription ge (day + 3)) <> 
                           text " |" <> text (drawCurrentDescription ge (day + 4)) <> 
                           text " |" <> text (drawCurrentDescription ge (day + 5)) <> 
                           text " |" <> text (drawCurrentDescription ge (day + 6)) <> text " |\r\n" 
            
            -- drawCurrentDay expects a list of tuples each consisting of a day and the event happening on that day            
            drawCurrentDay :: [(String, Event)] -> Int -> String            
            drawCurrentDay allEvents day | compareDates allEvents day = getEventDate (getEvent allEvents day)
                                         | otherwise = "         " 
                    where
                        compareDates [] _ = False
                        compareDates ((a,b) : xs) day 
                                    | a == getDay (show day) = True
                                    | otherwise = compareDates xs day
                        getEvent ((a,b) : xs) day
                                    | a == getDay (show day) = b
                                    | otherwise = getEvent xs day
                        getDay day | length day == 1 = "0" ++ day
                                   | otherwise = day
            
            -- drawCurrentDescription
            drawCurrentDescription :: [(String, Event)] -> Int -> String            
            drawCurrentDescription allEvents day | compareDates allEvents day = getEventName (getEvent allEvents day)
                                         | otherwise = "         " 
                    where
                        compareDates [] _ = False
                        compareDates ((a,b) : xs) day 
                                    | a == getDay (show day) = True
                                    | otherwise = compareDates xs day
                        getEvent ((a,b) : xs) day
                                    | a == getDay (show day) = b
                                    | otherwise = getEvent xs day
                        getDay day | length day == 1 = "0" ++ day
                                   | otherwise = day  
                                   
            --- GET INFORMATION FROM CALENDAR ---
            
            -- getEventName gives the first nine characters of the description, and if empty
            -- will try to get the first nine characters of the summary. 
            getEventName :: Event -> String
            getEventName b = results b
                    where 
                        results (Event props) = description props props
                        
                        description [] p = summary p
                        description (Description x:xs) _ = take 9 x
                        description (x : xs) p = description xs p
                        
                        summary [] = "         "
                        summary (Summary x:xs) = take 9 x
                        summary (x : xs) = summary xs 

            -- getEventDate gives the start and end time (without date) of an event in the form of a string            
            getEventDate :: Event -> String    
            getEventDate b = results b
                    where
                        results (Event props) = start props ++ "-" ++ end props                                         
                       
                        start [] = ""
                        start (Dtstart (DateTime (Date y mo day) b 
                              (Time h m s z)):xs) 
                                    = printHour h ++ printMinute m
                        start (x : xs) = start xs 
                        
                        end [] = ""
                        end (Dtend (DateTime a b (Time h m s z)):xs) = printHour h ++ printMinute m
                        end (x : xs) = end xs 
             
            -- getAllEvents gets all the events in a certain month and year
            getAllEvents :: Calendar -> String -> String -> [(String, Event)] 
            getAllEvents (Calendar a b) year month = results b year month
                    where 
                        results [] _ _ = []
                        results (e@(Event props): xs) year month | getDate props year month = (printDay (getDateDay props) ,e) : (results xs year month)
                                                                 | otherwise = results xs year month

                                                              
                        getDate [] _ _= False
                        getDate (Dtstart 
                                    (DateTime (Date y mo day) b c): xs) 
                                    year month 
                                        | printYear y == year && printMonth mo == month = True
                                        | otherwise = False            
                        getDate (x:xs) year month = getDate xs year month
                        getDateDay (Dtstart (DateTime (Date y mo day) b c): xs) = day           
                        getDateDay (x:xs) = getDateDay xs            
