module Testing 
(
    ec -- Example Calendar
)

where
-- This module is for testing purposes only

import ParseLib.Abstract
import Types
import Parser
import Printer
import Instances

-- Check run function
x = run parseDigit "1"
x2 = run parseDigit "123"

-- Check parsePrint
dt1 = parsePrint "19970610T172345Z"
dt2 = parsePrint "19970715T040000Z"
dt3 = parsePrint "19970715T04000Z" -- Nothing
dt4 = parsePrint "20111012T083945" 
dt5 = parsePrint "20040230T431337Z"

-- Example string
testCalprops = "VERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\n"   
testEvent = "BEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\n"
testEventprops = "UID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\n"
testUid = "UID:19970610T172345Z-AF23B2@example.com\r\n"

-- Example Calendar strings
testCalendar = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T1700" ++
     "00Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
testMultipleEvents = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:NONSGML v1.0//EN\r\n" ++ 
      "BEGIN:VEVENT\r\nUID:19971112T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:20130718T000000Z\r\nDTEND:20130718T000001Z\r\nSUMMARY:Vacation!\r\n" ++
      "END:VEVENT\r\n" ++
      "BEGIN:VEVENT\r\nUID:19971113T172345Z-AF23B2@example.com\r\nDTSTAMP:19980610T172345Z\r\nDTSTART:20131130T230000Z\r\nDTEND:20131130T235959Z\r\nSUMMARY:End birthday month\r\n" ++
      "END:VEVENT\r\n" ++ 
      "BEGIN:VEVENT\r\nUID:19971113T172345Z-AF23B2@example.com\r\nDTSTAMP:19980610T172345Z\r\nDTSTART:20131112T000000Z\r\nDTEND:20131112T235959Z\r\nDESCRIPTION:Birthday!\r\n" ++
      "END:VEVENT\r\n" ++ 
      "BEGIN:VEVENT\r\nUID:19971113T172345Z-AF23B2@example.com\r\nDTSTAMP:19980610T172345Z\r\nDTSTART:20131101T090000Z\r\nDTEND:20131101T103000Z\r\nSUMMARY:Start birthday month\r\n" ++
      "END:VEVENT\r\n" ++ 
      "BEGIN:VEVENT\r\nUID:19971006T172345Z-AF23B2@example.com\r\nDTSTAMP:19990610T172345Z\r\nDTSTART:20131201T170000Z\r\nDTEND:20131201T170000Z\r\nSUMMARY:Deadline T&C\r\n" ++
      "END:VEVENT\r\nEND:VCALENDAR\r\n"
tme = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal/ /NONSGML v1.0//EN\r\n" ++
      "BEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\n" ++ 
      "END:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\n" ++
      "END:VEVENT\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\n" ++
      "END:VEVENT\r\nEND:VCALENDAR\r\n"

tstart = "BEGIN:VCALENDAR\r\n"

testPrint = putStrLn (printCalendar (makeCalendar testCalendar))

-- makeCalendar creates a Calendar from a string
makeCalendar :: String -> Calendar
makeCalendar x = fst $ head $ parse parseCalendar x

-- Example Calendar used for debugging
ec = makeCalendar testMultipleEvents