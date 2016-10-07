module Types where
-- This module posses all types and datatypes iCalendar uses

import ParseLib.Abstract

-- Datatypes for representing a DateTime
data DateTime = DateTime Date Datesep Time deriving (Show, Eq)
data Date = Date Year Month Day deriving (Show, Eq)
data Time = Time Hour Minute Second TimeUTC deriving (Show, Eq)
data Year = Year Digit Digit Digit Digit deriving (Show, Eq)
data Month = Month Digit Digit deriving (Show, Eq)
data Day = Day Digit Digit deriving (Show, Eq)
data Hour = Hour Digit Digit deriving (Show, Eq)
data Minute = Minute Digit Digit deriving (Show, Eq)
data Second = Second Digit Digit deriving (Show, Eq)
data TimeUTC = Empty | Z deriving (Show, Eq)
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine 
data Datesep = T deriving (Show, Eq)

-- Datatypes for representing a Calendar
data Calendar = Calendar [Calprop] [Event] deriving (Show)
data Calprop = Prodid Text| Version deriving (Show)

data Event = Event [Eventprop] deriving (Show)
data Eventprop = Dtstamp DateTime | 
                 Uid Text | 
                 Dtstart DateTime | 
                 Dtend DateTime| 
                 Description Text | 
                 Summary Text | 
                 Location Text deriving (Show)                  

type Crlf = String
type Text = String

-- Type which has keeps a start and an end DateTime which represents the timespan of an event
type TimeSpan = (DateTime, DateTime)

-- Instances to represent and compare Digits
instance Show Digit where
    show Zero   = "0"
    show One    = "1"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"
    show Six    = "6"
    show Seven  = "7"
    show Eight  = "8"
    show Nine   = "9"
    
instance Eq Digit where
	x == y = show x == show y    
 								  