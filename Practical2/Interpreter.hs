module Interpreter where

import Data.Map as L  
import Control.Monad
import ParseLib.Abstract
import Data.Char hiding (Space)
import ArrowCode
import Tokens
import Parser
import Language
import Types hiding (Empty, Lambda, Asteroid, Boundary, Debris)
import Prelude hiding (Left, Right, Nothing)
 
-- scan is used to scan a string of tokens defined in Tokens.x
scan = alexScanTokens

-- printSpace makes a readable string from the Space
printSpace :: Space -> String
printSpace s = showFirst ++ "\n" ++ (showAll (elems s) maxRow)
    where
        getMax = fst $ findMax s
        showFirst = show getMax
        maxRow = fst getMax
        showAll [] _ = ""
        showAll (x:xs) 0 = (showc x) ++ "\n" ++ showAll xs maxRow
        showAll (x:xs) n = (showc x) ++ showAll xs (n - 1)

        -- Show the right symbol for the correpsonding Contents
        showc :: Contents -> String        
        showc c = showContents c
            where
                showContents Empty = "."  
                showContents Lambda = "\\"
                showContents Debris = "%"
                showContents Asteroid = "0"
                showContents Boundary = "#"
        
       

-- toEnvironment scans the string, then parses it and checks if it is valid and if so
-- creates an Environment from it 
toEnvironment :: String -> Environment
toEnvironment s | check program = createEnv (getRules program) -- Check validity
                | otherwise = error "No valid Program"
    where
        -- Scan and parse the string
        program = parseTokens $ scan s 
        
        -- Get all commands from the program
        getRules (Program xs) = xs
        
        -- Create environments by using the idents and the commands from the program
        createEnv css = L.fromList $ zipWith (\ c d   ->  (c,d)) 
                        (Prelude.map getNames css) 
                        (Prelude.map getCommands css)
        
        getNames (Rule name cmds) = name
        getCommands (Rule name cmds) = cmds

-- step expects an Environment and ArrowState and executes the first command on the 
-- stack by pattern matching it in readStack to the right Command
step :: Environment -> ArrowState -> Step
step env (ArrowState space pos@(a,b) heading stack@(Cmds cs)) 

                    -- Done or fail for lists with zero or one element(s)
                    | length cs == 0 = Done space pos heading
                    | length cs == 1 = translate $ readStack stack
                    
                    -- Ok or Fail
                    | length cs > 1 =  readStack stack
    where
        -- Translate an Ok to a Done or a fail when the stack is empty
        translate (Ok (ArrowState sp ps he (Cmds st))) | length st == 0 = Done sp ps he
        
                                                        -- When the Command was a call make it an Ok again
                                                       | otherwise = Ok (ArrowState sp ps he (Cmds st))
        translate (Fail s) = Fail s
        
        -- readStack pattern matches the right Command and returns a step
        readStack :: Stack -> Step
        readStack (Cmds (Go :xs)) = Ok $ ArrowState space forward heading (Cmds xs)
        readStack (Cmds (Take :xs)) = Ok $ ArrowState pickUp pos heading (Cmds xs)
        readStack (Cmds (Mark :xs)) = Ok $ ArrowState mark pos heading (Cmds xs)
        readStack (Cmds (Nothing :xs)) = Ok $ ArrowState space pos heading (Cmds xs)
        readStack (Cmds (Turn d :xs)) = Ok $ ArrowState space pos (turnArrow d) (Cmds xs)
        
        -- The sensor is sensed to a direction depending on the current heading and checks
        -- if there is an Alt which matches the scan
        readStack (Cmds (Case d ass : xs)) | dir == West && b > 0 = sensor(a,b -1) ass xs
                                           | dir == East && b < x = sensor(a,b + 1) ass xs
                                           | dir == South && a < y = sensor(a + 1,b) ass xs
                                           | dir == North && a > 0 = sensor(a -1,b) ass xs
                                           
                                           -- Sensing outside of the map is automatically a boundary
                                           | otherwise = sensorBoundary ass xs
                                    where 
                                        dir = turnArrow d
        
        -- Check if call exist in environment and if so, add all the Commands to the top of the stack
        readStack (Cmds (Cmd x :xs)) | checkCall x == Cmds [Cmd "notexistent ->."] = Fail "fail"
                                     | otherwise = Ok $ ArrowState space pos heading 
                                                  (Cmds (getC (checkCall x) ++ xs))
        
        
        -- Move 'forward' in the current direction if it is on the board and a valid move
        forward  | heading == West && b > 0 = checkAhead (a, b -1)
                 | heading == East && b < x = checkAhead (a, b + 1)
                 | heading == South && a < y = checkAhead (a + 1, b)
                 | heading == North && a > 0 = checkAhead (a - 1, b)
                 | otherwise = (a,b)
                 
        -- Return the new position if it is a valid move, otherwise return the old position
        checkAhead p   | space!pos == Empty = p
                       | space!pos == Lambda = p
                       | space!pos == Debris = p
                       | otherwise = pos -- old position
        
        -- Maximal amount of columns and rows of the space board
        x = fst $ fst $ max  
        y = snd $ fst $ max 
        max = findMax space
        
        -- Functions to adjust the board
        pickUp = adjust (\x -> Empty) pos space -- Pick up whatever is on your current spot and make it Empty
        mark = adjust (\x -> Lambda) pos space -- Mark the current spot with a Lambda
        
        -- Turn 90 degrees from current heading 
        turnArrow d | heading == West && d == Left = South
                    | heading == West && d == Right = North
                    | heading == South && d == Left = East
                    | heading == South && d == Right = West
                    | heading == East && d == Left = North
                    | heading == East && d == Right = South
                    | heading == North && d == Left = West
                    | heading == North && d == Right = East
                    | otherwise = heading
        
        
        
        -- sensor and sensorboundary check if there is a match between de Contents and the Alts
        -- sensor checks the board en sensorBoundary makes the content automatically a boundary
        sensorBoundary ass xs | matches == [Cmd "notexistent ->."] = Fail "no existing match found"
                              | otherwise = Ok $ ArrowState space pos heading (Cmds (matches ++ xs))
                where
                    matches = sensorBoundaryMatch ass
                    sensorBoundaryMatch (Alts ass) = match Boundary ass
        
        sensor p ass xs | matches == [Cmd "notexistent ->."] = Fail "no existing match found"
                        | otherwise = Ok $ ArrowState space pos heading (Cmds (matches ++ xs))
                where
                    -- Check the value in Space and match it
                    matches = sensorMatch p ass
                    sensorMatch p (Alts ass) = match (space!p) ass
                    
        -- If the Contents and the Pat have the same show (string) they match
        match c (Alt pt (Cmds cs) :xs) | show c == show pt = cs
        
                                        -- When the content is a Underscore it matches all Pats
                                       | pt == Underscore = cs 
                                       
                                       -- Check all other Alts when the current one does not match
                                       | otherwise = match c xs
        match _ [] = [Cmd "notexistent ->."]


        -- Check if the current call exists or return non existing Cmd 
        -- (so rules which are empty don't give a fail)
        checkCall x = findWithDefault (Cmds [Cmd "notexistent ->."]) x env 
        getC (Cmds xs) = xs   
