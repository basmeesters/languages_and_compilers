module Test where

import Prelude hiding (Left, Right, Nothing)
import ArrowCode as A
import Types as T
import Parser
import Language
import Interpreter
import Driver

import ParseLib.Abstract
import System.IO

----------------------------------------
-- Example programs from the assignment
----------------------------------------
rd = "start -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right, s2. " ++
               "s2 -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right, s3. " ++
               "s3 -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right, s4. " ++
               "s4 -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right."

walkBoard = "start -> mark, go, mark, go, mark, go, mark, bla. " ++
            "bla -> turn left, mark, go, mark, go, mark, go, mark, bla2. " ++
            "bla2 -> turn left, mark, go, mark, go, mark, go, mark. "

-----------------------------------
-- Tests for scanning and parsing
-----------------------------------
-- Empty rule 
test1 = "start ->." 
testParse1 = parseTokens $ scan test1

-- Multiple cmds 
test2 = "start -> go, take, mark, turn left."
testParse2 = parseTokens $ scan test2

-- Multple rules
test3 = "rule1 ->. start -> case left of Lambda -> go, take end."
testParse3 = parseTokens $ scan test3

-- Multiple rules + multiple Alts
test4 = "start ->. rule2 -> case left of Lambda -> go, take end. rule3 -> " ++
             "case right of Debris -> turn right ; Lambda -> go end."
testParse4 = parseTokens $ scan test4

-- Call to rule in Case expression (should return valid Program)
test5 = "start ->. rule2 -> case left of Empty -> go, take, turn left end. rule3 -> " ++
             "case right of Debris -> turn right ; Debris -> go end. rule5 -> rule3."     
testParse5 = parseTokens $ scan test5

-- Call to undefined rule in Case expression (should return non valid Program)
test6 = "start ->. rule2 -> case left of lambda -> go, hallo, take, turn left end. rule3 -> " ++
             "case right of debris -> turn right ; debris -> go end. rule5 -> rule3."     
testParse6 = parseTokens $ scan test6

-- Correct parse but non valid Program (no start)
test7 = scan "rule1 -> hallo. "
testParse7 = parseTokens test7

-- False test
testFalse = "rule1 ->. rule2 -> case left of Lambda -> go, take end.."

testParse x = do
        o <- openFile x ReadMode
        h <- hGetContents o
        return $ parse parseSpace h
        

----------------------------------------------------------
-- Tests for testing the step function with various input
----------------------------------------------------------
{-test = step te testCall -- Change last argument to test other commands

-- test Go, Take, Mark, Nothing, Turn, Case and Call
testGo = ArrowState sp (0,0) Right (Cmds [Go]) 
testTake = ArrowState sp (0,0) Right (Cmds [Take])
testNothing = ArrowState sp (0,0) Right (Cmds [Nothing])
testMark = ArrowState sp (0,0) Right (Cmds [Mark])
testTurn = ArrowState sp (0,0) Left (Cmds [Turn Right])
testCall = ArrowState sp (0,0) Left (Cmds [Cmd "rule2"])
testCase = ArrowState sp (6,4) Right (Cmds [Case Left (Alts [Alt T.Empty (Cmds [Go, Take])])])
  -}  
    
    
-------------------------------------------------------------
-- Example ArrowState, Environment and interactive functions
-------------------------------------------------------------
tas = ArrowState sp (4,3) South (Cmds [])
te = toEnvironment removeDebris
