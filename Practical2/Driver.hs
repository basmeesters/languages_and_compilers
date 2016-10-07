module Driver where

import Prelude hiding (Left, Right, Nothing)
import ArrowCode as A
import Types as T
import Parser
import Language
import Interpreter

import ParseLib.Abstract
import System.IO

-- Example from the assignment 
exampleProgram = interactive (toEnvironment removeDebris) (ArrowState sp (4,3) South (Cmds []))
removeDebris = "start -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right, s2. " ++
               "s2 -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right, s3. " ++
               "s3 -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right, s4. " ++
               "s4 -> take, case front of Debris -> go, start, turn right, turn right, " ++  
               "go, turn right, turn right ; _ -> nothing end, turn right."
               
sp = fst $ head $ parse parseSpace testSpace       
testSpace = "(7,7)\n........\n....%...\n..%%%%..\n....%%%.\n" ++
            "...%%%..\n....%.%%\n....%%%%\n........\n"  

-- Instance for Step so the current situation can be shown
instance Show Step where
    show (Done s p h) = 
            "Position: " ++ (show p) ++ 
            "\nHeading: " ++(show h) ++ "\n" ++
            "Stack amount: 0\n" ++    
            printSpace s ++ "\n"
    show (Ok (ArrowState s p h (Cmds st))) = 
            "Position: " ++ (show p) ++ 
            "\nHeading: " ++(show h) ++ "\n" ++
            "Stack amount: " ++ (show $ length st) ++ "\n" ++  (show st) ++   
            printSpace s ++ "\n"
    show (Fail s) = s ++ "\n"
    

-- interactive instantiates the driver which first shows a welcome and then starts looping the loop function
interactive :: Environment -> ArrowState -> IO()
interactive env state@(ArrowState s p h (Cmds stack))  = do

        -- put the start rule in the environment and at the top of the stack
        let b = step env (ArrowState s p h (Cmds ((Cmd "start" ): stack)))
        let c (Ok a) = a
        
        -- Standard explanation
        putStrLn $ "Welkom to the Arrow program!\n" ++
                   "Use the keyboard to navigate the program\n" ++
                   "To see a list of possible input type help\n" ++
                   "Each turn will show your current state at first before " ++
                   "executing your input\n"
        loop env (c b)

-- loop reads the input of the user and executes it, at the end it will call itself again with the new
-- ArrowState. When stop is typed the execution will be stopped
loop env state@(ArrowState s p h (Cmds stack))= do
        input <- getLine
        if input == "stop" then
            putStr "The application will end now\n"
        else do
        
            -- readInput reads the input and shows the result to the user
            if input == "help" then do
                readInput input
                loop env state
            else do
                -- Go through the stack one by one (press 'y' after each step to go on) 
                if input == "execute" then do
                    showExecute state
                
                -- Execute the command given by the user
                else do 
                    -- executeInput creates a step from the input
                    let b = executeInput input
                    
                    -- newState makes a new ArrowState from the last step where after the result is shown
                    let c = newState b
                    putStr $ show b
                    
                    -- loop gets called again with the new state
                    loop env c
            
    where
            -- Read and execute input
            readInput x | x == "help" =  putStr showHelp
                        | otherwise   = putStr $ "You typed " ++ x ++ ", which results in:\n"
             
            -- Execute the impute given by the user
            executeInput :: String -> Step
            executeInput x 
                        -- Execute all executes all commands from the stack and ONLY shows the result from the last step
                        | x == "execute all"      = steps (Ok $ state) env
                            
                       -- For all other (valid) input the command is put on the top of the stack
                       -- where after it is executed immediately
                       | x == "a"            = step env (ArrowState s p h (Cmds ((Turn Left): stack)))
                       | x == "w"            = step env (ArrowState s p h (Cmds ((Go): stack)))
                       | x == "d"            = step env (ArrowState s p h (Cmds ((Turn Right): stack)))
                       | x == "take"         = step env (ArrowState s p h (Cmds ((Take): stack)))
                       | x == "mark"         = step env (ArrowState s p h (Cmds ((Mark): stack)))
                       | x == "nothing"      = step env (ArrowState s p h (Cmds ((Nothing): stack)))
                       | x == "show"         = step env (ArrowState s p h (Cmds ((Nothing): stack))) -- the same as nothing
                       | take 4 x == "call"  = step env (ArrowState s p h (Cmds ((Cmd (drop 5 x) ): stack)))
                       | otherwise           = Fail "No valid step, nothing changed"
                           
            -- Show all valid inputs              
            showHelp =    "Valid input:\n" ++
                          "type \'execute\' to execute all commands in the stack one by one\n" ++
                          "type \'execute all\' to execute all commands in the stack at once\n" ++
                          "type \'show\' to show the current situation\n" ++
                          "type \'w\' to move in the current direction\n" ++
                          "type \'take\' to take whatever is beneath you\n" ++
                          "type \'mark\' to mark your current position\n" ++
                          "type \'nothing\' to do absolutely nothing!\n" ++
                          "type \'a\', or \'d\' to turn the heading to 90 degrees " ++
                          "to the Left, or the Right\n" ++
                          "type \'call\' and a name to call the corresponding rule from the environment\n" ++
                          "type \'stop\' to stop the interaction\n"  
                                      
            -- newStates create an ArrowState from the last step
            -- when failed the previous ArrowState is given back as result
            newState :: Step -> ArrowState
            newState (Ok a) = a
            newState (Done s p h) = ArrowState s p h (Cmds [])
            newState (Fail _ ) = ArrowState s p h (Cmds [])
            
            -- Executes all steps in a stack and returns a Done or a Fail
            steps :: Step -> Environment ->  Step
            steps f@(Fail s) _ = f   
            steps d@(Done s p h) _  = d
            steps (Ok a) env = steps (step env a) env
            
            -- showExecute goes through the stack one by one expecting user input to go through
            showExecute :: ArrowState -> IO()
            showExecute s@(ArrowState sp ps hd stk@(Cmds ss)) = do
                if length ss > 0 then do
                    let a = step env s
                    let b = newState a
                    putStr $ show a
                    putStrLn "Go on? (y / (everything else))"
                    line <- getLine
                    if line == "y" || line == [] then -- y and enter are interpreted as go to the next step
                        showExecute b
                    else do
                        putStrLn "Execution of the program stopped, you can continue from the last state"
                        loop env b
                else do
                    putStrLn "Stack is empty, you can give new commands from the current state"
                    loop env s
                    