module Language where

import Types
import Prelude hiding (Left, Right, Nothing)

type ProgramAlgebra p r i cmds cmd dir alts alt pat = (
            [r] -> p,                   -- Program
            i -> cmds -> r,             -- Rule
            [cmd] -> cmds,              -- Cmds
            Idents -> i,                -- Idents
            cmd,                        -- Go
            cmd,                        -- Take
            cmd,                        -- Mark
            cmd,                        -- Nothing
            dir -> cmd,                 -- Turn
            dir -> alts -> cmd,         -- Case
            Idents ->  cmd,             -- Idents
            dir,                        -- Left
            dir,                        -- Right
            dir,                        -- Front
            [alt] -> alts,              -- Alts
            pat -> cmds -> alt,         -- Alt
            pat,                        -- Empty
            pat,                        -- Lambda
            pat,                        -- Debris
            pat,                        -- Asteroid
            pat,                        -- Boundary
            pat)                        -- Underscore

foldProgram :: ProgramAlgebra p r i cmds cmd dir alts alt pat 
                -> Program -> p			                
                                
foldProgram (program, 
            rule, cmds, idents,
            go, take, mark, nothing, turn, c, idents2, 
            left, right, front,
            alts, alt, 
            empty, lambda, debris, asteroid, boundary, underscore) = f
        where
                -- Program function
                f (Program xs) = program (map frule xs)
                
                -- Rule function
                frule (Rule ind cs) = rule (idents ind) (fcs cs)
                
                -- Cmds function
                fcs (Cmds xs) = cmds (map fcmd xs)
                
                -- Cmd functions
                fcmd (Go) = go
                fcmd (Take) = take
                fcmd (Mark) = mark
                fcmd (Nothing) = nothing
                fcmd (Turn d) = turn (fdir d)
                fcmd (Case d a) = c (fdir d) (falts a)
                fcmd (Cmd i) = idents2 i 
                
                -- Dir functions
                fdir (Left) = left
                fdir (Right) = right
                fdir (Front) = front
                
                -- Alts  en Alt functions
                falts (Alts xs) = alts (map falt xs)
                falt (Alt p cms) = alt (fpat p) (fcs cms)
                
                -- Pat functions 
                fpat (Empty) = empty
                fpat (Lambda) = lambda
                fpat (Debris) = debris
                fpat (Asteroid) = asteroid
                fpat (Boundary) = boundary
                fpat (Underscore) = underscore
                
-- 6. 
check :: Program -> Bool
check = foldProgram checkAlgebra

-- checkAlgebra checks if a Program does have a rule start, no double defined rules
-- and if all calls to rules are existent
-- The type is therefore all Strings which check if they are valid, returning a boolean
checkAlgebra :: ProgramAlgebra Bool (String,[String]) String [String] [String] String [String] [String]  String
checkAlgebra = (program, rule, cmds, idents, go, take, mark, nothing, turn, c,
                idents2, left, right, front, alts, alt, empty, lambda, 
                debris, asteroid, boundary, underscore)
            where
                
                program = 
                    -- Check if there is a rule named start
                    (\xs -> (isStart $ getNames xs) 
                    
                    -- Check if all rules are unique
                     && (checkUnique $ getNames xs)  
                     
                     -- Check if all calls to rules are valid
                     && (valid (getAll xs) (getNames xs))) 
                          
                    where
                        -- Get all names of existing rules
                        getNames [] = []
                        getNames ((name, calls) :xs) = name : getNames xs
                        
                        -- Check if a rule start exists
                        isStart xs = any(=="start") xs 
                        
                        -- Check if there are no double defined rules
                        checkUnique xs = and $ checkDoubles  xs
                        checkDoubles [] = [True]
                        checkDoubles (x:xs) = (not (any (==x) xs)) : checkDoubles xs
                        
                        
                -- rule returns a tuple of the name and all the commands
                rule = (\i xs -> (i, xs))
                
                -- cmds returns the list of calls to rules filtering empty calls
                cmds = (\xs -> (filter (/= "") $ concat xs))
                        
                idents = id -- Name of the rule
                
                -- Case expressions return the list of all calls in Alts
                c = (\x y -> y)
                idents2 = (\i -> [i]) 
                
                -- Since pats are always valid, check only Cmds 
                alts = (\xs -> concat xs)
                alt = (\x y -> y) 
                
                -- cmds expects a list of calls which are empty in the cases below
                go = [""]
                take = [""]
                mark = [""]
                nothing = [""]
                turn = (\d -> [""])
                
                -- All matched constructors without variables don't have calls
                left = ""
                right = "" 
                front = ""
                
                empty = ""
                lambda = ""
                debris = ""
                asteroid = ""
                boundary = ""
                underscore = ""
                
                -- valid checks if the each call is in the list of rules               
                valid xs ys = and (validCalls xs ys)                
                validCalls [] _ = [True]
                validCalls (x:xs) list = (any (==x) list) : validCalls xs list 
                
                -- getAll returns all calls to other rules in the program and puts them in one list
                getAll xs = concat $ getCalls xs
                getCalls :: [(String,[String])] -> [[String]]
                getCalls [] = [[]]
                getCalls ((name, calls) :xs) = calls : getCalls xs 

