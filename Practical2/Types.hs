module Types where

import Prelude hiding (Left, Right, Nothing)    
  
-- Types used in the Arrow language  
data Program = Program [Rule] 
        deriving Show
data Rule = Rule Idents Cmds 
        deriving Show
data Cmds = Cmds [Cmd] 
        deriving (Show, Eq)
data Cmd = Go | Take | Mark | Nothing |
            Turn Dir | Case Dir Alts | Cmd Idents
        deriving (Show, Eq)
data Dir = Left | Right | Front deriving (Show, Eq)
data Alts = Alts [Alt] deriving (Show, Eq)
data Alt = Alt Pat Cmds deriving (Show, Eq)
data Pat = Empty | Lambda | Debris | Asteroid | Boundary |Underscore deriving (Show, Eq)

-- Idents is only accepted by tokens when it consists of only letters, digits, 
-- plusses and minusses, so making it a String is sufficient
type Idents = String 

