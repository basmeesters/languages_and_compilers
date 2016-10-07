module CSharpLex where
-- Lexer which transforms a string into a list of tokens

import Data.Char
import Control.Monad
import ParseLib.Abstract

data Token = POpen    | PClose        -- parentheses     ()
           | SOpen    | SClose        -- square brackets []
           | COpen    | CClose        -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse  
           | KeyWhile 
           | KeyFor                   -- Added for assignment 9
           | KeyReturn 
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String         
           | Operator  String         
           | UpperId   String        
           | LowerId   String         
           | ConstInt  Int            
           | ConstBool Bool           -- Added for assignment 1
           | ConstChar Char           -- Added for assignment 1
           deriving (Eq, Show)

-- Recognize names of classes, methods and variables
keyword :: String -> Parser Char String
keyword []                    = succeed ""
keyword xs@(x:_) | isLetter x = do
                                  ys <- greedy (satisfy isAlphaNum)
                                  guard (xs == ys)
                                  return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty

terminals :: [(Token, String)]
terminals =
    [( POpen     , "("      )
    ,( PClose    , ")"      )
    ,( SOpen     , "["      )
    ,( SClose    , "]"      )
    ,( COpen     , "{"      )
    ,( CClose    , "}"      )
    ,( Comma     , ","      )
    ,( Semicolon , ";"      )
    ,( KeyIf     , "if"     )
    ,( KeyElse   , "else"   )
    ,( KeyWhile  , "while"  )
    ,( KeyFor    , "for"    )   -- Added for assignment 8
    ,( KeyReturn , "return" )
    ,( KeyTry    , "try"    )
    ,( KeyCatch  , "catch"  )
    ,( KeyClass  , "class"  )
    ,( KeyVoid   , "void"   )
    ]

-- Parse whitespace and comments
lexIgnoreList :: Parser Char String
lexIgnoreList = lexWhiteSpace <* many (lexComments <* lexWhiteSpace)	
	
-- Return an empty list for comments	
lexComments :: Parser Char String
lexComments = let comments = greedy (satisfy (/= '*')) <* symbol '*' <* ([] <$ symbol '/' <<|> comments)
              in [] <$ token "/*" <* comments  <|> 
                 [] <$ token "//" <* many (satisfy (/='\n')) 

-- Whitespace			
lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

-- Parse ints, strings, bools and chars
lexLowerId :: Parser Char Token
lexLowerId =  (\x xs -> LowerId (x:xs))
          <$> satisfy isLower
          <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId =  (\x xs -> UpperId (x:xs))
          <$> satisfy isUpper
          <*> greedy (satisfy isAlphaNum)

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

lexConstBool :: Parser Char Token
lexConstBool = ConstBool <$> ((True <$ keyword "true") <|> (False <$ keyword "false"))

lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$> (symbol '\'' *> anySymbol <* symbol '\'')

-- Make specific parser from the keywords recognized and the given String to Token
lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice (map (\ (t,s) -> t <$ keyword s) terminals)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float",
            "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+=", "*=", "-=", "+", "-", "*", "/", "%", "&&", "||",
             "^", "<=", "<", ">=", ">", "==",
             "!=", "="]


-- Parse a token greedily			 
lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstBool                 -- Added for assignment 1
             , lexConstChar                 -- Added for assignment 1
             , lexLowerId
             , lexUpperId
             ]

-- Parse the complete input and ignores comments and white space (assignment 7)		 
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexIgnoreList *> greedy (lexToken <* lexIgnoreList) <* eof

-- Parse standard types, strings, ints and operators
sStdType :: Parser Token Token
sStdType = satisfy isStdType
       where isStdType (StdType _) = True
             isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
       where isUpperId (UpperId _) = True
             isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
       where isLowerId (LowerId _) = True
             isLowerId _           = False

sPrint :: Parser Token Token
sPrint = satisfy isPrint
    where
        isPrint (LowerId "print") = True
        isPrint _                 = False

sConst :: Parser Token Token
sConst  = satisfy isConst
       where isConst (ConstInt  _) = True
             isConst (ConstBool _) = True     
             isConst (ConstChar _) = True
             isConst _             = False
             
-- Recognize the different operators            
sOperator = satisfy isOperator
	   where
            isOperator (Operator "=") = True
            isOperator (Operator "+=") = True
            isOperator (Operator "*=") = True
            isOperator (Operator "-=") = True
            isOperator _ = False
             
sOperator0 = satisfy isOperator
	   where
			isOperator (Operator "||") = True
			isOperator _ = False
			
sOperator1 = satisfy isOperator
	   where
			isOperator (Operator "&&") = True
			isOperator _ = False	

sOperator2 = satisfy isOperator
	   where
			isOperator (Operator "^") = True
			isOperator _ = False
			
sOperator3 = satisfy isOperator
	   where
			isOperator (Operator "==") = True
			isOperator (Operator ">=") = True
			isOperator (Operator "<=") = True
			isOperator (Operator ">") = True
			isOperator (Operator "<") = True
			isOperator (Operator "!=") = True
			isOperator _ = False	
			
sOperator4 = satisfy isOperator
	   where 
			isOperator (Operator "+") = True
			isOperator (Operator "-") = True
			isOperator _              = False
			
sOperator5 = satisfy isOperator
	   where 
			isOperator (Operator "*") = True
			isOperator (Operator "/") = True
			isOperator (Operator "%") = True
			isOperator _              = False		
             
-- Recognize semicolon
sSemi :: Parser Token Token
sSemi =  symbol Semicolon

