module CSharpGram where
-- Syntax of C# and functions for parsing it from the tokens

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex

-- Abstract syntax        
data Class = Class     Token [Member]          
          deriving Show

data Member = MemberD  Decl
            | MemberM  Type Token [Decl] Stat
          deriving Show
          
data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst       Token
          | ExprVar         Token
          | ExprOper        Token Expr Expr
          
          -- Print and method calls with parameters (assignments 3 and 4)
          | ExprMethodCall  Token [Expr]
          | ExprPrintCall   Token [Expr]
          deriving Show
          
data Decl = Decl       Type Token
          deriving Show

data Type = TypeVoid
          | TypePrim   Token
          | TypeObj    Token
          | TypeArray  Type
          deriving (Eq,Show)	  

parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

-- Parse expressions
pExprSimple :: Parser Token Expr
pExprSimple = ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr
           <|> 
           (
               -- ExprPrintCall should be parsed greedily so it is not recognized as a method
               ExprPrintCall <$> sPrint <*> parenthesised (option (listOf pExpr (symbol Comma)) [])
               <<|> ExprMethodCall <$> sLowerId <*> parenthesised (option (listOf pExpr (symbol Comma)) [])
           )
 
-- Expressions with priorities	   
pExpr :: Parser Token Expr							  
pExpr = chainr e (ExprOper <$> sOperator)
e  = chainr e1 (ExprOper <$> sOperator0) -- All but assignment operators are left associative
e1 = chainl e2 (ExprOper <$> sOperator1)
e2 = chainl e3 (ExprOper <$> sOperator2)
e3 = chainl e4 (ExprOper <$> sOperator3) 
e4 = chainl e5 (ExprOper <$> sOperator4)
e5 = chainl pExprSimple (ExprOper <$> sOperator5)
		     
-- Parse declarations or methods
pMember :: Parser Token Member
pMember =   MemberD <$> pDeclSemi
        <|> pMeth

-- Parse statements 
pStatDecl :: Parser Token Stat
pStatDecl =   pStat
          <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat = StatExpr
         <$> pExpr 
         <*  sSemi
     <|> StatIf
         <$  symbol KeyIf
         <*> parenthesised pExpr
         <*> pStat
         <*> option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])
     <|> StatWhile
         <$  symbol KeyWhile
         <*> parenthesised pExpr
         <*> pStat
         
     -- Recognize a for loop and make it a while statement
     <|> makeFor
            <$ symbol KeyFor 
            <*> parenthesised
                ((\x y z -> (x,y,z)) <$>
                    StatExpr <$> pExpr <* sSemi
                    <*> pExpr <* sSemi
                    <*> pExpr
                )
            -- Inside of a for-loop
            <*> pStat     
            
     <|> StatReturn
         <$  symbol KeyReturn
         <*> pExpr
         <*  sSemi
     <|> pBlock
   where 
        makeFor (x,y,z) inside = StatBlock (x: [StatWhile y (makeBlock inside z)])
        makeBlock :: Stat -> Expr -> Stat
        makeBlock (StatBlock x) y = StatBlock (x ++ [StatExpr y])
        makeBlock x y = StatBlock (x : [StatExpr y])
     
pBlock :: Parser Token Stat
pBlock  =  StatBlock
           <$> braced( many pStatDecl )

pMeth :: Parser Token Member
pMeth =  MemberM
         <$> (   pType 
             <|> const TypeVoid <$> symbol KeyVoid
             )
         <*> sLowerId
         <*> parenthesised (option (listOf pDecl
                                           (symbol Comma)
                                   )
                                   []
                           )
         <*> pBlock

-- Parse types 		 
pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) 
        <$> pType0 
        <*> many (bracketed (succeed ()))

-- Parse declarations and classes
pDecl :: Parser Token Decl 
pDecl = Decl
        <$> pType
        <*> sLowerId
        
pDeclSemi :: Parser Token Decl 
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class
        <$  symbol KeyClass
        <*> sUpperId
        <*> braced ( many pMember )
