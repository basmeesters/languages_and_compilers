module CSharpCode where
-- Code generator to transform C# to SSM

import Prelude as P hiding (LT, GT, EQ)
import ParseLib.Abstract
import Data.Map as M
import Data.Char
import Data.List as L
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

data ValueOrAddress = Value | Address
  deriving Show
 
-- Types I introduced for return values and using local variables 
type Methods = [String] 
type Vars = [String]
type Env = Map String Int

type Environment = Env -> Methods -> Code	
type MethodEnvironment = Env -> Methods -> (Code, Env, String)
type VariableEnvironment = Env -> Methods -> Vars -> (Code, Env, Vars)

-- Algebra used to generate SSM code from C# code
codeAlgebra :: CSharpAlgebra Code
                             MethodEnvironment
                             VariableEnvironment 
                             (ValueOrAddress -> Environment)

codeAlgebra = ( (fClas)
              , (fMembDecl,fMembMeth)
              , (fStatDecl,fStatExpr,fStatIf,fStatWhile,fStatReturn,fStatBlock)
              , (fExprCon,fExprVar,fExprOp, fExprMethodCall, fExprPrintCall) 
              )
 where
 fClas :: Token -> [MethodEnvironment] -> Code
 fClas c ms             =   let code = concat [x| x <- (P.map fst3 $ newMethods)]
 
                                -- Calculate the list of all the methods with a return type
                                methods = P.map ($[""]) $ P.map  ($M.empty) ms
                                returns = P.map thr3 methods
                                newMethods = P.map ($returns) $ P.map  ($M.empty) ms
				            in [Bsr "main", HALT] ++ code                   
                    
 -- Declarations don't produce code and don't influence the environment so are pretty much useless now..
 fMembDecl :: Decl -> MethodEnvironment
 fMembDecl   d env mt        = ([], env, "")
 
 -- Methods change the environments so parameters can be used in the method itself
 fMembMeth :: Type -> Token -> [Decl] -> VariableEnvironment -> MethodEnvironment
 fMembMeth   t m ps s env mt = case m of
                                    LowerId x -> case t of
                                        -- Method without return
                                        TypeVoid -> (code x, env, "")
                                                    
                                        -- Method with return (load from RR register) 
                                        _        -> (code x, env, x)
                    where
                        -- Code from method + cleaning
                        code n = [LABEL n] ++ [LINK varAmount] ++ (fst3 $ body) ++ [UNLINK, STS (-es), AJS (-(es - 1)), RET]
                        
                        -- Create a new environment
                        addParameters :: [Decl] -> Env -> Env
                        addParameters [] env = env
                        addParameters d@((Decl t (LowerId i )): pp) env = addParameters pp (M.insert i (-(length d + 1)) env)
                        
                        es = length ps
                        varAmount = length $ thr3 body
                        body = s (addParameters ps env) mt [""] 
                          
 -- Declarations don't produce code but introduce local variables which are added to the total list of variables
 fStatDecl :: Decl -> VariableEnvironment
 fStatDecl   d@(Decl ty (LowerId s)) env mt v         = ([], env, s:v)
 
 -- Evaluate expression and put all the code on the stack
 fStatExpr :: (ValueOrAddress -> Environment) -> VariableEnvironment
 fStatExpr   e env mt v = (e Value env mt ++ [pop], env, v)
 
 fStatIf :: (ValueOrAddress -> Environment) -> VariableEnvironment -> VariableEnvironment -> VariableEnvironment
 fStatIf     e s1 s2 env mt v = let  c  = e Value env mt
                                     n1 = codeSize (fst3 $ s1 env v mt)
                                     n2 = codeSize (fst3 $ s2 env v mt)
                                in  (c ++ [BRF (n1 + 2)] ++ (fst3 $ s1 env v mt) ++ [BRA n2] ++ (fst3 $ s2 env v mt), env, v)
  
 fStatWhile :: (ValueOrAddress -> Environment) -> VariableEnvironment -> VariableEnvironment  
 fStatWhile  e s1 env mt v = let c = e Value env mt
                                 n = codeSize (fst3 $ s1 env mt v)
                                 k = codeSize c
                             in  ([BRA n] ++ (fst3 $ s1 env mt v) ++ c ++ [BRT (-(n + k + 2))], env, v)
                        
 -- Evaluate the expression and save it into the R3 register
 fStatReturn :: (ValueOrAddress -> Environment) -> VariableEnvironment
 fStatReturn e env mt v = (e Value env mt ++ [STR R3] ++ [UNLINK, RET], env, v)
 
 -- Here the environment needed for all statements is built, as is the total list of local variables
 fStatBlock :: [VariableEnvironment] -> VariableEnvironment
 fStatBlock  ss env mt v = let    stmts = P.map ($v) $ P.map ($mt) $ P.map ($env) ss
                                  newVars = remDup $ P.filter (/= "") $ concat $ P.map thr3 stmts 
                                  
                                  -- When there are blocks in doubles will appear which need to be filtered out again
                                  remDup [] = []
                                  remDup (x:xs) = x : remDup (P.filter (\y -> not (x ==y)) xs)
                                  
                                  -- Create new environment from the updated list of local variables
                                  newEnv = (makeEnv newVars env 0)
                                  makeEnv [] env pos      = env
                                  makeEnv (x:xs) env pos  = makeEnv xs (M.insert x pos env) (pos + 1)
                                  
                                  -- Calculate the code from the 'new' statements
                                  newStmts = P.map ($newVars) $ P.map ($mt) $ P.map ($newEnv) ss
                                  (code, e, vars) =   (concat $ P.map fst3 newStmts, 
                                                      newEnv, 
                                                      newVars)
                           in     (code, e, vars)
                
 
 -- Handle constants with pattern matching
 fExprCon :: Token -> ValueOrAddress -> Environment
 fExprCon    c va env mt = case c of
                                ConstInt n -> [LDC n]
                                ConstBool True -> [LDC (-1)]
                                ConstBool False -> [LDC 0]
                                ConstChar n -> [LDC (ord n)]

 -- Variables used in expressions, loc uses the environment
 fExprVar :: Token -> ValueOrAddress -> Environment
 fExprVar    v va env mt = case v of
                             LowerId x -> let loc = env M.! x
                                          in  case va of
                                                Value    ->  [LDL  loc]
                                                Address  ->  [LDLA loc] 
 -- Operators in expressions
 fExprOp :: Token -> (ValueOrAddress -> Environment) -> (ValueOrAddress -> Environment) -> ValueOrAddress -> Environment
 fExprOp     o e1 e2  va env mt = case o of
                                     Operator "=" -> e2 Value env mt ++ [LDS 0] ++ e1 Address env mt ++ [STA 0]
                                     
                                     -- The special assignment operators work just like the normal assignment operator, except they
                                     -- first do the calculating using the new and old value and then assign it
                                     Operator "+=" -> calculation [ADD]
                                     Operator "-=" -> calculation [SUB]
                                     Operator "*=" -> calculation [MUL]
                                                      
                                     Operator op  -> e1 Value env mt ++ e2 Value env mt ++ [opCodes ! op]
                where
                    calculation x = e1 Value env mt ++ e2 Value env mt ++ x ++ [LDS 0] ++ e1 Address env mt ++ [STA 0]

 -- Methods calls check if the method called is in the list of methods with a return value, if so use the value from the RR register
 fExprMethodCall :: Token -> [ValueOrAddress -> Environment] -> ValueOrAddress -> Environment                              
 fExprMethodCall  t es va env mt = case t of 
                                      LowerId s -> let params = P.map ($mt) $ P.map ($env) $ P.map ($Value) es
                                                       code = concat params
                                                       ls = length es
                                                       retValue | any (==s) mt = [LDR R3]
                                                                | otherwise = []   
                                                                
                                                    -- Evaluated parameters, method call, optional value and parameter clean up
                                                    in code ++ [Bsr s] ++ retValue ++ [STS (-ls), AJS (-(ls - 1))]
                                                 
 -- Reverse the order of the expressions, evaluate them and print each with a TRAP 0                                        
 fExprPrintCall :: [ValueOrAddress -> Environment] -> ValueOrAddress -> Environment                                                  
 fExprPrintCall   es va env mt = let params = P.map ($mt) $ P.map ($env) $ P.map ($Value) (reverse es)  
                                     code   = concat params ++ replicate (length params) (TRAP 0)
                                 in  code

 -- fst, snd and thr functions for three tuples
 fst3 (x,y,z) = x
 snd3 (x,y,z) = y
 thr3 (x,y,z) = z
 
-- Operators and their respective instructions							 
opCodes :: Map String Instr
opCodes
 = fromList
     [ ( "+" , ADD )
     , ( "-" , SUB )
     , ( "*" , MUL )
     , ( "/" , DIV )
     , ( "%" , MOD )
     , ( "<=", LE  ) 
     , ( ">=", GE  )
     , ( "<" , LT  )
     , ( ">" , GT  )
     , ( "==", EQ  )
     , ( "!=", NE  )
     , ( "&&", AND )
     , ( "||", OR  )
     , ( "^" , XOR )
     ]
