module CSharpPrint where
-- Printer to print parsed C# code to a string

import Prelude as P hiding (LT, GT, EQ)
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import Text.PrettyPrint.Leijen
import System.Environment
import System.FilePath
import ParseLib.Abstract.Derived hiding (empty, (<$>))

-- All variables return a Doc and statements expect an Int as how far to indent the Doc
printAlgebra :: CSharpAlgebra Doc Doc (Int -> Doc) Doc
printAlgebra = ( (fClas)
              , (fMembDecl,fMembMeth)
              , (fStatDecl,fStatExpr,fStatIf,fStatWhile,fStatReturn,fStatBlock)
              , (fExprCon,fExprVar,fExprOp, fExprMethodCall, fExprPrintCall) 
              )
 where
 fClas       c ms 	        = case c of
                                UpperId s -> text "class" <+> text s <$> text "{\n" <$> indent 3 (vcat ms) <> text "}"
 
 -- decl is used in global declarations, local declarations and parameter declarations.
 decl :: String -> Decl -> Doc                              
 decl        i d@(Decl t n) = case t of
                                TypePrim (StdType s) -> case n of
                                                          LowerId m -> text s <+> text m <> text i -- i is the end signature of the declarion
                                                          
 fMembDecl   d              = decl ";\n" d
 fMembMeth   t m ps s       = case m of
                                LowerId x -> case t of
                                    TypeVoid                -> text "void" <+> text x <> 
                                                               makeBody
                                    TypePrim (StdType i)    -> text i <+> text x <> 
                                                               makeBody 
                                    _                       -> empty
                where
                    makeParams x        | length x == 0 = text ")\n"        
                                        | length x == 1 = decl ")\n" (head x)
                                        | otherwise = hcat (map (decl ", ") (init x)) <> (decl ")\n" (last x))
                    makeBody = text " (" <> makeParams ps <> indent 3 (text "{\n") <> s 6 <> indent 3 (text "}\n\n")
 
 -- Statements are the only thing that can nest multiple times, so they expect an Int to see how far they should indent 
 fStatDecl   d i              = indent i $ decl ";\n" d
                                                          
 fStatExpr   e i            = indent i $ e <> text ";\n"
 fStatIf     e s1 s2 i      | (show empty == show (s2 i))   = ifStatement
                            | otherwise                     = ifStatement <> elseStatement
                    where
                            ifStatement = indent i $ text "if (" <> e <> text ")\n" <> indent i (text "{\n" <> (s1 (i + 3) <> indent i (text "}\n")))
                            elseStatement = indent i $ text "else (" <> e <> text ")\n" <> indent i (text "{\n" <> (s1 (i + 3) <> indent i (text "}\n")))
                            
 fStatWhile  e s1 i          = indent i $ text "while (" <> e <> text ")\n" <> indent i (text "{\n" <> (s1 (i + 3) <> indent i (text "}\n")))
 fStatReturn e i             = indent i $ text "return " <> e <> text ";\n"
 fStatBlock  ss i            = hcat ((map ($i) ss))  
 
 -- Expression prints
 fExprCon    c              = case c of
                                ConstInt i      -> int i
                                ConstBool True  -> text "true"
                                ConstBool False -> text "false"
                                ConstChar i     -> char i
 fExprVar    v              = case v of
                                LowerId s -> text s
 fExprOp     o e1 e2        = case o of
                                Operator s -> e1 <+> text s <+> e2
                                
 fExprMethodCall  t ps      = case t of 
                                LowerId s -> text s <> text "(" <> makeParams ps
                      
 fExprPrintCall  ps         = text "print(" <> makeParams ps <> text ")\n" 
 
 -- Print parameters for methods and print calls
 makeParams x       | length x == 0 = empty         
                    | length x == 1 = (head x)  
                    | otherwise = hcat (map (\x -> x <> text ", ") (init x)) <> (last x) <> text ")"                 