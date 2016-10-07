Read me
---------------------------------
Student             Bas Meesters
Studentnumber       3700569
  
---------------------------------

I didn't really know how to make perfecetly clear what changes I did to the framework so I choose to put two kinds of explanations below.
First I will roughly explain the biggest changes for each file, thereafter I will explain below for each assignment what I did. So step 
by step it becomes clear how the end result came to be. For the record, I finished assignment 6 earlier then 5 so I changed the order below 
as well. 
    
---------------------------------   
Changes per file: 
    CSharpLex.hs
        - Token is changed so for loops and constants can be used
        - There is a ignore list lexer which uses the whitespace and the comment lexers (replaces the old one in lexicalscanner)
        - sOperator is split into different parsers for priorities
    
    CSharpGram
        - Expr is changed to handle print and method calls
        - pExpr is changed so priorities are given to operators
        - pStat is changed so it makes while statements from for statements
    
    CSharpAlgebra
        - Only adapted to handle the changed datatypes
    
    CSharpCode
        - Uses three kind of Environments
           -> Environment: 
                 Used in expressions, it expects a Map String Int and a list of methods and returns Code
                 The list of methods is needed so a method call knows if it should load the last value from the RR or not
           -> VariableEnvironment: 
                 Used in statements, it expects a list of methods, a list of variables and an environment
                 In a statblock all the variables needed are calculated and used in the calculation for code  
                 The same goes for the environment, it is calculated beforehand and given to all statements in the block
                 This is needed so there can be made place for local variables with LINK and they can be used properly
                 Because there can be more than one statblock in a method the lists get concatenated and doubles will appear, they are filtered out
            -> MethodEnvironment:
                 Used in methods, they get a list of all methods with return types calculated by fClas the same way as explained above
                 This list is used in call methods to check if there is a return value
        - There are sub functions for handling print and method calls
        - fExprOp is changed so +=, -= and *= operations can be handled as well
    
    CSharpPrint
        - New file containing an algebra for pretty printing parsed C# files

    Main
        - Has one new function for using the printAlgebra
                 
---------------------------------    
Changes for the assignments: 
	1. For boolean and character values to be accepted I first created lexers for both of them in CSharpLex.hs. Thereafter I changed 
       fExprConst so that it converts the boolean and character values to corresponding integers. I also changed sConst so the values
       are accepted when parsing constants. 
	2. In CSharpLex.hs I changed the sOperator to separated parsers, grouped according to their priorities. Then it was possible to 
       change pExpr in CSharpGram.hs and call the different parsers in the right order using chainl and chainr. All operators are
       left associated except for the assign operators. The comparison operators should be non associative but I did not make them so.
	3. I created a type Env in CSharpCode.hs which contains the names of variables as keys and their addresses as values. I also created 
       a type Environment = Env -> (Code, Env) which gets passed around in the CodeAlgebra. There are two other environments as well, but
       they are not used for this assingment yet. All methods (except for fClas) in the algebra expect a Env and return a new one in a 
       tuple with SSM code. In CSharpGram I created another constructor, ExprMethodCall, so methods can be called with zero or more 
       parameters. When a method is called the parameters are stored in the environment that is used in the method and fExprVar makes 
       sure the address of the variable used is returned by looking up the variable in the environment.     
    4. In CSharpGram.hs I gave Expr another constructor for calls, ExprPrintCall. This constructor is parsed greedily compared to methods 
       calls, since it should parse to ExprPrintCall and not to ExprMethodCall when print is recognized. I also changed the CSharpAlgebra 
       and the foldCSharp to handle this new constructor. So when an expression starts with print it will evaluate all the expressions 
       in the reverse order and print each of them by finishing with TRAP 0 for each argument. Since the last values on the stack get 
       printed first, the original order is returned.
    6. I changed the types again so it expects a list of type Methods, which is a list of strings, together with the environment and it 
       will return code, the environment and the list of variables in a three tuple. fStatblock is changed so it computes the code using 
       the complete list of variables. Here the environment is also built with all the statements in the block. The new computed environment 
       and list of variables are given to all statements in the block, so local variables can be accessed correctly in all statements.  
	5. I couldn't really figure out how to intuitively calculate when a method has a return type and when it hasn't from the call expression,
       so I let fClas compute a list of all methods with a return type and sent it futher down the algebra. So when a method call is done it
       checks if the method called is in the list, and if so, it will put the load from the RR register at the end of the call. fStatreturn
       is changed so it will store the last value in the RR register. Also there were some problems with using the result in an assignment
       because the parameters were in they way, so they get cleaned up as well. 
	7. In CSharpLex.hs I added a function lexComments which recognizes a '//' and discards all the symbols after until a new line starts.
	   Or it recognizes a '/*'  and discards all the symbols after until another '*/' is recognized. 
    8. I added the 'special' operators +=, -= and *= to the lowest priority operators (like =) in the CSharplex.hs. In the CSharpCode.hs
       I changed the fExprOp expression so it handles these operators properly. They work like normal assignments accepted the old value
       is taken into account when assigning a new one. Because the minus is the only one of which the order matters, the first value is put
       the stack first, then the second, thereafter the calculation is done and the value is saved like a normal assignment.
    9. To support for statements I changed the tokens so a for statement is lexed as well. In the grammar I parsed the for statement in such
       a way that it becomes a while-loop. It places the first expression (declaration) before the loop, places the third expression at the end
       of the while loop (counter) and uses the second argument as comparison expression like a normal while loop. The algebra and the fold 
       doesn't have to change any more since it will be handled as a while loop from that moment on.
   10. It is possible to print the parsed C# file back to a string. This is done in CSharpPrint.cs. It contains an algebra for doing this.
       Because I had some trouble with the aligning at first I added a Indentations type which expects an Int and returns a tuple of a Doc and an 
       Int. This way, it was easier to have nested loops (for, while and if statements) and blocks of statements aligned well. 
 