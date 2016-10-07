README 
    Assignment 2 Languages and Compilers

Student
    Bas Meesters    3700569

Program structure
    The program exists of seven modules; Types, Tokens, Parser, Language, ArrowCode, 
    Interpreter, and Driver
        - Tokens is generated with Alex from Tokens.x 
          (exercise 1)
        - Types consists of the types and data structures of the arrow program 
          (exercise 2)
        - Parser is generated with Happy from Parser.y (exercise 3)
        - Language contains the type ProgramAlgebra, checkAlgebra and foldProgram functions 
          (exercise 5 and 6)
        - ArrowCode is the same as given with the exception of some types
        - Interpreter contains printSpace, toEnvironment and step
          (exercises 7, 8 and 9)
        - Driver contains the iterative function which makes it possible to walk through
          an ArrowState in an Environment. Next to walking through the stack step by step, 
          it is to go through the stack at once or give commands yourself at any point in the 
          execution point which are then placed on top of the stack and therefore executed 
          immediately.

          When interactive is instantiated type 'execute all' to execute all commands at once, 
          or type "execute" to execute one step at a time, asking for permission to go to
          the next step between each. Type 'help' to see all other possible commands. The
          remove debris example is implemented, so by typing exampleProgram and then execute
          all will show the correct result for that program in the given space.
          (exercise 11)
 
Exercise 4:
    Happy uses left recursion because it is more efficient than parsing right recursive.
    The result is in constant stack space whereas with right recursive rules the stack 
    space is proportional to the length of the list. Especially with long lists this can
    make an enormous difference. The only 'downside' is that the result is a reversed list
    from the original order.

Exercise 10: 
    In the step function the top of the stack gets executed. When that command is 
    recursive all the commands in the rule will be added to the top of the stack, 
    including the recursive one. If the recursive call is halfway in the sequence all
    the commands coming before the recursive call are placed before the recursive call
    at the top of the stack again. But all the calls after the recursive call will
    not be called until the recursive call has some kind of end and is not recursive
    any more. So it does matter if the recursion is in the middle or at the end, since 
    all the commands after the call are only executed after the recursion ends. Also,
    since all commands are added to the stack but new ones are added halfway because 
    of the recursion the stack keeps increasing until the recursion ends and the rest 
    of the commands can be executed
    