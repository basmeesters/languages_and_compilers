README 
    Assigment 1 Languages and Compilers

Student
    Bas Meesters    3700569

Program structure (also explained in the corresponding files as well)
    The program exists of six modules; Types, Parser, Printer, Instances, Testing and Calendar.
    - Types contains all the types and datatypes used in the program, as well as instances for
      showing and comparing Digits.
    - Parser contains all the parser functions to create datatypes read from strings. It also 
      contains the check functions to see if the parsing created valid datatypes.
    - Instances contains Ord instances for DateTimes so they can be compared.
    - Printer contains all print functions used to create strings in a valid format from the 
      datatypes.
    - Testing contains mainly strings and functions to check the rest of the program
    - Calendar contains the functions to create a Maybe Calendar from a file, get information
      from a Calendar and contains functions to draw a calendar for a certain month and year.