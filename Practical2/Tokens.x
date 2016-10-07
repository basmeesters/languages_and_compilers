{
module Tokens where
}

%wrapper "basic"

$digit = 0-9            -- digits
$letter = [a-zA-Z]      -- alphabetic characters
$ident = [$letter $digit \+ \-]

tokens :-

    $white+                               ;
    "--".*                                ;
    "->"                                { \s -> TokenArrow }
    \.                                  { \s -> TokenPoint }
    \,                                  { \s -> TokenComma }
    go                                  { \s -> TokenGo }
    take                                { \s -> TokenTake }
    mark                                { \s -> TokenMark }
    nothing                             { \s -> TokenNothing }
    turn                                { \s -> TokenTurn }
    case                                { \s -> TokenCase }
    of                                  { \s -> TokenOf }
    end                                 { \s -> TokenEnd }
    left                                { \s -> TokenLeft }
    right                               { \s -> TokenRight}
    front                               { \s -> TokenFront }
    \;                                  { \s -> TokenSemiColon }
    Lambda                              { \s -> TokenLambda }
    Debris                              { \s -> TokenDebris }
    Asteroid                            { \s -> TokenAsteroid }
    Boundary                            { \s -> TokenBoundary }
    Empty                               { \s -> TokenEmpty }
    \_                                  { \s -> TokenUnderscore }
    $ident+                             { \s -> TokenIdents s}
{

-- The token type:
data Token = TokenArrow | TokenPoint | TokenComma | TokenGo | TokenTake |
             TokenMark | TokenNothing | TokenTurn |TokenCase | TokenOf |
             TokenEnd | TokenLeft | TokenRight | TokenFront | TokenSemiColon |
             TokenEmpty | TokenLambda | TokenDebris | TokenAsteroid | TokenBoundary |
             TokenUnderscore | TokenIdents String deriving Show

}    
