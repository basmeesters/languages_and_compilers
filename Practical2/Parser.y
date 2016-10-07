{
module Parser where
import Types
import Tokens
import Prelude hiding (Left, Right, Nothing)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    go          { TokenGo }
    take        { TokenTake }
    mark        { TokenMark }
    nothing     { TokenNothing }
    turn        { TokenTurn }
    case        { TokenCase }
    of          { TokenOf }
    end         { TokenEnd }
    left        { TokenLeft }
    right       { TokenRight}
    front       { TokenFront }
    debris      { TokenDebris }
    asteroid    { TokenAsteroid }
    boundary    { TokenBoundary }
    empty       { TokenEmpty }
    lambda      { TokenLambda }
    idents      { TokenIdents $$}
    point       { TokenPoint}
    comma       { TokenComma }
    semicolon   { TokenSemiColon }
    underscore  { TokenUnderscore}
    arrow       { TokenArrow}

%%
Program : Prods {Program (reverse $1)}
Prods : Rule {[$1]} | Prods Rule {$2 : $1 }
Rule : idents arrow Cmds point {Rule $1 $3}

Cmds : ProdsCmds {Cmds (reverse $1)}
ProdsCmds : Cmd {[$1]} | ProdsCmds comma Cmd{$3 : $1} | {- empty -} {[]}
Cmd : go {Go} | take {Take} | mark {Mark} | nothing {Nothing} | 
      turn Dir {Turn $2} | case Dir of Alts end {Case $2 $4} | idents {Cmd $1}

Dir : left {Left} | right {Right}| front {Front}    
Alts: ProdsAlts {Alts (reverse $1)}
ProdsAlts : Alt {[$1]} | ProdsAlts semicolon Alt{$3 : $1} | {- empty -} {[]}
Alt : Pat arrow Cmds {Alt $1 $3}
Pat : empty {Empty} | lambda {Lambda} | debris {Debris} | 
      asteroid {Asteroid} | boundary {Boundary} | underscore {Underscore}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}