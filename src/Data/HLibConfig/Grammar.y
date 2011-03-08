{
module Data.HLibConfig.Grammar where
         
import Text.Regex.Posix
import Data.Char

}

%name configParse
%tokentype { Token }
%error { parseError }

%token 
      name	      { TokenName $$ }
      bool	      { TokenBool $$ }
      int	      { TokenInt $$ }
      int64	      { TokenInt64 $$ }
      hex	      { TokenHex $$ }
      hex64	      { TokenHex64 $$ }
      float	      { TokenFloat $$ }
      string	      { TokenString $$ }
      ':'             { TokenColon }
      '='             { TokenEq }
      ';'             { TokenFin }
      ','             { TokenSep }
      '['             { TokenOA }
      ']'             { TokenCA }
      '('             { TokenOL }
      ')'             { TokenCL }
      '{'             { TokenOG }
      '}'             { TokenCG }

%%

Configuration : SettingList { $1 }
      	      | Empty       { $1 }

SettingList : Setting			{ [$1] }
	    | SettingList Setting	{ $2 : $1 }

Setting : name ':' Value ';'   { Setting $1 $3 }
	| name '=' Value ';'   { Setting $1 $3 }

Value : ScalarValue  { ScalarValue $1 }
      | Array 	     { $1 }
      | List 	     { $1 }
      | Group	     { $1 }

ValueList : Value		{ [$1] }
          | ValueList ',' Value	{ $3 : $1 }

ScalarValue : bool 	   { ScalarBool $1 }
	    | int 	   { ScalarInt $1 }
	    | int64        { ScalarInt $1 }
	    | hex 	   { ScalarInt $1 }
	    | hex64 	   { ScalarInt $1 }
	    | float	   { ScalarFloat $1 }
            | string	   { ScalarString $1 }

ScalarValueList : ScalarValue				{ [$1] } 					
		| ScalarValueList ',' ScalarValue	{ $3 : $1 }

Array : '[' ScalarValueList ']' { Array $2 }
      | '[' Empty ']' { Array [] }

List : '(' ValueList ')'{ List $2 }
List : '(' Empty ')'	{ List [] }
     
Group : '{' SettingList '}' { Group $2 }
Group : '{' Empty '}'	    { Group [] }
     
Empty :	{- empty -}	{ [] }

{

parseError :: [Token] -> a
parseError t = error ("Parse error on " ++ (show t))

data Token
    = TokenLet
    | TokenName String
    | TokenBool Bool
    | TokenInt Int
    | TokenInt64 Int
    | TokenHex Int
    | TokenHex64 Int
    | TokenFloat Float
    | TokenString String
    | TokenColon
    | TokenEq
    | TokenFin
    | TokenSep
    | TokenOA
    | TokenCA
    | TokenOL
    | TokenCL
    | TokenOG
    | TokenCG
 deriving Show
          
type Configuration = [Setting]

data Setting = Setting String Value deriving (Eq, Show)

data Value = ScalarValue ScalarValue
           | Array [ScalarValue]
           | List [Value]
           | Group [Setting]
           deriving (Eq, Show)
             
data ScalarValue = ScalarBool Bool
                 | ScalarInt Int
                 | ScalarFloat Float
                 | ScalarString String
                 deriving (Eq, Show)
             


lexer :: String -> [Token]
lexer [] = []

lexer (':':cs) = TokenColon : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer (';':cs) = TokenFin : lexer cs
lexer (',':cs) = TokenSep : lexer cs
lexer ('[':cs) = TokenOA : lexer cs
lexer (']':cs) = TokenCA : lexer cs
lexer ('(':cs) = TokenOL : lexer cs
lexer (')':cs) = TokenCL : lexer cs
lexer ('{':cs) = TokenOG : lexer cs
lexer ('}':cs) = TokenCG : lexer cs

lexer (c:cs) 
      | isSpace c = lexer cs
      | otherwise = lexRegexp (c:cs)
                    
boolRegexp = "^([Tt][Rr][Uu][Ee])|([Ff][Aa][Ll][Ss][Ee])"
stringRegexp = "^\\\"([^\\\"\\\\]|\\\\.)*\\\""
nameRegexp = "^[A-Za-z\\*][-A-Za-z0-9_\\*]*"
integerRegexp =	"^[-+]?[0-9]+"
integer64Regexp = "^[-+]?[0-9]+L(L)?" 
hexRegexp = "^0[Xx][0-9A-Fa-f]+" 
hex64Regexp = "^0[Xx][0-9A-Fa-f]+L(L)?" 
floatRegexp = "^([-+]?([0-9]*)?\\.[0-9]*([eE][-+]?[0-9]+)?)|([-+]([0-9]+)(\\.[0-9]*)?[eE][-+]?[0-9]+)"

lexRegexp cs =
  let i = (cs =~boolRegexp) :: String
      l = length i
  in if l /= 0 then
       TokenBool (read ((toUpper $ head i):tail i) ) : (lexer $ drop l cs)
     else let i = (cs =~stringRegexp) :: String
              l = length i
          in if l /= 0 then
               TokenString (read i) : (lexer $ drop l cs)
             else let i = (cs =~nameRegexp) :: String
                      l = length i
                  in if l /= 0 then
                       TokenName i : (lexer $ drop l cs)
                     else let i = (cs =~floatRegexp) :: String
                              l = length i
                          in if l /= 0 then
                               TokenFloat (read i) : (lexer $ drop l cs)
                             else let i = (cs =~integerRegexp) :: String
                                      l = length i
                                  in if l /= 0 then
                                       TokenInt (read i) : (lexer $ drop l cs)
                                     else (lexer $ drop 1 cs)

main = getContents >>= print . configParse . lexer

}

