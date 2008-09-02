{ 
module SetLangParser (module SetLang, applySetLang, parseSetLang) where
import SetLang 
import SetLangLexer
import Char
}

%name slparse
%tokentype { Token }
%error { parseError }

%token
	string	{ TString $$ }
	'='	{ TEq }
	"!="	{ TNEq }
	'!'	{ TNot }
	"||"	{ TOr }
	"&&"	{ TAnd }
	"^^"	{ TXor }
	'('	{ TOB }
	')'	{ TCB }
	':'	{ TColon }
	';'	{ TSemicolon }
	','	{ TComma }
	"true"	{ TTrue }
	"false"	{ TFalse }
	"nop"	{ TNop }
	"in"	{ TIn }
	"defined" { TDefined }
	"declare" { TDeclare }
	"unset"	{ TUnset }
%%

Statement : Condition ':' Actions { Expression $1 $3 }
Actions
	: Action		  { [ $1 ] }
	| Action ';' Actions	  { $1 : $3 }

Condition
	: '!' Condition				{ Not $2 }
	| Condition "&&" Condition		{ And $1 $3 }
	| Condition "||" Condition		{ Or $1 $3 }
	| "defined" '(' Identifier ')'		{ Defined $3 }
	| Condition "^^" Condition		{ Xor $1 $3 }
	| Identifier "in" '(' Domain ')'	{ In $1 $4 }
	| Identifier '=' Value	{ Is $1 $3 }
	| Identifier "!=" Value	{ Not (Is $1 $3) }
	| "true"		{ CTrue }
	| "false"		{ CFalse }
	| '(' Condition ')'	{ $2 }

Action
	: "nop" { Nop }
	| "declare" Identifier '=' '(' Domain ')' { DeclareVar $2 $5 }
	| "unset" Identifier { UnsetVar $2 }
	| Identifier '=' Value { SetVar $1 $3 }

Identifier : string	{ $1 }
Value : string	{ $1 }

Domain : SetOfValues { domainFromStringList $1 }
SetOfValues
	: Value	{ [ $1 ] }
	| Value ',' SetOfValues { $1 : $3 }

{
parseError :: [Token] -> a
parseError x = error ("Parse error: " ++ show x)

parseSetLang :: String -> Expression
parseSetLang = slparse . alexScanTokens 

applySetLang :: VarState -> String -> Maybe VarState
applySetLang s e = apply (parseSetLang e) s

}
