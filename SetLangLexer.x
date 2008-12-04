{
module SetLangLexer where
}

%wrapper "basic"

tokens :-

	$white+ 	;
	"="		{ \s -> TEq }
	"!="		{ \s -> TNEq }
	"!"		{ \s -> TNot }
	"||"		{ \s -> TOr }
	"&&"		{ \s -> TAnd }
	"^^"		{ \s -> TXor }
	"("		{ \s -> TOB }
	")"		{ \s -> TCB }
	":"		{ \s -> TColon }
	";"		{ \s -> TSemicolon }
	","		{ \s -> TComma }
	"true"		{ \s -> TTrue }
	"false"		{ \s -> TFalse }
	"nop"		{ \s -> TNop }
	"in"		{ \s -> TIn }
	"defined"	{ \s -> TDefined }
	"declare"	{ \s -> TDeclare }
	"unset"		{ \s -> TUnset }
	[^$white\(\)\:\;\,\=\!\&\|\^]+	{ \s -> TString s }
	"žšžšžš"	;

{
data Token 
	= TString String
	| TEq
	| TNEq
	| TNot
	| TOr
	| TAnd
	| TXor
	| TOB
	| TCB
	| TColon
	| TSemicolon
	| TComma
	| TTrue
	| TFalse
	| TNop
	| TIn
	| TDefined
	| TDeclare
	| TUnset
  deriving (Show, Eq)
}
