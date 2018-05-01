Program:
	topblocks+=TopBlock
;
TopBlock:
	GlobalVarsBlock|ActorsBlock|ContractParamsBlock|ProseContractBlock|FormalContractBlock
;
GlobalVarDec:
	ID ':' GvarType
;
GlobalVarDecWithId:
	ID ':' GvarType '=' Expression
;
GlobaVarsBlock:
	'global vars:' '{'
		globalvardecs += (GlobalVarDec|GlobalVarDecWithInit)
	'}'
;
ActorsBlock:
	'actor' '{' actors+=ID[','] '}'
;
FormalContractBlock:
	('start')? 'state' ID '{'
		InStateBlock

	'}'
;
InStateBlock:









Dec:
	TypeAndNamespaceDec|TypeDec|ConstantDec|StaticConstantDec|NamespaceDec|AbstractFunctionDec|FunctionDec|StaticAbstractFunctionDec|StaticFunctionDec|GetsetDec|DerivedFormsDec|StaticEnumDec|StaticFlagDec|DetachedSubnamespace|AbstractTypeCheckerFunction
;
RuntimeEnumTypeDec:  // red
	'enum' name=ID '=' options+=ID['|']
;
StaticEnumDec:  // sed
	'static-enum' name=ID '=' options+=ID['|']
;
StaticFlagDec:  // f
	'static-flag' name=ID
;
TypeDec:  // t
	'type' name=ID ('=' subtypes+=ID['⊎'] )?
;
TypeAndNamespaceDec: // tn
	'type&namespace' name=ID ('⊆' supertype=ID)? ('<' typeparams+=ID[','] '>')? '{'
		decs+=Dec
	'}'
;
NamespaceDec: // n
	'namespace' name=ID '{'
		decs+=Dec
	'}'
;
DetachedSubnamespace: // sn
	('detached_subnamespace'|'detached-subnamespace') name=NamespacedIdent '{'
		decs+=Dec
	'}'
;
ConstantDec:  // cd
	'constant' name=ID ':' typ=ID
;
StaticConstantDec:  // scd
	'static constant' name=ID ':' typ=ID
;
GetsetDec:  // gsd
	'getset' name=ID ':' typ=ID
;
AbstractFunctionDec: // afd
	'absfn' name=ID '(' paramtyps*=Typ[','] ')' ('-->' rettyp=ID)?
;
StaticAbstractFunctionDec: // safd
	'static absfn' name=ID '(' paramtyps*=Typ[','] ')' ('-->' rettyp=ID)?
;
AbstractTypeCheckerFunction: // atcfd
	'tcabsfn' name=ID '(' param=TypedParam ')' '-->' var=ID 'is' subtype=ID
;
FunctionDec:  // fd
	'fn' name=ID '(' params*=TypedParam[','] ')' ('-->' rettyp=ID)? '{'
		Block
	'}'
;
StaticFunctionDec:  // sfd
	'static fn' name=ID '(' params*=TypedParam[','] ')' ('-->' rettyp=ID)? '{'
		Block
	'}'
;
// Should probably parse AbstractFunctionDec in DerivedFormsDec, and _then_ report an error.
DerivedFormsDec:  // dfd
	('derived_forms'|'derived-forms') name=ID '{'
		decs+=FunctionDec
	'}'
;
Typ:
	BinderTyp|ID
;
BinderTyp:  // btp
	'openexpr' (parts+=ID['~~>'])
;
TypedParam:  // tp
	param=ID ':' typ=Typ
;
Block:
	statements+=Statement
;
ValueTerm:
	FnApp|NamespacedIdent|BinderTerm  // note var occurrences are a special case of NamespacedIdent
;
BinderTermArg: // ba
	'(' var=ID ':' typ=Typ ')'
;
BinderTermVars:
	vars*=BinderTermArg['~>']
;
BinderTerm:  // btm
 	BinderTermVars '~>' '{' body=Block '}'
;
NamespacedIdent:  // ni
	parts+=ID['.']
;
Statement:
	'pass'|ReturnStatement|VarAssign|IfType|ValueTerm
;
IfType:  // ift
	'iftype' '(' NamespacedIdent '(' NamespacedIdent ')' ')' '{'
		body=Block
	'}'
;
IfElse:  // ife
	'if' '(' condition=ValueTerm ')' '{'
		trueblock=Block
	'}' 'else' '{'
		falseblock=Block
	'}'
;
Switch: // sw
	'switch' NamespacedIdent '{'
		cases+=SwitchCase
	'}'
;
SwitchCase: // swc
	'case' case=ID '{'
		body=Block
	'}'
;
ReturnStatement:  // rs
	'return' expr=ValueTerm
;
FnApp:  // fa
	NamespacedIdent '(' args*=ValueTerm[','] ')'
;
// ClosureVarAssign:  // av
//	cvar=ID ':=' expr=ValueTerm
// ;
VarAssign:  // av
	var=ID ':=' expr=ValueTerm
;
ConstVarAssign:  // acv
	fixed=ID ':=' expr=ValueTerm
;
Comment:  // com
  	/\/\/.*$/
;

