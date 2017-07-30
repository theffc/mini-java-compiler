
type id = string

and _type = 
	| Int
	| Float
	| Double
	| Bool
	| Char
	| String
    | Void
(*		  | Array of _type *)

and pos = Lexing.position (* tipo e posição no arquivo fonte *)

and prog = 
	Prog of mainClass

and mainClass = 
	MainClass of id * mainMethod * _method list

and mainMethod = 
	MainMethod of _method

and _method = Method of {
        name: id;
        return_type: _type;
        parameters: parameter list;
        body: statement list;
    }

and parameter = 
	Parameter of id * _type

(* and statementsBlock = StatementsBlock of statement list
and statement = Statement of  *)

and statement = 
	| StmAttr of variable * expression
    | StmVarDecl of varDeclaration list
    | StmMethodCall of methodCall
    | StmPrint of expression
    | StmPrintLn of expression
    | StmIf of expression * statement list * stmElse option
    | StmReturn of expression
    | StmWhile of expression * statement list
    | StmNewObj of newObj

and stmElse = 
	StmElse of statement list
	(* | StmElseIf of expression * statement list * stmElse option *)

and varDeclaration =
	VarDecl of id * _type

and operator =
	| OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpAnd
    | OpOr
    | OpLess
    | OpLessEqual
    | OpEqual
    | OpDif
    | OpGreater
    | OpGreaterEqual

and literal =
    | LitBool of bool * pos
    | LitInt of int * pos
    | LitFloat of float * pos
    | LitDouble of float * pos
    | LitChar of char * pos
    | LitString of string * pos

and methodCall = 
	| MethodCall of id * methodArgument list
	| MethodCallThroughType of variable * id * methodArgument list

and methodArgument = 
	MethodArgument of expression 

and expression = 
	| ExpOperator of expression * operator * expression
	| ExpTerm of term
	| ExpNotTerm of term
	| ExpMinusTerm of term

and term = 
	| TermLiteral of literal
	| TermVariable of variable
	| TermMethodCall of methodCall
	| TermNewObj of newObj

and variable =
	| Var of id
	| VarArray of id * expression

and newObj = 
	NewObj of methodCall
