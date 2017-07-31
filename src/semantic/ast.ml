
(* posição no arquivo fonte *)
type filePosition = int * int

and id = {
    name: string;
    pos: filePosition
}

and _type = 
	| Int
	| Float
	| Double
	| Bool
	| Char
	| String
    | Void
(*		  | Array of _type *)

and prog = 
	Prog of mainClass

and mainClass = 
	MainClass of id * mainMethod * _method list

and mainMethod = 
	MainMethod of _method

and _method = Method of {
        id: id;
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

and opType =
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

and operator = {
    pos: filePosition;
    opType: opType
}

and litType = 
    | LitBool of bool
    | LitInt of int
    | LitFloat of float
    | LitDouble of float
    | LitChar of char
    | LitString of string

and literal = {
    pos: filePosition;
    litType: litType
}

and methodCall = 
	| MethodCall of id * methodArgument list
	| MethodCallThroughType of variable * id * methodArgument list

and methodArgument = 
	| MethodArgument of expression 

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
