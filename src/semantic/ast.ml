
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

and 'exp prog = 
	Prog of 'exp mainClass

and 'exp mainClass = 
	MainClass of id * 'exp mainMethod * 'exp _method list

and 'exp mainMethod = 
	MainMethod of 'exp _method

and 'exp _method = 
    Method of {
        id: id;
        return_type: _type;
        parameters: parameter list;
        body: 'exp statement list;
    }

and parameter = 
	Parameter of id * _type

(* and statementsBlock = StatementsBlock of statement list
and statement = Statement of  *)

and 'exp statement = 
	| StmAttr of 'exp variable * 'exp
    | StmVarDecl of varDeclaration list
    | StmMethodCall of 'exp methodCall
    | StmPrint of 'exp
    | StmPrintLn of 'exp
    | StmIf of 'exp * 'exp statement list * 'exp stmElse option
    | StmReturn of 'exp
    | StmWhile of 'exp * 'exp statement list
    | StmNewObj of 'exp newObj

and 'exp stmElse = 
	StmElse of 'exp statement list

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

and 'exp methodCall = 
	| MethodCall of id * 'exp methodArgument list
	| MethodCallThroughType of 'exp variable * id * 'exp methodArgument list

and 'exp methodArgument = 
	| MethodArgument of 'exp 

and 'exp term = 
	| TermLiteral of literal
	| TermVariable of 'exp variable
	| TermMethodCall of 'exp methodCall
	| TermNewObj of 'exp newObj

and 'exp variable =
	| Var of id
	| VarArray of id * 'exp

and 'exp newObj = 
	NewObj of 'exp methodCall
