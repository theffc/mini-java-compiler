type id = string

and _type = 
	  Int
	| Float
	| Double
	| Bool
	| Char
	| String
(*		  | Array of _type *)

and prog = Prog of mainClass

and mainClass = MainClass of id * mainClassBody
and mainClassBody = MainClassBody of mainMethod * _method list

and mainMethod = MainMethod

and _method = Method of id * _type * parameter list * expression

and parameter = Parameter of id * _type

(* and statementsBlock = StatementsBlock of statement list
and statement = Statement of  *)


and operator =
	  OpAdd
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
      LitBool of bool
    | LitInt of int
    | LitFloat of float
    | LitDouble of float
    | LitChar of char
    | LitString of string


and methodCall = MethodCall of id * methodArgument list

and methodArgument = MethodArgument of expression 

and expression = 
	  ExpOperator of expression * operator * expression
	| ExpTerm of term
	| ExpNotTerm of term

and term = 
	TermLiteral of literal
	| TermId of id
	| TermMethodCall of methodCall



