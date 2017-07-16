
%{
  open Ast
%}

%token <string> ID

%token <bool> LIT_BOOL
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <float> LIT_DOUBLE
%token <string> LIT_STRING
%token <char> LIT_CHAR

%token PUBLIC
%token PRIVATE
%token STATIC
%token MAIN
%token CLASS
%token NEW
%token RETURN
%token VOID
%token INT
%token CHAR
%token FLOAT
%token DOUBLE
%token STRING
%token BOOLEAN
%token IF
%token ELSE
%token FOR
%token DO
%token WHILE
%token SWITCH
%token CASE
%token DEFAULT
%token BREAK
%token CONTINUE
%token THIS
%token NULL
%token OP_INCR
%token OP_DECR
%token OP_ADD
%token OP_SUB
%token OP_MUL
%token OP_DIV
%token OP_MOD
%token OP_NOT
%token OP_AND
%token OP_OR
%token OP_LESS
%token OP_LESS_EQUAL
%token OP_EQUAL
%token OP_DIF
%token OP_GREATER
%token OP_GREATER_EQUAL
%token ATRIB
%token OPEN_PARENTESIS
%token CLOSE_PARENTESIS
%token OPEN_BRACKETS
%token CLOSE_BRACKETS
%token OPEN_BRACES
%token CLOSE_BRACES
%token SEMI_COLON
%token COMMA
%token PERIOD
%token COLON
%token PRINT
%token PRINT_LN
%token IMPORT_SCANNER
%token SCANNER
%token SYSTEM_IN
%token NEXT_BOOLEAN
%token NEXT_DOUBLE
%token NEXT_FLOAT
%token NEXT_INT
%token NEXT_BYTE
%token NEXT_LINE
%token EOF

%left OP_OR
%left OP_AND
%left OP_EQUAL  OP_DIF
%left OP_GREATER OP_GREATER_EQUAL OP_LESS OP_LESS_EQUAL
%left OP_ADD OP_SUB
%left OP_MULT OP_DIV OP_MOD
/* %right OP_POTENCIA */
 


%start <Ast.prog> prog

%%
   
prog:
	c=main_class EOF	{ Prog(c) }
    | IMPORT_SCANNER SEMI_COLON c=main_class EOF { Prog(c) }
    ;

main_class:
    PUBLIC CLASS id=ID OPEN_BRACES body=main_class_body CLOSE_BRACES { MainClass(id, body) }
    ;

main_class_body:
	main=main_method ms=_method*  { MainClassBody(main, ms) }
	;

main_method:
	PUBLIC STATIC VOID MAIN OPEN_PARENTESIS STRING OPEN_BRACKETS CLOSE_BRACKETS ID CLOSE_PARENTESIS OPEN_BRACES CLOSE_BRACES { MainMethod }
	;

_method:
    PUBLIC STATIC t=_type name=ID OPEN_PARENTESIS ps=parameters CLOSE_PARENTESIS OPEN_BRACES e=expression CLOSE_BRACES { Method(name, t, ps, e) }
	;

parameters:
    ps=separated_list(COMMA, parameter) { ps }
parameter:
    t=_type id=ID { Parameter(id, t)}
    ;

_type:
    INT { Int }
    | DOUBLE { Double }
    | FLOAT { Float }
    | CHAR { Char }
    | STRING { String }
    ;


/* statement:
    stm_if {}
    ;

stm_if:
    IF OPEN_PARENTESIS expression CLOSE_PARENTESIS  OPEN_BRACES stms=statement* senao=stm_senao? CLOSE_BRACES { StmSe(e,stms,senao)}
    ;

stm_else:
    ELSE OPEN_PARENTESIS expression CLOSE_PARENTESIS OPEN_BRACES stms=stm_list* CLOSE_BRACES { StmElse }
*/

expression:
   | e1=expression o=operator e2=expression { ExpOperator(e1,o,e2) }
   | t=term {ExpTerm t} 
   | OP_NOT t=term { ExpNotTerm t}
   /*| OPEN_PARENTESIS e=expression CLOSE_PARENTESIS { e }*/
   ;

operator:
    | OP_ADD { OpAdd }
    | OP_SUB { OpSub }
    | OP_MUL { OpMul }
    | OP_DIV { OpDiv }
    | OP_MOD { OpMod }
    | OP_AND { OpAnd }
    | OP_OR { OpOr }
    | OP_LESS { OpLess }
    | OP_LESS_EQUAL { OpLessEqual }
    | OP_EQUAL { OpEqual }
    | OP_DIF { OpDif }
    | OP_GREATER { OpGreater }
    | OP_GREATER_EQUAL { OpGreaterEqual }
    ;

term:
    | l=literal { TermLiteral(l) }
    | id=ID { TermId(id) }
    | m=method_call { TermMethodCall(m) }
    ;

literal:
    l=LIT_BOOL { LitBool(l) }
    | l=LIT_INT { LitInt(l) }
    | l=LIT_FLOAT { LitFloat(l) }
    | l=LIT_DOUBLE { LitDouble(l) }
    | l=LIT_CHAR { LitChar(l) }
    | l=LIT_STRING { LitString(l) }
    ;

method_call:
    | id=ID OPEN_PARENTESIS args=method_args CLOSE_PARENTESIS { MethodCall(id, args) }
    ;

method_args:
    | exprs=separated_list(COMMA, expression) {  List.map (fun expr -> MethodArgument(expr)) exprs}
