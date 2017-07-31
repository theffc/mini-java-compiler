
%{
open Ast

let make_pos (pos: Lexing.position): Ast.filePosition =
    let line = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol in
    (line, col)

let make_id id =
    let (n, pos) = id in
    {name = n; pos = make_pos pos}

%}

%token <string * Lexing.position> ID

%token <bool * Lexing.position> LIT_BOOL
%token <int * Lexing.position> LIT_INT
%token <float * Lexing.position> LIT_FLOAT
%token <float * Lexing.position> LIT_DOUBLE
%token <string * Lexing.position> LIT_STRING
%token <char * Lexing.position> LIT_CHAR

%token <Lexing.position> PUBLIC
%token <Lexing.position> PRIVATE
%token <Lexing.position> STATIC
%token <Lexing.position> MAIN
%token <Lexing.position> CLASS
%token <Lexing.position> NEW
%token <Lexing.position> RETURN
%token <Lexing.position> VOID
%token <Lexing.position> INT
%token <Lexing.position> CHAR
%token <Lexing.position> FLOAT
%token <Lexing.position> DOUBLE
%token <Lexing.position> STRING
%token <Lexing.position> BOOLEAN
%token <Lexing.position> IF
%token <Lexing.position> ELSE
%token <Lexing.position> FOR
%token <Lexing.position> DO
%token <Lexing.position> WHILE
%token <Lexing.position> SWITCH
%token <Lexing.position> CASE
%token <Lexing.position> DEFAULT
%token <Lexing.position> BREAK
%token <Lexing.position> CONTINUE
%token <Lexing.position> THIS
%token <Lexing.position> NULL
%token <Lexing.position> OP_INCR
%token <Lexing.position> OP_DECR
%token <Lexing.position> OP_ADD
%token <Lexing.position> OP_SUB
%token <Lexing.position> OP_MUL
%token <Lexing.position> OP_DIV
%token <Lexing.position> OP_MOD
%token <Lexing.position> OP_NOT
%token <Lexing.position> OP_AND
%token <Lexing.position> OP_OR
%token <Lexing.position> OP_LESS
%token <Lexing.position> OP_LESS_EQUAL
%token <Lexing.position> OP_EQUAL
%token <Lexing.position> OP_DIF
%token <Lexing.position> OP_GREATER
%token <Lexing.position> OP_GREATER_EQUAL
%token <Lexing.position> ATTR
%token <Lexing.position> OPEN_PARENTESIS
%token <Lexing.position> CLOSE_PARENTESIS
%token <Lexing.position> OPEN_BRACKETS
%token <Lexing.position> CLOSE_BRACKETS
%token <Lexing.position> OPEN_BRACES
%token <Lexing.position> CLOSE_BRACES
%token <Lexing.position> SEMI_COLON
%token <Lexing.position> COMMA
%token <Lexing.position> PERIOD
%token <Lexing.position> COLON
%token <Lexing.position> PRINT
%token <Lexing.position> PRINT_LN
%token <Lexing.position> IMPORT_SCANNER

/*%token <Lexing.position> NEXT_BOOLEAN
%token <Lexing.position> NEXT_DOUBLE
%token <Lexing.position> NEXT_FLOAT
%token <Lexing.position> NEXT_INT
%token <Lexing.position> NEXT_BYTE
%token <Lexing.position> NEXT_LINE*/

%token EOF

%left OP_OR
%left OP_AND
%left OP_EQUAL OP_DIF
%left OP_GREATER OP_GREATER_EQUAL OP_LESS OP_LESS_EQUAL
%left OP_ADD OP_SUB
%left OP_MULT OP_DIV OP_MOD
/* %right OP_POTENCIA */
 


%start <Ast.prog> prog

%%
   
prog:
	| c=main_class EOF	{ Prog(c) }
    | IMPORT_SCANNER SEMI_COLON c=main_class EOF { Prog(c) }
    ;

main_class:
    | PUBLIC CLASS id=ID OPEN_BRACES main=main_method methods=_method* CLOSE_BRACES 
        { MainClass(make_id id, main, methods) }
    ;

main_method:
	| PUBLIC STATIC VOID MAIN pos=OPEN_PARENTESIS STRING OPEN_BRACKETS CLOSE_BRACKETS ID CLOSE_PARENTESIS OPEN_BRACES stms=statement*  CLOSE_BRACES 
        { MainMethod (
            Method {id={name="main"; pos= make_pos pos}; return_type = Ast.Void; parameters = []; body = stms;} 
        )} 
    ;

_method:
    | PUBLIC STATIC t=_type id=ID OPEN_PARENTESIS ps=parameters CLOSE_PARENTESIS OPEN_BRACES stms=statement* CLOSE_BRACES 
        { Method {id = make_id id; return_type = t; parameters = ps; body = stms;} }
	;

parameters:
    | ps=separated_list(COMMA, parameter) { ps }
parameter:
    | t=_type id=ID { Parameter(make_id id, t)}
    ;

_type:
    | INT { Int }
    | DOUBLE { Double }
    | FLOAT { Float }
    | CHAR { Char }
    | STRING { String }
    ;


statement:
    | s=stm_attr { s }
    | s=stm_var_declaration { s }
    | s=method_call SEMI_COLON { StmMethodCall(s) }
    | s=new_obj SEMI_COLON { StmNewObj(s) }
    | s=stm_return { s }
    | s=stm_print { s }
    | s=stm_if { s }
    | s=stm_while { s }
    ;

stm_attr:
    | v=variable ATTR e=expression SEMI_COLON 
        { StmAttr(v,e) }
    ;

stm_var_declaration:
    | t=_type ids=separated_nonempty_list(COMMA, ID) SEMI_COLON 
        { StmVarDecl(List.map(fun id -> VarDecl(make_id id, t)) ids) }
    ;

stm_print:
    | PRINT OPEN_PARENTESIS e=expression CLOSE_PARENTESIS SEMI_COLON 
        { StmPrint(e) }

    | PRINT_LN OPEN_PARENTESIS e=expression CLOSE_PARENTESIS SEMI_COLON 
        { StmPrintLn(e) }
    ;

stm_if:
    | IF OPEN_PARENTESIS exp=expression CLOSE_PARENTESIS s=statement senao=stm_else? 
        { StmIf(exp, [s], senao) }

    | IF OPEN_PARENTESIS exp=expression CLOSE_PARENTESIS  OPEN_BRACES stms=statement* CLOSE_BRACES senao=stm_else? 
        { StmIf(exp, stms, senao) }
    ;

stm_else:
    | ELSE s=statement 
        { StmElse([s]) }

    | ELSE OPEN_BRACES stms=statement* CLOSE_BRACES 
        { StmElse(stms) }
    ;

stm_return:
    | RETURN e=expression SEMI_COLON { StmReturn(e) }
    ;

stm_while:
    | WHILE OPEN_PARENTESIS e=expression CLOSE_PARENTESIS OPEN_BRACES s=statement* CLOSE_BRACES
        { StmWhile(e, s) }
    ;


expression:
    | e1=expression o=operator e2=expression 
        { ExpOperator(e1,o,e2) }
    | t=term {ExpTerm t} 
    | OP_NOT t=term { ExpNotTerm t }
    | OP_SUB t=term { ExpMinusTerm t } 
    | OPEN_PARENTESIS e=expression CLOSE_PARENTESIS { e }
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
    | v=variable { TermVariable(v) }
    | m=method_call { TermMethodCall(m) }
    | n=new_obj { TermNewObj(n) }
    ;

variable:
    | id=ID { Var(make_id id) }
    | id=ID OPEN_BRACKETS e=expression CLOSE_BRACKETS { VarArray(make_id id, e) }
    | ID PERIOD v=variable { v }
    ;

literal:
    | l=LIT_BOOL 
        { let (t, p) = l in {pos=make_pos p; litType=LitBool(t)} }
    | l=LIT_INT 
        { let (t, p) = l in {pos=make_pos p; litType=LitInt(t)} }
    | l=LIT_FLOAT 
        { let (t, p) = l in {pos=make_pos p; litType=LitFloat(t)} }
    | l=LIT_DOUBLE 
        { let (t, p) = l in {pos=make_pos p; litType=LitDouble(t)} }
    | l=LIT_CHAR 
        { let (t, p) = l in {pos=make_pos p; litType=LitChar(t)} }
    | l=LIT_STRING 
        { let (t, p) = l in {pos=make_pos p; litType=LitString(t)} }
    ;


/* ESSA PARTE DE CHAMADA DE METODO TA DANDO MUITO PROBLEMA */

method_call:
    | id=ID OPEN_PARENTESIS args=method_args CLOSE_PARENTESIS 
        { MethodCall(make_id id, args) }

    | receiver=variable PERIOD id=ID OPEN_PARENTESIS args=method_args CLOSE_PARENTESIS
        { MethodCallThroughType(receiver, make_id id, args) }
    ;

method_args:
    | exprs=separated_list(COMMA, expression)
        {  List.map (fun expr -> MethodArgument(expr)) exprs }

new_obj:
    | NEW m=method_call { NewObj(m) }
    ;

