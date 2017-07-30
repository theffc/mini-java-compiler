
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
%token ATTR
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

/*%token NEXT_BOOLEAN
%token NEXT_DOUBLE
%token NEXT_FLOAT
%token NEXT_INT
%token NEXT_BYTE
%token NEXT_LINE*/

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
	PUBLIC STATIC VOID MAIN OPEN_PARENTESIS STRING OPEN_BRACKETS CLOSE_BRACKETS ID CLOSE_PARENTESIS OPEN_BRACES stms=statement*  CLOSE_BRACES { MainMethod(stms) }
	;

_method:
    PUBLIC STATIC t=_type name=ID OPEN_PARENTESIS ps=parameters CLOSE_PARENTESIS OPEN_BRACES stms=statement* CLOSE_BRACES 
    { 
        Method { 
        name = name;
        return_type = t;
        parameters = ps;
        body = stms;
        } 
    }
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


statement:
      s=stm_attr { s }
    | s=stm_var_declaration { s }
    | s=method_call SEMI_COLON { StmMethodCall(s) }
    | s=new_obj SEMI_COLON { StmNewObj(s) }
    | s=stm_return { s }
    | s=stm_print { s }
    | s=stm_if { s }
    | s=stm_while { s }
    ;

stm_attr:
    v=variable ATTR e=expression SEMI_COLON { StmAttr(v,e) }
    ;

stm_var_declaration:
    t=_type ids=separated_nonempty_list(COMMA, ID) SEMI_COLON { StmVarDecl(List.map(fun id -> VarDecl(id, t)) ids) }
    ;

stm_print:
      PRINT OPEN_PARENTESIS e=expression CLOSE_PARENTESIS SEMI_COLON { StmPrint(e) }
    | PRINT_LN OPEN_PARENTESIS e=expression CLOSE_PARENTESIS SEMI_COLON { StmPrintLn(e) }
    ;

stm_if:
      IF OPEN_PARENTESIS exp=expression CLOSE_PARENTESIS s=statement senao=stm_else? { StmIf(exp, [s], senao) }
    | IF OPEN_PARENTESIS exp=expression CLOSE_PARENTESIS  OPEN_BRACES stms=statement* CLOSE_BRACES senao=stm_else? { StmIf(exp, stms, senao) }
    ;

stm_else:
      ELSE s=statement { StmElse([s]) }
    | ELSE OPEN_BRACES stms=statement* CLOSE_BRACES { StmElse(stms) }
    /*| ELSE IF OPEN_PARENTESIS exp=expression CLOSE_PARENTESIS s=statement another=stm_else? { StmElseIf(exp, [s], another) }
    | ELSE IF OPEN_PARENTESIS exp=expression CLOSE_PARENTESIS OPEN_BRACES stms=statement* CLOSE_BRACES another=stm_else? { StmElseIf(exp, stms, another) }*/
    ;

stm_return:
    RETURN e=expression SEMI_COLON { StmReturn(e) }
    ;

stm_while:
    WHILE OPEN_PARENTESIS e=expression CLOSE_PARENTESIS OPEN_BRACES s=statement* CLOSE_BRACES { StmWhile(e, s) }
    ;


expression:
   | e1=expression o=operator e2=expression { ExpOperator(e1,o,e2) }
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
      id=ID { Var(id) }
    | id=ID OPEN_BRACKETS e=expression CLOSE_BRACKETS { VarArray(id, e) }
    | ID PERIOD v=variable { v }
    ;

literal:
    l=LIT_BOOL { LitBool(l) }
    | l=LIT_INT { LitInt(l) }
    | l=LIT_FLOAT { LitFloat(l) }
    | l=LIT_DOUBLE { LitDouble(l) }
    | l=LIT_CHAR { LitChar(l) }
    | l=LIT_STRING { LitString(l) }
    ;


/* ESSA PARTE DE CHAMADA DE METODO TA DANDO MUITO PROBLEMA */

method_call:
      name=ID OPEN_PARENTESIS args=method_args CLOSE_PARENTESIS { MethodCall(name, args) }
      | receiver=variable PERIOD name=ID OPEN_PARENTESIS args=method_args CLOSE_PARENTESIS { MethodCallThroughType(receiver, name, args) }
    ;

method_args:
    | exprs=separated_list(COMMA, expression) {  List.map (fun expr -> MethodArgument(expr)) exprs }

new_obj:
    NEW m=method_call { NewObj(m) }
    ;

