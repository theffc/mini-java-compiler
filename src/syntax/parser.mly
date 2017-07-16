
%{
  open Ast
%}

%token <string> ID
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

/*
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> LITSTRING
%token <string> LITCHAR

%left OU XOU
%left E
%left IGUAL DIFERENTE
%left MAIOR MAIORIGUAL MENOR MENORIGUAL
%left SOMA SUB
%left MULT DIVISAO MOD
%right POTENCIA
*/
 


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
    PUBLIC STATIC t=_type name=ID OPEN_PARENTESIS ps=parameters CLOSE_PARENTESIS OPEN_BRACES CLOSE_BRACES { Method(name, t, ps) }
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


