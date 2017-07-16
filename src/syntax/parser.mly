
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
%token PERION
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
%token ALGORITMO
%token SOMA SUB MULT DIVISAO MOD
%token POTENCIA
%token APAR
%token FPAR
%token ACOL
%token FCOL
%token IGUAL
%token DIFERENTE
%token MAIOR
%token MAIORIGUAL
%token MENOR
%token MENORIGUAL
%token E OU NAO XOU
%token EOF
%token ATRIB
%token DECLARA 
%token PTV
%token VAR
%token INTEIRO
%token LOGICO
%token REAL
%token ATE 
%token CARACTER
%token CASO
%token DE
%token VIRGULA
%token INICIO
%token FUNCAO
%token FIMFUNCAO
%token FIMPARA 
%token FIMSE
%token FIMENQUANTO
%token FIMESCOLHA
%token SE
%token ENTAO
%token SENAO
%token ENQUANTO
%token ESCOLHA
%token ESCREVA
%token ESCREVAL
%token LEIA
%token FACA
%token OUTROCASO
%token PARA 
%token PASSO 
%token RETORNE
%token VERDADEIRO
%token FALSO
%token FIMALGORITMO


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
	main=main_method  { MainClassBody(main) }
    | main=main_method ms=_method* { MainClassBodyWithMethods(main, ms) }
	;

main_method:
	PUBLIC STATIC VOID MAIN OPEN_PARENTESIS STRING OPEN_BRACKETS CLOSE_BRACKETS ID CLOSE_PARENTESIS OPEN_BRACES CLOSE_BRACES { MainMethod }
	;

_method:
	PUBLIC STATIC t=_type name=ID OPEN_PARENTESIS CLOSE_PARENTESIS OPEN_BRACES CLOSE_BRACES { Method(name, t) }
    | PUBLIC STATIC t=_type name=ID OPEN_PARENTESIS ps=parameter* CLOSE_PARENTESIS OPEN_BRACES CLOSE_BRACES { MethodWithParameters(name, t, ps) }
	;

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


