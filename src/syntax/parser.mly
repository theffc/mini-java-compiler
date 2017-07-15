
%{
  open Ast
%}

%token PTV
%token ImportScanner
%token AChave
%token FChave
%token EOF
%token Public
%token Class
%token <string> ID

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
    | ImportScanner PTV dc=decl_class AChave FChave EOF { Prog (dc) }
    ;

decl_class:
    | Public Class id=ID { DeclClass(id) }
    ;


