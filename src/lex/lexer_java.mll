(* Instrucoes de uso (pelo terminal):

   ocamllex this-file-name.mll
   ocamlc -c this-file-name.ml
   rlwrap ocaml
   #use "carregador.ml";;
   lex "arquivo.txt";;
*)

(* docs do ocamllex e ocamlyacc:  caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html *)


{ (* HEADER *)

open Lexing
open Printf

let incr_num_linha lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                         pos_lnum = pos.pos_lnum + 1;
                         pos_bol = pos.pos_cnum;
                       }

let msg_erro lexbuf c =
  let pos = lexbuf.lex_curr_p in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  sprintf "%d-%d: caracter desconhecido %c" lin col c

let erro lin col msg =
  let mensagem = sprintf "%d-%d: %s" lin col msg in
     failwith mensagem

let msg_erro_comentario lexbuf s =
  let pos = lexbuf.lex_curr_p in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  sprintf "%d-%d: final de comentario %s utilizado errado" lin col s


type tokens = AParen
            | FParen
            | Atrib
            | LitBoolean of bool
            | LitInt of int
            | LitDouble of float
            | LitString of string
            | LitChar of char
            | ID of string
            | EOF
            | Main
            | Public
            | Private
            | Static
            | Class
            | New
            | Return
            | Void
            | Int
            | Char
            | Float
            | Double
            | String
            | Boolean
            | If
            | Else
            | For
            | Do
            | While
            | Switch
            | Case
            | Default
            | Break
            | Continue
            | This
            | Null
            | OpIncr
            | OpDecr
            | OpSoma
            | OpSub
            | OpMul
            | OpDiv
            | OpMod
            | OpNao
            | OpE
            | OpOu
            | OpMenor
            | OpMenorIgual
            | OpIgual
            | OpDiferente
            | OpMaior
            | OpMaiorIgual
            | AColc
            | FColc
            | AChave
            | FChave
            | PTV
            | Virg
            | Ponto
            | DoisPontos
            | Imprime
            | ImprimeSalta
            | ImportScanner
            | Scanner
            | SystemIn
            | LeBool
            | LeDouble
            | LeFloat
            | LeInt
            | LeByte
            | LeString

}


(* EXPRESSOES REGULARES *)

let digito = ['0' - '9']
let inteiro = digito+

let letra = ['a' - 'z' 'A' - 'Z']
let identificador = letra ( letra | digito | '_')*
let character = ['_' 'a'-'z' 'A'-'Z' '0'-'9']

let brancos = [' ' '\t']+
let novalinha = '\r' | '\n' | "\r\n"

let comentario = "//" [^ '\r' '\n' ]*

let booleano = "true" | "false"

let numeroFloat = digito+ '.' digito+

let strings = '"' identificador* digito* '"'


(* RULES or ENTRY POINTS *)

rule token = parse
    brancos { token lexbuf } (* ignora espacos *)
  | novalinha { incr_num_linha lexbuf; token lexbuf } (* ignora fim de linha *)
  | comentario { token lexbuf } (* ignora comentario *)
  | "/*" { comentario_bloco 0 lexbuf }
  | "*/" { failwith (msg_erro_comentario lexbuf "*/"); } (* achou um fechamento de comentario do nada *)
  | booleano as bol { let value = bool_of_string bol in
                    LitBoolean (value)}
  | inteiro as num { let numero = int_of_string num in
                   LitInt (numero) }
  | numeroFloat as num { let value = float_of_string num in LitDouble (value) }
  | '"'  { let pos = lexbuf.lex_curr_p in
           let lin = pos.pos_lnum
           and col = pos.pos_cnum - pos.pos_bol - 1 in
           let buffer = Buffer.create 1 in
           let str = leia_string lin col buffer lexbuf in
           LitString str }
  | '\'' (character as c) '\'' { LitChar (c) }
  | "public static void main" { Main }
  | "public" { Public }
  | "private" { Private }
  | "static" { Static }
  | "class" { Class }
  | "new" { New }
  | "return" { Return }
  | "void" { Void }
  | "int" { Int }
  | "char" { Char }
  | "float" { Float }
  | "double" { Double }
  | "String" { String }
  | "boolean" { Boolean }
  | "if" { If }
  | "else" { Else }
  | "for" { For }
  | "do" { Do }
  | "while" { While }
  | "switch" { Switch }
  | "case" { Case }
  | "default" { Default }
  | "break" { Break }
  | "continue" { Continue }
  | "this" { This }
  | "null" { Null }
  | "++" { OpIncr }
  | "--" { OpDecr }
  | '+' { OpSoma }
  | '-' { OpSub }
  | '*' { OpMul }
  | '/' { OpDiv }
  | '%' { OpMod }
  | '!' { OpNao }
  | "&&" { OpE }
  | "||" { OpOu }
  | '<' { OpMenor }
  | "<=" { OpMenorIgual }
  | "==" { OpIgual }
  | "!=" { OpDiferente }
  | '>' { OpMaior }
  | ">=" { OpMaiorIgual }
  | '=' { Atrib }
  | '(' { AParen }
  | ')' { FParen }
  | '[' { AColc }
  | ']' { FColc }
  | '{' { AChave }
  | '}' { FChave }
  | ';' { PTV }
  | ',' { Virg }
  | '.' { Ponto }
  | ':' { DoisPontos }
  | "System.out.print" { Imprime }
  | "System.out.println" { ImprimeSalta }
  | "import java.util.Scanner" { ImportScanner }
  | "Scanner" { Scanner }
  | "System.in" { SystemIn }
  | "nextBoolean" { LeBool }
  | "nextDouble" { LeDouble }
  | "nextFloat" { LeFloat }
  | "nextInt" { LeInt }
  | "nextByte" { LeByte }
  | "nextLine" { LeString }
  | identificador as id { ID (id) }
  | _ as c { failwith (msg_erro lexbuf c); }
  | eof { EOF }

(* regra para tratar comentarios de varias linhas *)
and comentario_bloco n = parse
    "*/"  { if n=0 then token lexbuf
           else comentario_bloco (n-1) lexbuf }
  | "/*"  { comentario_bloco (n+1) lexbuf }
  | novalinha { incr_num_linha lexbuf; comentario_bloco n lexbuf }
  | _     { comentario_bloco n lexbuf }
  | eof   { failwith "Comentario nao fechado" }

(* regra para tratar strings literais *)
and leia_string lin col buffer = parse
  '"'       { Buffer.contents buffer}
| "\\t"     { Buffer.add_char buffer '\t'; leia_string lin col buffer lexbuf }
| "\\n"     { Buffer.add_char buffer '\n'; leia_string lin col buffer lexbuf }
| '\\' '"'  { Buffer.add_char buffer '"'; leia_string lin col buffer lexbuf }
| '\\' '\\' { Buffer.add_char buffer '\\'; leia_string lin col buffer lexbuf }
| _ as c    { Buffer.add_char buffer c; leia_string lin col buffer lexbuf }
| eof       { erro lin col "A string nao foi fechada"}
