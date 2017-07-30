(* Instrucoes de uso (pelo terminal):

   ocamllex this-file-name.mll
   ocamlc -c this-file-name.ml
   rlwrap ocaml
   #use "carregador.ml";;
   lex "arquivo.txt";;
*)

(* docs do ocamllex e ocamlyacc:  caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html *)


{ (* HEADER *)

open Parser
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

let pos_atual lexbuf = lexbuf.lex_start_p

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

  (* LITERALS *)
  | booleano as bol { let value = bool_of_string bol in
                    LIT_BOOL (value, pos_atual lexbuf) }
  | inteiro as num { let numero = int_of_string num in
                   LIT_INT (numero, pos_atual lexbuf) }
  | numeroFloat as num { let value = float_of_string num in LIT_DOUBLE (value, pos_atual lexbuf) }
  | '"'  { let pos = lexbuf.lex_curr_p in
           let lin = pos.pos_lnum
           and col = pos.pos_cnum - pos.pos_bol - 1 in
           let buffer = Buffer.create 1 in
           let str = leia_string lin col buffer lexbuf in
           LIT_STRING (str, pos_atual lexbuf) }
  | '\'' (character as c) '\'' { LIT_CHAR (c, pos_atual lexbuf) }

  (* KEYWORDS *)
  | "public" { PUBLIC (pos_atual lexbuf) }
  | "private" { PRIVATE (pos_atual lexbuf) }
  | "static" { STATIC (pos_atual lexbuf) }
  | "main" { MAIN (pos_atual lexbuf) }
  | "class" { CLASS (pos_atual lexbuf) }
  | "new" { NEW (pos_atual lexbuf) }
  | "return" { RETURN (pos_atual lexbuf) }
  | "void" { VOID (pos_atual lexbuf) }
  | "int" { INT (pos_atual lexbuf) }
  | "char" { CHAR (pos_atual lexbuf) }
  | "float" { FLOAT (pos_atual lexbuf) }
  | "double" { DOUBLE (pos_atual lexbuf) }
  | "String" { STRING (pos_atual lexbuf) }
  | "boolean" { BOOLEAN (pos_atual lexbuf) }
  | "if" { IF (pos_atual lexbuf) }
  | "else" { ELSE (pos_atual lexbuf) }
  | "for" { FOR (pos_atual lexbuf) }
  | "do" { DO (pos_atual lexbuf) }
  | "while" { WHILE (pos_atual lexbuf) }
  | "switch" { SWITCH (pos_atual lexbuf) }
  | "case" { CASE (pos_atual lexbuf) }
  | "default" { DEFAULT (pos_atual lexbuf) }
  | "break" { BREAK (pos_atual lexbuf) }
  | "continue" { CONTINUE (pos_atual lexbuf) }
  | "this" { THIS (pos_atual lexbuf) }
  | "null" { NULL (pos_atual lexbuf) }
  | "++" { OP_INCR (pos_atual lexbuf) }
  | "--" { OP_DECR (pos_atual lexbuf) }
  | '+' { OP_ADD (pos_atual lexbuf) }
  | '-' { OP_SUB (pos_atual lexbuf) }
  | '*' { OP_MUL (pos_atual lexbuf) }
  | '/' { OP_DIV (pos_atual lexbuf) }
  | '%' { OP_MOD (pos_atual lexbuf) }
  | '!' { OP_NOT (pos_atual lexbuf) }
  | "&&" { OP_AND (pos_atual lexbuf) }
  | "||" { OP_OR (pos_atual lexbuf) }
  | '<' { OP_LESS (pos_atual lexbuf) }
  | "<=" { OP_LESS_EQUAL (pos_atual lexbuf) }
  | "==" { OP_EQUAL (pos_atual lexbuf) }
  | "!=" { OP_DIF (pos_atual lexbuf) }
  | '>' { OP_GREATER (pos_atual lexbuf) }
  | ">=" { OP_GREATER_EQUAL (pos_atual lexbuf) }
  | '=' { ATTR (pos_atual lexbuf) }
  | '(' { OPEN_PARENTESIS (pos_atual lexbuf) }
  | ')' { CLOSE_PARENTESIS (pos_atual lexbuf) }
  | '[' { OPEN_BRACKETS (pos_atual lexbuf) }
  | ']' { CLOSE_BRACKETS (pos_atual lexbuf) }
  | '{' { OPEN_BRACES (pos_atual lexbuf) }
  | '}' { CLOSE_BRACES (pos_atual lexbuf) }
  | ';' { SEMI_COLON (pos_atual lexbuf) }
  | ',' { COMMA (pos_atual lexbuf) }
  | '.' { PERIOD (pos_atual lexbuf) }
  | ':' { COLON (pos_atual lexbuf) }
  | "System.out.print" { PRINT (pos_atual lexbuf) }
  | "System.out.println" { PRINT_LN (pos_atual lexbuf) }
  | "import java.util.Scanner" { IMPORT_SCANNER (pos_atual lexbuf) }

(*
  | "nextBoolean" { NEXT_BOOLEAN }
  | "nextDouble" { NEXT_DOUBLE }
  | "nextFloat" { NEXT_FLOAT }
  | "nextInt" { NEXT_INT }
  | "nextByte" { NEXT_BYTE }
  | "nextLine" { NEXT_LINE }
*)

  | identificador as id { ID (id, pos_atual lexbuf) }
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
