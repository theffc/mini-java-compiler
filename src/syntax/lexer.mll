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

  | "public" { Public }
  | "class" { Class }
  | "{" { AChave }
  | "}" { FChave }
  | ';' { PTV }
  | "import java.util.Scanner" { ImportScanner }
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
