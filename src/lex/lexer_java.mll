(* Instrucoes de uso (pelo terminal):

   ocamllex this-file-name.mll
   ocamlc -c this-file-name.ml
   rlwrap ocaml
   #use "carregador.ml";;
   lex "arquivo.txt";;
*)

(* docs for ocamllex and ocamlyacc:  caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html *)


{ (* HEADER *)

open Lexing
open Printf

let incr_num_linha lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                         pos_lnum = pos.pos_lnum + 1;
                         pos_bol = pos.pos_cnum;
                       }

let incr_num_col lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                         pos_lnum = pos.pos_lnum;
                         pos_bol = pos.pos_bol + 1;
                       }

let msg_erro lexbuf c =
  let pos = lexbuf.lex_curr_p in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  sprintf "%d-%d: caracter desconhecido %c" lin col c

let msg_erro_comentario lexbuf s =
  let pos = lexbuf.lex_curr_p in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  sprintf "%d-%d: final de comentario %s utilizado errado" lin col s

(* TODO: adicionar aqui os tipos de tokens/identificadores

  igual ao codigo do professor:

  type tokens = APAR
              | FPAR
              | ATRIB
              | IF
              | WHILE
              | MAIS
              | LITINT of int
              | LITSTRING of string
              | ID of string
              | EOF
  }
 *)

}


(* EXPRESSOES REGULARES *)

let digito = ['0' - '9']

let identificador = ['a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

let booleano = "true" | "false"

let numeroFloat = digito+ '.' digito+

let strings = '"' identificador* digito* '"'

let character = ['_' 'a'-'z' 'A'-'Z' '0'-'9']


(* RULES or ENTRY POINTS *)

rule token = parse
  [' ' '\t' ] { incr_num_col lexbuf; token lexbuf } (* ignora
                                                         espa os *)
  | ['\n'] { incr_num_linha lexbuf; token lexbuf } (* ignora
                                                    fim de linha *)
  | "//" { simple_comment lexbuf } (* ignora comentario *)
  | "/*" { full_comment lexbuf }
  | "*/" { failwith (msg_erro_comentario lexbuf "*/"); }
  | booleano as bol { let value = bool_of_string bol in
                    LitBoolean (value)}
  | digito+ as num { let numero = int_of_string num in
                   LitInt (numero) }
  | numeroFloat as num { let value = float_of_string num in
                       LitDouble (value) }
  | '"' { let buffer = Buffer.create 1 in
        LitString (cadeia buffer lexbuf) }
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
  | ';' { PTVirg }
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
  | identificador as id { Ident (id) }
  | _ as c { failwith (msg_erro lexbuf c); }
  | eof { EOF }

(* regra para tratar comentarios de varias linhas *)
and full_comment = parse
  | "*/" { token lexbuf }
  | '\n'{ incr_num_linha lexbuf; full_comment lexbuf }
  | _ { full_comment lexbuf}

(* regra para tratar comentarios de apenas 1 linha *)
and simple_comment = parse
  | '\n' { incr_num_linha lexbuf; token lexbuf }
  | _ { simple_comment lexbuf }

(* regra para tratar strings literais *)
and cadeia buffer = parse
  | '"' { Buffer.contents buffer }
  | '\t' { Buffer.add_char buffer '\t'; cadeia buffer lexbuf }
  | '\n' { Buffer.add_char buffer '\n'; cadeia buffer lexbuf }
  | '\\' '"' { Buffer.add_char buffer '"' ; cadeia buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; cadeia buffer lexbuf }
  | _ as c { Buffer.add_char buffer c ; cadeia buffer lexbuf }
  | eof { failwith "string nao foi fechada" }
