
(* 
  TERMINAL USAGE:
- "ocamlbuild -use-menhir main.byte"
- "rlwrap ocaml" (rlwrap is just give you the power of remembering commands while in ocaml)
- call one of the following functions
*)

let parse_ast_from_string s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.token lexbuf in
  ast

let parse_ast_from_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parser.prog Lexer.token lexbuf in
  let _ = close_in ic in
  ast
