#load "lexer_java.cmo";;

let rec tokens lexbuf =
  let tok = Lexer_java.token lexbuf in
  match tok with
  | Lexer_java.EOF -> [Lexer_java.EOF]
  | _ -> tok :: tokens lexbuf
;;

let lexico str =
  let lexbuf = Lexing.from_string str in
  tokens lexbuf
;;

let lex arq =
  let ic = open_in arq in
  let lexbuf = Lexing.from_channel ic in
  let toks = tokens lexbuf in
  let _ = close_in ic in
  toks
