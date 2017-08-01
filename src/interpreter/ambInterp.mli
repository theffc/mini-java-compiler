
type entrada =  EntFun of (Tast.exp Ast._method)
             |  EntVar of Ast._type * (Ast.litType option)

(* O Ast.litType deve ser option pq a variavel pode estar sendo utilizada antes de ter sido inicializada com um valor *)

type t

val novo_amb :  (string * entrada) list -> t
val novo_escopo : t -> t
val busca: t -> string -> entrada
val atualiza_var: t -> string -> Ast._type ->  (Ast.litType option) -> unit
val insere_var : t -> string -> Ast._type -> (Ast.litType option) -> unit
val insere_fun : t -> (Tast.exp Ast._method) -> unit
val insere: t -> string -> entrada -> unit
