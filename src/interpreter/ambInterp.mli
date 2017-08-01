type entrada_fn = { tipo_fn:  Ast._type;
                    formais: (string * Ast._type) list;
										(*      locais: (string * Asabs.tipo) list *)
}

type entrada =  EntFun of entrada_fn
             |  EntVar of Ast._type

type t

val novo_amb :  (string * entrada) list -> t
val novo_escopo : t -> t
val busca: t -> string -> entrada
val insere_local : t -> string -> Ast._type -> unit
val insere_param : t -> string -> Ast._type -> unit
val insere_fun : t -> string -> (string * Ast._type) list -> Ast._type -> unit
