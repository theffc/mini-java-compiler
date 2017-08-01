module Tab = Tabsimb
module A = Ast

type entrada =  
    | EntFun of (Tast.exp Ast._method)
    | EntVar of A._type * (A.litType option)

type t = {
  ambv : entrada Tab.tabela
}

let novo_amb xs = { ambv = Tab.cria xs }

let novo_escopo amb = { ambv = Tab.novo_escopo amb.ambv }

let busca amb ch = Tab.busca amb.ambv ch

let atualiza_var amb ch t v =
  Tab.atualiza amb.ambv ch (EntVar (t,v))

let insere_var amb nome t v =
  Tab.insere amb.ambv nome (EntVar (t,v))

let insere_fun amb fn =
  let A.Method{id} = fn in
  Tab.insere amb.ambv id.name (EntFun(fn))

let insere amb nome value =
  Tab.insere amb.ambv nome value
