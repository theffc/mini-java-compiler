module Amb = AmbInterp
module A = Ast
module S = Sast
module T = Tast


exception Valor_de_retorno of A.litType

type operator_context = Arithmetical | Logical | Relational

let find_operator_context (op: A.operator): operator_context =
  let open A in
  match op.opType with
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod 
   -> Arithmetical

  | OpAnd
  | OpOr 
   -> Logical

  | OpLess
  | OpLessEqual
  | OpEqual
  | OpDif
  | OpGreater
  | OpGreaterEqual 
   -> Relational


let type_to_str (t: A._type) : string =
  let open A in
  match t with
  | Int -> "Int"
  | Float -> "Float"
  | Double -> "Double"
  | Char -> "Char" 
  | String -> "String"
  | Bool -> "Bool"
  | Void -> "Void"


let position_msg_error (pos: A.filePosition) : string =
  let (l, c) = pos in
  "ERROR (line " ^ string_of_int l ^ " column " ^ string_of_int c ^ "): "


let pega_int (lit: A.litType) : int =
  match lit with
  |  LitInt i  -> i
  | _ -> failwith "pega_int: nao eh inteiro"

let pega_float (lit: A.litType) : float = 
  match lit with
  | LitFloat f -> f
  | _ -> failwith "pega_float: nao eh float"

let pega_double (lit: A.litType) : float = 
  match lit with
  | LitDouble f -> f
  | _ -> failwith "pega_double: nao eh double"

let pega_string (lit: A.litType) : string =
  match lit with
  |  LitString s -> s
  | _ -> failwith "pega_string: nao eh string"

let pega_bool (lit: A.litType) : bool =
  match lit with
  |  LitBool b -> b
  | _ -> failwith "pega_bool: nao eh booleano"


let interpret_relational_operation (v1: A.litType) (op_type: A.opType) (v2: A.litType) (result_type: A._type) : A.litType  =
  (match result_type with
   | Int ->
     (match op_type with
      | OpLess -> LitBool (pega_int v1 < pega_int v2)
      | OpGreater  -> LitBool (pega_int v1 > pega_int v2)
      | OpEqual   -> LitBool (pega_int v1 == pega_int v2)
      | OpDif   -> LitBool (pega_int v1 != pega_int v2)
      | OpLessEqual    -> LitBool (pega_int v1 <= pega_int v2)
      | OpGreaterEqual    -> LitBool (pega_int v1 >= pega_int v2)
      | _ -> failwith "interpreta_relacional"
     )
   | Float ->
     (match op_type with
      | OpLess -> LitBool (pega_float v1 < pega_float v2)
      | OpGreater  -> LitBool (pega_float v1 > pega_float v2)
      | OpEqual   -> LitBool (pega_float v1 == pega_float v2)
      | OpDif   -> LitBool (pega_float v1 != pega_float v2)
      | OpLessEqual    -> LitBool (pega_float v1 <= pega_float v2)
      | OpGreaterEqual    -> LitBool (pega_float v1 >= pega_float v2)
      | _ -> failwith "interpreta_relacional"
     )
   | String ->
     (match op_type with
      | OpLess -> LitBool (pega_string v1 < pega_string v2)
      | OpGreater  -> LitBool (pega_string v1 > pega_string v2)
      | OpEqual   -> LitBool (pega_string v1 == pega_string v2)
      | OpDif   -> LitBool (pega_string v1 != pega_string v2)
      | OpLessEqual    -> LitBool (pega_string v1 <= pega_string v2)
      | OpGreaterEqual    -> LitBool (pega_string v1 >= pega_string v2)
      | _ -> failwith "interpreta_relacional"
     )
   | Bool ->
     (match op_type with
      | OpLess -> LitBool (pega_bool v1 < pega_bool v2)
      | OpGreater  -> LitBool (pega_bool v1 > pega_bool v2)
      | OpEqual   -> LitBool (pega_bool v1 == pega_bool v2)
      | OpDif   -> LitBool (pega_bool v1 != pega_bool v2)
      | OpLessEqual    -> LitBool (pega_bool v1 <= pega_bool v2)
      | OpGreaterEqual    -> LitBool (pega_bool v1 >= pega_bool v2)
      | _ -> failwith "interpreta_relacional"
     )
   | _ ->  failwith "interpreta_relacional"
  )


let interpret_logical_operation (v1: A.litType) (op_type: A.opType) (v2: A.litType) (result_type: A._type): A.litType  =
  (match result_type with
   | Bool ->
     (match op_type with
      | OpOr -> LitBool (pega_bool v1 || pega_bool v2)
      | OpAnd -> LitBool (pega_bool v1 && pega_bool v2)
      | _ ->  failwith "interpreta_logico"
     )
   | _ ->  failwith "interpreta_logico"
  )


let interpret_arithmetical_operation (v1: A.litType) (op_type: A.opType) (v2: A.litType) (result_type: A._type): A.litType =
  let open A in
  (match result_type with
   | Int ->
     (match op_type with
      | OpAdd -> LitInt (pega_int v1 + pega_int v2)
      | OpSub -> LitInt (pega_int v1 - pega_int v2)
      | OpMul -> LitInt (pega_int v1 * pega_int v2)
      | OpDiv  -> LitInt (pega_int v1 / pega_int v2)
      | OpMod -> LitInt (pega_int v1 mod pega_int v2)
      | _ -> failwith "interpreta_aritmetico"
     )
   | Float ->
     (match op_type with
      | OpAdd -> LitFloat (pega_float v1 +. pega_float v2)
      | OpSub -> LitFloat (pega_float v1 -. pega_float v2)
      | OpMul -> LitFloat (pega_float v1 *. pega_float v2)
      | OpDiv  -> LitFloat (pega_float v1 /. pega_float v2)
      | _ -> failwith "interpreta_aritmetico"
     )
    | Double ->
      (match op_type with
       | OpAdd -> LitDouble (pega_double v1 +. pega_double v2)
       | OpSub -> LitDouble (pega_double v1 -. pega_double v2)
       | OpMul -> LitDouble (pega_double v1 *. pega_double v2)
       | OpDiv  -> LitDouble (pega_double v1 /. pega_double v2)
       | _ -> failwith "interpreta_aritmetico"
      )
   | _ -> failwith "interpreta_aritmetico"
  )


let find_var (amb: Amb.t) (id:A.id) : A.litType = 
  try 
    (match (Amb.busca amb id.name) with
      | Amb.EntFun _ ->
        let error = position_msg_error id.pos in
        let msg = "nome de funcao usado como nome de variavel: " ^ id.name in
        failwith (error ^ msg)

      | Amb.EntVar(_, Some(var)) -> var

      | _ ->
        let error = position_msg_error id.pos in
        let msg = "Variavel " ^ id.name ^ " sendo usada antes de ser inicializada com algum valor" in
        failwith (error ^ msg)
    )
  with Not_found ->
    let error = position_msg_error id.pos in
    let msg = "A variavel " ^ id.name ^ " nao foi declarada" in
    failwith (error ^ msg)
  

let find_method (amb: Amb.t) (id: A.id): T.exp Ast._method =
  try 
    (match (Amb.busca amb id.name) with
      | Amb.EntFun (fun_data) -> 
        fun_data

      | Amb.EntVar _ -> 
        let error = position_msg_error id.pos in
        let msg = "nome de variavel usada como nome de funcao: " ^ id.name in
        failwith (error ^ msg) 
    )
  with Not_found ->
    let error = position_msg_error id.pos in
    let msg = "A funcao " ^ id.name ^ " nao foi declarada" in
    failwith (error ^ msg)


type 'e typed_expression = {
  t: A._type;
  e: 'e;
}


let add_parameter_to_ambient (amb: Amb.t) (parameter: A.parameter) =
  let A.Parameter (id, t) = parameter in
  Amb.insere_param amb id.name t


let add_variable_to_amb (amb: Amb.t) (variable: A.varDeclaration) =
  let A.VarDecl(id, t) = variable in
  Amb.insere_local amb id.name t None


let rec interpret_expression (amb: Amb.t) (expression: T.exp) : A.litType =
  let open A in
  match expression with  
  | ExpOperator {e1; op; e2; expType} -> (
    let v1 = interpret_expression amb e1 in
    let v2 = interpret_expression amb e2 in
    
    let op_context = find_operator_context op in
    let value = (
      match op_context with
      | Arithmetical -> interpret_arithmetical_operation v1 op.opType v2 expType
      | Logical -> interpret_logical_operation v1 op.opType v2 expType
      | Relational -> interpret_relational_operation v1 op.opType v2 expType
    ) in
    value
  )

  | ExpLiteral (l, _) ->
    l.litType

  | ExpVariable(Var id, t) ->
    let value = find_var amb id in
    value

  | ExpMethodCall(m) ->
    interpret_method_call amb m


and interpret_statement (amb: Amb.t) (statement:T.exp A.statement) =
  let open A in
  match statement with
  | StmReturn(expression) ->
    (match expression with
     (* Se a função não retornar nada, então retorne ExpVoid *)
      | None -> raise (Valor_de_retorno LitVoid)
      | Some e ->
        (* Avalia a expressão e retorne o resultado *)
        let e1 = interpret_expression amb e in
        raise (Valor_de_retorno e1)
    )

  | StmVarDecl(declared_variables) ->
    List.iter (add_variable_to_amb amb) declared_variables;

  | StmMethodCall(m) ->
    interpret_method_call amb m;
    ()

  | StmAttr(Var(id), expression) ->
    let Amb.EntVar(t, _) = Amb.busca amb id.name in
    let e = interpret_expression amb expression in
    Amb.atualiza_var amb id.name t;
    ()

  | StmIf(expression, body, elseBody) ->
    let LitBool condition = interpret_expression amb expression in
    if condition then
      let amb_if = Amb.novo_escopo amb in
      List.iter (interpret_statement amb_if) body
    else

    ( match elseBody with
    | None -> ()

    | Some(StmElse(body)) ->
      let amb_else = Amb.novo_escopo amb in
      List.iter (interpret_statement amb_else) body;
    )

  | StmWhile(expression, body) ->
    let amb_while = Amb.novo_escopo amb in
    let rec loop exp cmds =
      let LitBool condition = interpret_expression amb_while exp in
      if condition then (
        List.iter (interpret_statement amb_while) cmds;
        loop exp cmds
      )
      else 
        ()
    in

    loop expression body;

  | StmPrint(expression) ->
    let e = interpret_expression amb expression in
    (match e with
      | A.LitInt (n) -> print_int n
      | A.LitFloat (n) -> print_float n
      | A.LitString (n) -> print_string n
      | _ -> failwith "Fail print"
    )

  | StmPrintLn(expression) ->
    let e = interpret_expression amb expression in
    (match e with
      | A.LitInt (n) -> print_int n
      | A.LitFloat (n) -> print_float n
      | A.LitString (n) -> print_string n
      | _ -> failwith "Fail print"
    )


and interpret_method_call (amb: Amb.t) (m:T.exp A.methodCall): A.litType =
  let (id, arguments) = 
    match m with
    | A.MethodCall(id, arguments) -> (id, arguments)
    | A.MethodCallThroughType(_, id, arguments) -> (id, arguments)
  in

  let A.Method(_method) = find_method amb id in

  let args_values = 
    List.map 
      ( fun a -> let A.MethodArgument(e) = a in Some(interpret_expression amb e) ) 
      arguments 
  in

  let amb_fun = Amb.novo_escopo amb in
  List.iter2 ( fun p v ->
    let A.Parameter(id, t) = p in
    add_parameter_to_ambient amb_fun p;
    Amb.atualiza_var amb_fun id.name t v
  ) _method.parameters args_values;

  try
    List.iter (interpret_statement amb_fun) _method.body;
    A.LitVoid
  with
    Valor_de_retorno value -> value


(* Lista de cabeçalhos das funções pré definidas *)
let predefined_functions = 
  let open A in 
  [
    ("System.out.println", [("x", Int); ("y", Int)], Void);
    ("System.out.println",  [("x", Int); ("y", Int)], Void)
  ]

(* insere as funções pré definidas no ambiente global *)
(* let add_predefined_methods_to_ambient amb =
  List.iter 
    (fun (name, fn) -> Amb.insere amb name fn;)
    predefined_functions; *)


let interpret (ast:T.exp A.prog) : unit =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in

  (* add_predefined_methods_to_ambient amb_global; *)

  let A.Prog (A.MainClass (_class_name, MainMethod(Method(main_method)), methods)) = ast in

  List.iter (Amb.insere_fun amb_global) methods;

  List.iter (interpret_statement amb_global) main_method.body;


