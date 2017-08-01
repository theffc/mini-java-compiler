module Amb = Ambiente
module A = Ast
module S = Sast
module T = Tast


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


let infer_arithmetical_operation_type (exp1_type: A._type) (exp2_type: A._type) (pos: A.filePosition) : A._type =
  let error = position_msg_error pos in

  if exp1_type <> exp2_type then
    let msg = "Operador Aritmetico so pode ser usado com expressoes de mesmo tipo" in
    failwith (error ^ msg)
  else

  let open A in
  match exp1_type with
  | Int | Float | Double -> exp1_type

  | _ -> 
    let msg = "Operador Aritmetico nao pode ser usado para expressoes de tipo " ^ type_to_str exp1_type in
    failwith (error ^ msg)


let infer_logical_operation_type (exp1_type: A._type) (exp2_type: A._type) (pos: A.filePosition) : A._type =
  let error = position_msg_error pos in
  let msg = "Operador Logico so pode ser usado com expressoes de tipo Bool" in

  if exp1_type <> exp2_type then
    failwith (error ^ msg)
  else

  let open A in
  match exp1_type with
  | Bool -> Bool
  | _ -> failwith (error ^ msg)


let infer_relational_operation_type (exp1_type: A._type) (exp2_type: A._type) (pos: A.filePosition) : A._type =
  let error = position_msg_error pos in

  if exp1_type <> exp2_type then
    failwith (error ^ "Operador Relacional so pode ser usado com expressoes de mesmo tipo")
  else

  let open A in
  match exp1_type with
  | Void  ->
    failwith (error ^ "Operador Relacional nao pode ser usado para expressoes de tipo Void")
  | _ -> Bool


let find_var_type (amb: Amb.t) (id: A.id) : A._type =
  try 
    (match (Amb.busca amb id.name) with
      | Amb.EntVar _type -> _type
      | Amb.EntFun _ ->
        let error = position_msg_error id.pos in
        let msg = "nome de funcao usado como nome de variavel: " ^ id.name in
        failwith (error ^ msg)
    )
  with Not_found ->
    let error = position_msg_error id.pos in
    let msg = "A variavel " ^ id.name ^ " nao foi declarada" in
    failwith (error ^ msg)
  

let find_method (amb: Amb.t) (id: A.id): Amb.entrada_fn =
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

let rec infer_expression_type (amb: Amb.t) (expression: S.exp) : T.exp typed_expression =
  let open A in
  match expression with  
  | ExpOperator {e1; op; e2} -> (
    let {t=exp1_type; e=t_e1} = infer_expression_type amb e1 in
    let {t=exp2_type; e=t_e2} = infer_expression_type amb e2 in
    
    let op_context = find_operator_context op in
    let t = (
      match op_context with
      | Arithmetical -> infer_arithmetical_operation_type exp1_type exp2_type op.pos
      | Logical -> infer_logical_operation_type exp1_type exp2_type op.pos
      | Relational -> infer_relational_operation_type exp1_type exp2_type op.pos
    ) in
    {t; e= T.ExpOperator{e1=t_e1; op=op; e2=t_e2; expType=t} }
  )

  | ExpLiteral l ->
    ( match l.litType with
      | LitBool(_) -> let t = Bool in {t; e= T.ExpLiteral (l, t)}
      | LitInt(_) -> let t = Int in {t; e= T.ExpLiteral (l, t)}
      | LitFloat(_) -> let t = Float in {t; e=T.ExpLiteral (l, t)}
      | LitDouble(_) -> let t = Double in {t; e=T.ExpLiteral (l, t)}
      | LitChar(_) -> let t = Char in {t; e=T.ExpLiteral (l, t)}
      | LitString(_) -> let t = String in {t; e=T.ExpLiteral (l, t)}
    )

  | ExpVariable(Var id) ->
    let t = find_var_type amb id in
    {t; e=T.ExpVariable(Var id, t)}


  | ExpMethodCall(m) ->
    let {t; e} = verify_method_call amb m in
    {t; e=T.ExpMethodCall(e)}


and verify_method_call (amb: Amb.t) (m:S.exp A.methodCall): (T.exp A.methodCall) typed_expression =
  let (id, arguments) = 
    match m with
    | A.MethodCall(id, arguments) -> (id, arguments)
    | A.MethodCallThroughType(_, id, arguments) -> (id, arguments)
  in

  let open Amb in
  let {tipo_fn; formais = parameters} = find_method amb id in

  let ps_types = List.map (fun p -> let (_, p_type) = p in p_type ) parameters in

  let typed_args = 
    List.map 
      (fun a -> 
        let A.MethodArgument (expression) = a in 
        infer_expression_type amb expression
      )
      arguments
  in

  let args_types = List.map (fun x -> x.t) typed_args in

  List.map2 
    (fun a_t p_t -> 
      if a_t <> p_t then 
        let error = position_msg_error id.pos in
        let msg = "Argumentos com tipos diferentes do esperado" in
        failwith (error ^ msg)
    )
    args_types ps_types
  ;

  let args = List.map (fun x -> A.MethodArgument(x.e)) typed_args in

  {t= tipo_fn; e= A.MethodCall(id, args)}


let add_variable_to_amb amb variable =
  let A.VarDecl(id, t) = variable in
  Amb.insere_local amb id.name t


let rec verify_statement_type (return_type: A._type) (amb: Amb.t) (statement:S.exp A.statement) : T.exp A.statement =
  let open A in
  match statement with
  | StmReturn(expression) ->
    let {t; e} = infer_expression_type amb expression in
    if t <> return_type then
      failwith "a expressao retornada deve ter o mesmo tipo que o retorno declarado pelo metodo"
    else
      StmReturn(e)

  | StmVarDecl(declared_variables) ->
    List.iter (add_variable_to_amb amb) declared_variables;
    StmVarDecl(declared_variables)

  | StmMethodCall(m) ->
    let {t; e} = verify_method_call amb m in
    StmMethodCall(e)

  | StmAttr(Var(id), expression) ->
    let var_type = find_var_type amb id in
    let {t=exp_type; e} = infer_expression_type amb expression in
    if var_type <> exp_type then
      let error = position_msg_error id.pos in
      let msg = "Atribuicao: variavel que recebera o resultado deve ter o mesmo tipo que a expressao" in
      failwith (error ^ msg)
    else
      StmAttr(Var(id), e)

  | StmIf(expression, body, elseBody) ->
    let {t; e} = infer_expression_type amb expression in
    if t <> A.Bool then
      failwith "expressao de condicao do IF deve resultar em um booleano"
    else

    let amb_if = Amb.novo_escopo amb in
    let typed_body = List.map (verify_statement_type return_type amb_if) body in

    ( match elseBody with
    | None -> 
      StmIf(e, typed_body, None)

    | Some(StmElse(body)) ->
      let amb_else = Amb.novo_escopo amb in
      let else_t_body = List.map (verify_statement_type return_type amb_else) body in
      StmIf(e, typed_body, Some(StmElse(else_t_body)))
    )

  | StmWhile(expression, body) -> 
    let {t; e} = infer_expression_type amb expression in
    if t <> A.Bool then
      failwith "expressao de condicao do WHILE deve resultar em um booleano"
    else

    let amb_while = Amb.novo_escopo amb in
    let typed_body = List.map (verify_statement_type return_type amb_while) body in
    StmWhile(e, typed_body)

  | StmPrint(expression) ->
    let {t; e} = infer_expression_type amb expression in
    if t <> A.String then
      failwith "comando print deve ser utilizado com tipos string"
    else
      StmPrint(e)

  | StmPrintLn(expression) ->
    let {t; e} = infer_expression_type amb expression in
    if t <> A.String then
      failwith "comando println deve ser utilizado com tipos strings"
    else
      StmPrintLn(e)


let add_parameter_to_ambient (amb: Amb.t) (parameter: A.parameter) =
  let A.Parameter (id, t) = parameter in
  Amb.insere_param amb id.name t


let verify_types_inside_method (amb: Amb.t) (m: S.exp A._method) : T.exp A._method =
  let A.Method {id; return_type; body; parameters} = m in

  let amb_fun = Amb.novo_escopo amb in
  List.iter (add_parameter_to_ambient amb) parameters;

  (* Verifica cada comando presente no corpo da função usando o novo ambiente *)
  let typed_body = List.map (verify_statement_type return_type amb_fun) body in
  A.Method {id; return_type; body=typed_body; parameters}


let add_method_to_ambient (amb: Amb.t) (_method:S.exp A._method) =
  let open A in
  let A.Method m = _method in
  let parameters = 
    List.map
      (fun p -> let Parameter(id, _type) = p in (id.name, _type)) 
      m.parameters
  in

  Amb.insere_fun amb m.id.name parameters m.return_type


(* Lista de cabeçalhos das funções pré definidas *)
let predefined_functions = 
  let open A in 
  [
   ("entrada", [("x", Int); ("y", Int)], Void);
   ("saida",   [("x", Int); ("y", Int)], Void)
  ]

(* insere as funções pré definidas no ambiente global *)
let add_predefined_methods_to_ambient amb =
  List.iter (
    fun (name, parameters, return_type) -> Amb.insere_fun amb name parameters return_type)
    predefined_functions


let semantic (ast:S.exp A.prog) : T.exp A.prog =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in

  add_predefined_methods_to_ambient amb_global;

  let A.Prog (A.MainClass (_class_name, MainMethod(main_method), methods)) = ast in

  List.iter (add_method_to_ambient amb_global) methods;

  let typed_methods = List.map (verify_types_inside_method amb_global) methods in

  let typed_main = verify_types_inside_method amb_global main_method in

  A.Prog (A.MainClass (_class_name, MainMethod(typed_main), typed_methods))



