module Amb = Ambiente
module A = Ast

type operator_type = Arithmetical | Logical | Relational

let find_operator_type (op: A.operator): operator_type =
  let open A in
  match op with
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


let infer_arithmetical_operation_type (exp1_type: A._type) (exp2_type: A._type) : A._type =
  if exp1_type <> exp2_type then
    failwith "Operador Aritmetico so pode ser usado com expressoes de mesmo tipo"
  else

  let open A in
  match exp1_type with
  | Int | Float | Double -> exp1_type

  | _ -> 
    failwith ("Operador Aritmetico nao pode ser usado para expressoes de tipo " ^ type_to_str exp1_type)


let infer_logical_operation_type (exp1_type: A._type) (exp2_type: A._type) : A._type =
  let msg = "Operador Logico so pode ser usado com expressoes de tipo Bool" in
  if exp1_type <> exp2_type then
    failwith msg
  else

  let open A in
  match exp1_type with
  | Bool -> Bool
  | _ -> failwith msg


let infer_relational_operation_type (exp1_type: A._type) (exp2_type: A._type) : A._type =
  if exp1_type <> exp2_type then
    failwith "Operador Relacional so pode ser usado com expressoes de mesmo tipo"
  else

  let open A in
  match exp1_type with
  | Void  ->
    failwith "Operador Relacional nao pode ser usado para expressoes de tipo Void"
  | _ -> Bool


let find_var_type (amb: Amb.t) (id: string) : A._type =
  try 
    (match (Amb.busca amb id) with
      | Amb.EntVar _type -> _type
      | Amb.EntFun _ ->
        let msg = "nome de funcao usado como nome de variavel: " ^ id in
        failwith msg
    )
  with Not_found ->
    let msg = "A variavel " ^ id ^ " nao foi declarada" in
    failwith msg
  

let find_method (amb: Amb.t) (id: string): Amb.entrada_fn =
  try 
    (match (Amb.busca amb id) with
      | Amb.EntFun (fun_data) -> 
        fun_data

      | Amb.EntVar _ -> 
        let msg = "nome de variavel usada como nome de funcao: " ^ id in
        failwith msg 
    )
  with Not_found ->
    let msg = "A funcao " ^ id ^ " nao foi declarada" in
    failwith msg


let rec infer_term_type (amb: Amb.t) (term: A.term): A._type =
  let open A in
  match term with
  | TermLiteral(literal) ->
    ( match literal with
      | LitBool(_) -> Bool
      | LitInt(_) -> Int
      | LitFloat(_) -> Float
      | LitDouble(_) -> Double
      | LitChar(_) -> Char
      | LitString(_) -> String
    )

  | TermVariable(Var(id)) ->
    find_var_type amb id

  | TermMethodCall(m) ->
    verify_method_call amb m


and infer_expression_type (amb: Amb.t) (expression: A.expression): A._type  =
  let open A in
  match expression with
  | ExpTerm(term) -> infer_term_type amb term
  
  | ExpOperator(exp1, op, exp2) ->
    let exp1_type = infer_expression_type amb exp1 in
    let exp2_type = infer_expression_type amb exp2 in
    
    let op_type = find_operator_type op in

    match op_type with
    | Arithmetical -> infer_arithmetical_operation_type exp1_type exp2_type
    | Logical -> infer_logical_operation_type exp1_type exp2_type
    | Relational -> infer_relational_operation_type exp1_type exp2_type


and verify_method_call (amb: Amb.t) (m: A.methodCall): A._type =
  let (id, arguments) = 
    match m with
    | A.MethodCall(id, arguments) -> (id, arguments)
    | A.MethodCallThroughType(_, id, arguments) -> (id, arguments)
  in

  let open Amb in
  let {tipo_fn; formais = parameters} = find_method amb id in

  let ps_types = List.map (fun p -> let (_, p_type) = p in p_type ) parameters in

  let args_types = 
    List.map 
      (fun a -> 
        let A.MethodArgument (expression) = a in 
        infer_expression_type amb expression
      ) 
      arguments
  in

  List.map2 
    (fun a_t p_t -> 
      if a_t <> p_t then failwith "Argumentos com tipos diferentes do esperado"
    )
    args_types ps_types
  ;

  tipo_fn


let add_variable_to_amb amb variable =
  let A.VarDecl(name, t) = variable in
  Amb.insere_local amb name t


let rec verify_statement_type (return_type: A._type) (amb: Amb.t) (statement: A.statement)  =
  let open A in
  match statement with
  | StmReturn(expression) ->
    let exp_type = infer_expression_type amb expression in
    if exp_type <> return_type then
      failwith "a expressao retornada deve ter o mesmo tipo que o retorno declarado pelo metodo"

  | StmVarDecl(declared_variables) ->
    List.iter (add_variable_to_amb amb) declared_variables

  | StmMethodCall(m) ->
    verify_method_call amb m;
    ()

  | StmAttr(Var(id), expression) ->
    let var_type = find_var_type amb id in
    let exp_type = infer_expression_type amb expression in
    if var_type <> exp_type then
      failwith "Atribuicao: variavel que recebera o resultado deve ter o mesmo tipo que a expressao"

  | StmIf(expression, body, elseBody) -> (
    let exp_type = infer_expression_type amb expression in
    if exp_type <> A.Bool then
      failwith "expressao de condicao do IF deve resultar em um booleano"
    else

    let amb_if = Amb.novo_escopo amb in
    List.iter (verify_statement_type return_type amb_if) body;

    match elseBody with
    | None -> ()
    | Some(StmElse(body)) -> 
      let amb_else = Amb.novo_escopo amb in
      List.iter (verify_statement_type return_type amb_else) body
    )

  | StmWhile(expression, body) -> 
    let exp_type = infer_expression_type amb expression in
    if exp_type <> A.Bool then
      failwith "expressao de condicao do WHILE deve resultar em um booleano"
    else

    let amb_while = Amb.novo_escopo amb in
    List.iter (verify_statement_type return_type amb_while) body

  | StmPrint(expression) ->
    let exp_type = infer_expression_type amb expression in
    if exp_type <> A.String then
      failwith "comando print deve ser utilizado com tipos string"

  | StmPrintLn(expression) ->
    let exp_type = infer_expression_type amb expression in
    if exp_type <> A.String then
      failwith "comando println deve ser utilizado com tipos strings"


let add_parameter_to_ambient (amb: Amb.t) (parameter: A.parameter) =
  let A.Parameter (name, t) = parameter in
  Amb.insere_param amb name t


let verify_types_inside_method amb m =
  let A.Method {return_type; body; parameters} = m in

  let amb_fun = Amb.novo_escopo amb in
  List.iter (add_parameter_to_ambient amb) parameters;

  (* Verifica cada comando presente no corpo da função usando o novo ambiente *)
  List.iter (verify_statement_type return_type amb_fun) body
  (* A.DecFun {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo = corpo_tipado} *)


let add_method_to_ambient (amb: Amb.t) (_method: A._method) =
  let open A in
  let A.Method m = _method in
  let parameters = 
    List.map
      (fun p -> let Parameter(name, _type) = p in (name, _type)) 
      m.parameters
  in

  Amb.insere_fun amb m.name parameters m.return_type


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


let semantic (ast: A.prog) =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in

  add_predefined_methods_to_ambient amb_global;

  let A.Prog (A.MainClass (_class_name, MainMethod(main_method), methods)) = ast in

  List.iter (add_method_to_ambient amb_global) methods;

  List.map (verify_types_inside_method amb_global) methods;

  verify_types_inside_method amb_global main_method;



