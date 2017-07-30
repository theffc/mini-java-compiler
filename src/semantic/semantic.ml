module Amb = Ambiente
module A = Ast




(* Lista de cabeçalhos das funções pré definidas *)
let predefined_functions = let open A in [
   ("entrada", [("x", Int); ("y", Int)], Void);
   ("saida",   [("x", Int); ("y", Int)], Void)
]

(* insere as funções pré definidas no ambiente global *)
let declare_predefined_functions amb =
  List.iter (
    fun (name, parameters, return_type) -> Amb.insere_fun amb name parameters return_type)
    predefined_functions

let declare_methods amb methods =
  let open A in
  List.iter 
    (fun x -> 
      let Method m = x in
      let ps = 
        List.map
          (fun p -> let Parameter(name, _type) = p in (name, _type)) 
          m.parameters
      in
      Amb.insere_fun amb m.name ps m.return_type
    )
    methods

let semantic ast =
    (* cria ambiente global inicialmente vazio *)
    let amb_global = Amb.novo_amb [] in

    let _ = declare_predefined_functions amb_global in

    let (A.Prog (A.MainClass (_class_name, A.MainClassBody (main_method, methods)))) = ast in

    declare_methods amb_global methods;
    

    let myprint e =
      let A.Method {name} = e in
      print_string name
    in

    List.iter myprint methods
