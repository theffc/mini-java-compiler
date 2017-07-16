type id = string

and prog = Prog of mainClass

and mainClass = MainClass of id * mainClassBody
and mainClassBody = MainClassBody of mainMethod * methods option

and mainMethod = MainMethod

and methods = Methods of _method list
and _method = Method of id * _type * parameters option

and parameters = Parameters of parameter list
and parameter = Parameter of id * _type

and _type = Int
		  | Float
		  | Double
		  | Bool
		  | Char
		  | String
(*		  | Array of _type *)

