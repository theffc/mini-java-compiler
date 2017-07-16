type id = string

and prog = Prog of mainClass

and mainClass = MainClass of id * mainClassBody
and mainClassBody = MainClassBody of mainMethod * _method list

and mainMethod = MainMethod

and _method = Method of id * _type * parameter list

and parameter = Parameter of id * _type

and _type = Int
		  | Float
		  | Double
		  | Bool
		  | Char
		  | String
(*		  | Array of _type *)

