open Ast

type exp = 
    | ExpOperator of {e1: exp; op: operator; e2: exp}
    | ExpLiteral of literal
    | ExpVariable of variable
    | ExpMethodCall of (exp methodCall)