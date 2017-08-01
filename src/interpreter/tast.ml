open Ast

type exp = 
    | ExpOperator of {e1: exp; op: operator; e2: exp; expType: _type}
    | ExpLiteral of literal * _type
    | ExpVariable of variable * _type
    | ExpMethodCall of (exp methodCall)