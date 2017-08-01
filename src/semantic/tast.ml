open Ast

type exp = 
    | ExpOperator of {e1: exp; op: operator; e2: exp; expType: _type}
    | ExpTerm of (exp term) * _type
    | ExpNotTerm of (exp term)
    | ExpMinusTerm of (exp term)