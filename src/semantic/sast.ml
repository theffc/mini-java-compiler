open Ast

type exp = 
    | ExpOperator of {e1: exp; op: operator; e2: exp}
    | ExpTerm of (exp term)
    | ExpNotTerm of (exp term)
    | ExpMinusTerm of (exp term)