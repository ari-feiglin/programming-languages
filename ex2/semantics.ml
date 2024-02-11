let default_state x = 0;;

let rec aritmetic_semantic e s = match e with
    | Num(n) -> n 0
    | Var(v) ->  s v
    | Add(a,a') -> (aritmetic_semantic a s) + (aritmetic_semantic a' s)
    | Mult(a,a') -> (aritmetic_semantic a s) * (aritmetic_semantic a' s)
    | Sub(a,a') -> (aritmetic_semantic a s) - (aritmetic_semantic a' s);;

let rec boolean_semantic e s = match e with
    | TT -> true
    | FF -> false
    | Aeq(a, a') -> (aritmetic_semantic a s) = (aritmetic_semantic a' s)
    | Beq(b, b') -> (boolean_semantic b s) = (boolean_semantic b' s)
    | Leq(a, a') -> (aritmetic_semantic a s) <= (aritmetic_semantic a' s)
    | Gte(a, a') -> (aritmetic_semantic a s) > (aritmetic_semantic a' s)
    | Neg(b) -> not (boolean_semantic b s)
    | And(b, b') -> (boolean_semantic b s) && (boolean_semantic b' s);;

let create_state prev_state x e = (fun v ->
    if v = x then
        aritmetic_semantic e prev_state
    else
        prev_state v
);;

let rec nos c = match c with
    | (Ass(v,e), s) -> create_state s v e
    | (Skip, s) -> s
    | (Comp(o,o'), s) -> nos (o', nos(o, s))
    | (If(b, o, o'), s) -> (
        if boolean_semantic b s then
            nos(o,s)
        else
            nos(o',s)
        )
    | (While(b, o), s) -> (
        if boolean_semantic b s then
            nos (While(b, o), nos(o, s))
        else
            s
        )
    | (If_Ass(v, e, o, o'),s) -> (
        let s' = nos(Ass(v, e), s) in
        if s' v != 0 then
            nos(o, s')
        else
            nos(o', s')
        )
    | (Repeat(o, b), s) -> (
        let s' = nos(o, s) in
        if boolean_semantic b s' then
            s'
        else
            nos(Repeat(o, b), s')
        );;
