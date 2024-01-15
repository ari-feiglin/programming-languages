type value =
    | NaN
    | Int of int

type expression =
    | Value of int
    | Add of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Divides of expression * expression;;

let oper = fun op p x y -> (* (int -> int -> int) -> (int -> int -> bool) -> value -> value -> value *)
    match x,y with
    | NaN,_ -> NaN
    | _,NaN -> NaN
    | Int(x),Int(y) -> 
        if not (p x y) then NaN else Int(op x y);;

let rec eval = fun e -> (* expression -> value *)
    match e with
    | Value(x) -> Int(x)
    | Add(e,f) -> oper (+) (fun x y -> true) (eval e) (eval f)
    | Minus(e,f) -> oper (-) (fun x y -> true) (eval e) (eval f)
    | Times(e,f) -> oper (fun x y -> x * y) (fun x y -> true) (eval e) (eval f)
    | Divides(e,f) -> oper (/) (fun x y -> x mod y = 0) (eval e) (eval f);;

let rec expr_to_str = fun e ->
    match e with
    | Value(x) -> string_of_int x
    | Add(e,f) ->   "(" ^ (expr_to_str e) ^ " + " ^ (expr_to_str f) ^ ")"
    | Minus(e,f) -> "(" ^ (expr_to_str e) ^ " - " ^ (expr_to_str f) ^ ")"
    | Times(e,f) -> "(" ^ (expr_to_str e) ^ " * " ^ (expr_to_str f) ^ ")"
    | Divides(e,f) -> "(" ^ (expr_to_str e) ^ " / " ^ (expr_to_str f) ^ ")";;

let rec get_all_partitions_help = fun l r ->
    match r with
    | [] -> [(l, [])]
    | x :: s -> (l,r) :: (get_all_partitions_help (l @ [x]) s);;

let get_all_partitions l = get_all_partitions_help [] l;;

let pair_to_str = fun (l,r) ->
    "[" ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_int x) "" l ^ "], [" ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_int x) "" r ^"]";;

let rec expr_from_sides = fun l r -> (* l and r are lists of expressions, and this will create all possible expressions from them.
                                        So for example if l=[Value(3)] and r=[Value(2)], this will give [Add(Value(3),Value(2)),Minus(Value(3),Value(2)),Times(Value(3),Value(2))]*)
    match l,r with
    | [], _ -> []
    | _, [] -> []
    | e::l1, f::r1 -> [Add(e,f); Minus(e,f); Times(e,f); Divides(e,f)] @ (expr_from_sides l1 r) @ (expr_from_sides l r1);;

let list_to_string = fun l ->
    List.fold_left (fun acc x -> acc ^ ", " ^ (string_of_int x)) "" l;;

let rec all_exprs_from_part = fun l r ->
    if l = [] || r = [] then [] else
    let l_exprs = all_exprs l in
    let r_exprs = all_exprs r in
    expr_from_sides l_exprs r_exprs

and all_exprs_from_all_parts = fun parts ->
    match parts with
    | [] -> []
    | (l,r) :: parts -> (all_exprs_from_part l r) @ (all_exprs_from_all_parts parts)

and all_exprs = fun lst ->
    match lst with
    | [] -> []
    | [x] -> [Value(x)]
    | lst -> all_exprs_from_all_parts (get_all_partitions lst);;

let rec count_equal_exprs = fun l_exprs r_exprs ->
    match l_exprs, r_exprs with
    | [], _ -> 0
    | _, [] -> 0
    | e :: l_exprs', f :: r_exprs' -> 
        let x = eval e in
        let y = eval f in
        if x != NaN && y != NaN && x = y then (
            (* ignore (print_endline ((expr_to_str e) ^ " = " ^ (expr_to_str f))); *)
            1 + (count_equal_exprs l_exprs' r_exprs) + (count_equal_exprs l_exprs r_exprs')
        )
        else
            (count_equal_exprs l_exprs' r_exprs) + (count_equal_exprs l_exprs r_exprs');;

let rec arithmetic_hell_parts = fun parts ->
    match parts with
    | [] -> 0
    | ([],r) :: parts -> arithmetic_hell_parts parts
    | (l,[]) :: parts -> arithmetic_hell_parts parts
    | (l,r) :: parts -> (
        let l_exprs = all_exprs l in
        let r_exprs = all_exprs r in
        (count_equal_exprs l_exprs r_exprs) + (arithmetic_hell_parts parts)
    );;

let rec arithmetic_hell = fun lst ->
    arithmetic_hell_parts (get_all_partitions lst);;

arithmetic_hell[1;2;3;4;5;6;7;8;9;10;1;2;3;4;5;6;7;8;9;10];;

