type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec insert = fun t x ->
    match t with
    | Empty -> Node(x, Empty, Empty)
    | Node(y, l, r) -> if x < y then Node(y, insert l x, r) else Node(y, l, insert r x);;

let rec construct_help = fun l ->
    match l with
    | [] -> Empty
    | x :: l -> insert (construct_help l) x;;

let rec reverse = fun l ->
    match l with
    | [] -> []
    | x :: l -> (reverse l) @ [x];;

let construct = fun l -> construct_help (reverse l);;

