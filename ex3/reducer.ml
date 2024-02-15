(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars = 
  if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
  then raise (OutOfVariablesError)
  else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)


let rec fv = function
    | Variable(x) -> StringSet.singleton x
    | Abstraction(x,t) -> StringSet.remove x (fv t)
    | Application(t1,t2) -> StringSet.union (fv t1) (fv t2);;

let rec var = function
    | Variable(x) -> StringSet.singleton x
    | Abstraction(x,t) -> StringSet.add x (var t)
    | Application(t1,t2) -> StringSet.union (var t1) (var t2);;


let extract_some = function
  | Some x -> x 

(*
 * Rules for substitution:
     * x[x->s]=s
     * y[x->s]=y
     * (\x.t1)[x->s] = \x.t1
     * (\y.t1)[x->s] = \z.(t1[y->z][x->s]) for y\in FV(s), z\notin var(
     * (\y.t1)[x->s] = \y.(t1[x->s]) for y\notin FV(s)
 *)
let rec substitute = fun x s t -> match t with
    | Variable(y) -> if x = y then s else Variable(y)
    | Abstraction(y,t1) -> (
        if x = y then Abstraction(x,t1)
        else
            if StringSet.mem y (fv s) then
                (* potentially fix this *)
                let z = fresh_var (StringSet.add x (StringSet.union (var t) (var s))) in
                Abstraction(z, substitute x s (substitute y (Variable(z)) t1))
            else
                Abstraction(y, substitute x s t1)
    )
    | Application(t1,t2) -> Application(substitute x s t1, substitute x s t2);;

let rec reduce_cbv = function
    | Variable(x) -> None
    | Abstraction(x,t) -> None
    | Application(Abstraction(x,t1),t2) -> (
        match t2 with
        | Abstraction(_,_) -> Some(substitute x t2 t1)
        | Variable(_) -> Some(substitute x t2 t1)
        | Application(t3,t4) -> Some(Application(Abstraction(x,t1), reduce_cbv (Application(t3,t4)) |> extract_some))
    )
    | Application(Variable(_),_) -> None
    | Application(t1,t2) -> Some(Application(reduce_cbv t1 |> extract_some, t2));;

let rec reduce_cbn = function
    | Variable(x) -> None
    | Abstraction(x,t) -> None
    | Application(Abstraction(x,t1),t2) -> Some(substitute x t2 t1)
    | Application(t1,t2) -> Some (Application(reduce_cbn t1 |> extract_some, t2))

