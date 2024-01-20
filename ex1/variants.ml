type shape = Circle of float | Square of float | Rectangle of float * float;;

let area s = match s with Circle x -> x *. x *. 3.14159 | Square x -> x *. x | Rectangle (x,y) -> x *. y;;

let rec total_area list = match list with [] -> 0.0 | x::h -> (area x) +. (total_area h);;
