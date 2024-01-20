let rec sum_list list = match list with [] -> 0 | t::h -> t + (sum_list h) ;;

let rec compress list = match list with [] -> [] | [x] -> [x] | x::y::h -> if x = y then (compress (y::h)) else x::(compress (y::h));; 
