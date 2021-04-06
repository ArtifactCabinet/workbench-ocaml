
(* 00 Example of inline test *)
let rec fact n = 
  if n = 1 
    then 1 
    else n * fact (n - 1)

let%test _ = fact 3 = 6


(* 01 Lists *)

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::t -> last t;;

let%test _ = last ["a";"b"] = Some "b"

(* n-th elements of a list *)
(* The idea is passing a var along the rec calling stack*)
let rec nth k = function
    | [] -> None
    | h::t -> if k = 1 then Some h else nth (k-1) t;;

let%test _ = nth 2 ["a";"b";"c"]  = Some "b"

(* Using a var as accumulator*)
let len list =
    let rec aux n = function
        | [] -> n
        | _::t -> aux (n+1) t
in aux 0 list;;
    
let%test _ = len ["a";"b";"c"] =  3

let rev list =
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (h::acc) t in
    aux [] list;;
(* add every new element to the HEAD of
   a list originally being [] *)
let%test _= rev [1;2;3] = List.rev [1;2;3]

(* eliminate consecutive duplicates *)
let rec compress = function
    | a :: ( (b :: _) as t) -> if a == b then compress t else a::compress t
    | x -> x;;

let%test _ = compress ["a";"a";"a";"b";"c";"c"] = ["a";"b";"c"] 

(* TODO: 09 pack list into sublists *)