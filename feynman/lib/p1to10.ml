
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


let pack list = 
    let rec aux cur acc = function
        | [] -> []
        | [x] -> (x::cur) :: acc
(* Take the head each time and compare it with the next element
if they are same, put a into the cur, and pass the unchanged acc
else emptify the cur_list, concatenate cur_list into the accumulator*)
        | a :: (( b :: _ ) as t) ->
            if a = b then aux (a::cur) acc t
            else aux [] (( a :: cur) :: acc) t in
    List.rev (aux [] [] list)

let%test _ = pack [1;1;2;2;2;3;] = [[1;1];[2;2;2];[3]]

(* Using the same principle on "Run-length encoding"*)

let encode list = 
    let rec aux count acc = function
        | []  -> []
        | [x] -> (count+1,x) :: acc
        | a::( b::_ as t) ->
            if a = b then aux (count + 1) acc t
            else aux 0 ((count+1,a)::acc) t in 
    List.rev (aux 0 [] list);;

let%test _ = 
    encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] 
    =[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

let encode_alt lst =
    List.map (fun l -> (List.length l, List.hd l)) (pack lst);;

let%test _ = 
    encode_alt ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] 
    =[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]