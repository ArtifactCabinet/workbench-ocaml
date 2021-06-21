module Array = Stdlib.Array;;
open Printf
(* https://www.seas.harvard.edu/courses/cs153/2019fa/cs153-self-assess.pdf *)

let i2s = function 1 -> "1" | 0 -> "0" | _ -> "";;
let int2bits_str x =
  let rec aux y lst = match y with 0 -> lst
    | _ -> aux (y/2) ((i2s (y mod 2))::lst)
  in
  aux x [];;
let int2bits_v1 a =
  print_int a;
  print_string " -> (str) ";
  List.iter (printf "%s") (int2bits_str a);
  print_endline "";;

(* without str and strict 32 length*)

(*
List of same elements n times 
copied from ocaml battries *)
let lst_make x n =
  if x < 0 then invalid_arg "lst_make";
  let rec loop x acc = function
    | 0 -> acc
    | i -> loop x (x::acc) (i-1)
  in
  loop n [] x

let int2bits x =
  let rec aux y lst = match y with 0 -> lst
    | _ -> aux (y/2) ((y mod 2)::lst)
  in
  aux x [];;

let bits32 x =
  List.append (lst_make (32-List.length x) 0) x;;

let prtIntList x = List.iter (printf "%d") x;;

let test i tx =
  if (bits32(int2bits i)) = tx
  then begin print_string "[pass] " end
  else begin print_string "[nono] " end;
    prtIntList tx;
    print_string " <> ";
    prtIntList(bits32(int2bits i));
    print_string " <- ";
    print_int i;
    print_endline "";;

let ()=
  int2bits_v1 321;
  int2bits_v1 100;
  prtIntList(bits32(int2bits 4294967295));
  print_endline "";
  test 4294967295 [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];
  test 2147483647 [0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];
  test 321 [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;0;1;0;0;0;0;0;1];
  test 1 [0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];

