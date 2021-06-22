type bexpr =
  | Var of bool
  | Not of bexpr
  | And of bexpr * bexpr
  | Or  of bexpr * bexpr
  | Xor of bexpr * bexpr (*two value are different*)

let rec beval = function
  | Var a -> a
  | Not a -> not (beval a)
  | And (a,b) -> beval a && beval b
  | Or (a,b) -> beval a || beval b
  | Xor (a,b) -> beval a <> beval b;;

beval (And (Var true, (Or (Var false,Var false))));;
beval (Xor (Var true, Var true));;
beval (Xor (Var true, Var false));;

(* Very similar *)
(* Define a mathematical type *)

type expr =
    Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec evaluate e=
  match e with
    Num x -> x
  | Add (e,e2) -> evaluate e + evaluate e2
  | Sub (e,e2) -> evaluate e - evaluate e2
  | Mul (e,e2) -> evaluate e * evaluate e2
  | Div (e,e2) -> evaluate e / evaluate e2;;

evaluate (Add (Num 1, Mul ( Num 2, Num 3)));; (* (Add 1 (Mul 2 3) => 7*)

(*

Using #trace evaluate

evaluate <-- Add (Num 1, Mul (Num 2, Num 3))
evaluate <-- Mul (Num 2, Num 3)
evaluate <-- Num 3
evaluate --> 3
evaluate <-- Num 2
evaluate --> 2
evaluate --> 6
evaluate <-- Num 1
evaluate --> 1
evaluate --> 7
- : int = 7
*)
