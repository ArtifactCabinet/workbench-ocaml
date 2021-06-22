(* Use Std lib only *)
(* define the map *)

open List;;

let slb_double x = x*2;;

(* They are composition operators *)
(* https://ocaml.org/api/Stdlib.html#1_Compositionoperators *)
slb_double @@ 3 ;;              (* apply *)
3 |> slb_double;;               (* pipeline *)

let rec slb_map f l =
  match l with
    [] -> []
  | h::t -> f h :: slb_map f t;;

let l1 = (slb_map slb_double [1;2;3] ) in
let l2 = [2;4;6] in
l1=l2;; (* return => true *)


(* Folding *)
(* 1 - (2 - (3 - 0)) => 2 *)
(* init value on right *)
fold_right (-) [1;2;3] 0;;

(* ((0 - 1) - 2) - 3 => -6 *)
(* init value on left *)
fold_left  (-) 0 [1;2;3];;

(* Folding with Trees *)
type 'a tree=
  | Leaf
  | Node of 'a * 'a tree * 'a tree;; (* the * is tuple type *)


(* Usage example *)

let mytree =
  Node(4,
       Node(2,
            Node(1,Leaf,Leaf),
            Node(3,Leaf,Leaf)),
       Node(5,
            Node(6,Leaf,Leaf),
            Node (7,Leaf,Leaf)
           )
      )

(* 4-( 2-(1,3) 5-(6,7)) *)

(* A generic function that put op on each level of the tree *)
let rec foldtree init op = function
  | Leaf -> init
  | Node (v,l,r) -> op v (foldtree init op l) (foldtree init op r);;

let treeSize t = foldtree 0 (fun _ l r -> 1 + l + r) t;;
let treeSum  t = foldtree 0 (fun x l r -> x +l + r) t;;

(* Less abstraction ? *)
let rec treeSize_alt = 
  function
    | Leaf -> 0
    | Node (_,l,r) -> 1 + treeSize_alt l + treeSize_alt r;;
let rec treeSum_alt = 
  function
    | Leaf -> 0
    | Node (v,l,r) -> v + treeSum_alt l + treeSum_alt r;;


treeSize mytree;;               (* 7 *)
treeSize_alt mytree;;

treeSum mytree;;                (* 28*)
treeSum_alt mytree;;

(* Tree fliper *)
let rec treeFlip = function
  | Leaf -> Leaf
  | Node (v,l,r) -> Node(v,treeFlip r,treeFlip l);;
mytree = treeFlip @@ treeFlip mytree;; (* True *)

(* define Python's range function *)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
    (* the n-1 will always be prepend on the head of acc, until n < i which will halt the rec process *)
  in aux j [] ;;

let square x = x*x
let sum = List.fold_left (+) 0

let sum_sq n =
  0--n                (* [0;1;2;...;n]   *)
  |> List.map square  (* [0;1;4;...;n*n] *)
  |> sum              (*  0+1+4+...+n*n  *);;

sum_sq 3;;

(* Types *)

(* Define a RGB type *)

type colour =
    Red
  | Green
  | Blue
  | Yellow
  | RGB of int * int * int;;

let components c =
  match c with
    Red -> (255,0,0)
  | Green -> (0,255,0)
  | Yellow -> (0,0,255)
  | RGB (r,g,b) -> (r,g,b)
  | _ -> (0,0,0);;

components Red;;
components (RGB (1,2,3));;
