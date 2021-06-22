(* A simple impl of syntax checker *)

(* 
example of valid identifier:
a
a-b
asd-bcd
a1-1a-asd

implications:
First and last char must not be dash
*)

(* Railroad chart

 letter -------------------------------------------->
          |                               ^
      -->--------------> letter --------|--|
      |   |        ^  |                 ^  |
      |   >- dash -|-->- digit ---------|  |
      |------------------------------------|
*)

let check_id s = 
  let check =
    let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let is_letter_or_digit x = is_letter x || is_digit x in 
    let rec is_valid s i after_dash_q =
      if i < 0 then not after_dash_q (* the backward search to the 1st ele and it must not be a dash*)
      else if is_letter_or_digit s.[i] then is_valid s (i-1) false
      else if s.[i] = '-' && not after_dash_q then is_valid s (i-1) true
      else false 
    in fun s -> (  (* the starting point*)
      let n = String.length s in
        n > 0 && is_letter s.[n-1] && is_valid s (n-2) false) 
      (*                      ^ 0 based index, means last element *)
in if check s then (print_endline "Valid id!") 
              else (print_endline "Not valid");;                        

check_id "a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a";;
check_id "A-000Bc-a";;

check_id "this is a nice";;
check_id "abc-asd";;
check_id "a--b";;
check_id "a-";;
check_id "000";;