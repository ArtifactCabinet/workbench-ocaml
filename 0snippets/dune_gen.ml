(* TODO: direct IO within ocaml, and clean the code *)

let endml name =
  Filename.check_suffix name ".ml";;

let rec trimml lst =
  match lst with 
    | [] -> []
    | h::t ->  String.sub h 0 ((String.length h) - 3) :: trimml t;;

let f= Sys.readdir "." |> Array.to_list |>  List.filter endml;;
let newf=trimml f;;

print_endline "(executables";;
print_string "(names";;
let f e =
  Printf.printf " %s" e
in 
  List.iter f newf;;
print_string "))\n";;