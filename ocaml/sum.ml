(* open Base *)

let sum_list lst =
  let total = ref 0 in
  List.iter (fun x -> total := !total + x) lst;
  !total;;

let () =
  [1; 2; 3]
  |> sum_list
  |> Printf.printf "%d "
