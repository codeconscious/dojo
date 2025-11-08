(* open Base *)
(* open Base.Poly *)
(* open Stdio *)

(* let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x) *)

(* let () =
  printf "Total: %F\n" (read_and_accumulate 0.) *) (* You must press Ctrl-D to finish. *)

(* let concat ?(sep="") x y = x ^ sep ^ y
let a = concat ~sep:":" "foo" "bar"
let b = concat ?sep:(Some ":") "foo" "bar" *)

(* let a = List.map ~f:String.length ["Hello"; "World"]
let a = List.map ["Hello"; "World"] String.length
let b = List.fold ~init:0 ~f:(+) [1;2;3]
let b = List.fold [1;2;3] 0 (+) *)

let () =
  (* Sum.run() *)
  print_endline "終"
