open Base
open Stdio

exception Exception of string

type deep_list =
  | S of int
  | L of deep_list list

let parse_scalar stream = 
  let scalar_part, remaining_part = List.split_while ~f:Char.is_digit stream in
  (Int.of_string @@ String.of_char_list scalar_part, remaining_part);;

let parse_deep_list stream =
  let rec parse_deep_list' stream buffer = 
    match stream with
    | '[' :: xs -> 
      let list, stream = parse_deep_list' xs [] in
      let buffer = (L list) :: buffer in
      if List.is_empty stream
        then buffer, stream
        else parse_deep_list' stream buffer
    | ',' :: xs -> parse_deep_list' xs buffer
    | ']' :: xs -> List.rev buffer, xs
    | xs -> 
      let scalar, stream = parse_scalar xs in 
      parse_deep_list' stream (S scalar :: buffer) in
  let list, _ = parse_deep_list' stream [] in
  List.hd_exn list

(*
let print_deep_list list =
  let rec print_deep_list' = function
    | S i -> printf "%d " i
    | L l -> 
      printf "[ ";
      List.iter ~f:print_deep_list' l;
      printf "] " in
  print_deep_list' list;
  printf "\n";;
*)

let rec (+<+) a b =
  match a, b with
  | S a, S b -> Int.compare a b
  | S a, b -> L [S a] +<+ b
  | a, S b -> a +<+ L [S b]
  | L (_ :: _), L [] -> 1
  | L [], L (_ :: _) -> -1
  | L (a :: ass), L (b :: bss) ->
    (match a +<+ b with
    | -1 -> -1
    | 1 -> 1
    | _ -> (L ass) +<+ (L bss))
  | L [], L [] -> 0

let part1 lines = 
  let compare = function
  | [a;b] -> a +<+ b
  | _ -> raise @@ Exception "Can't happen" in
  lines
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:(fun s -> parse_deep_list @@ String.to_list s)
  |> List.chunks_of ~length:2
  |> List.mapi ~f:(fun i pair -> (i, compare pair))
  |> List.filter ~f:(fun (_, c) -> c = -1)
  |> List.map ~f:(fun (i, _) -> i + 1)
  |> List.fold ~init:0 ~f:(+)

let part2 lines = 
  let beacon_a = parse_deep_list @@ String.to_list "[[2]]" in
  let beacon_b = parse_deep_list @@ String.to_list "[[6]]" in
  let sorted = lines
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:(fun s -> parse_deep_list @@ String.to_list s)
  |> List.append [beacon_a; beacon_b]
  |> List.sort ~compare:(+<+) in
  let ai, _ = List.findi_exn ~f:(fun _ l -> l +<+ beacon_a = 0) sorted in
  let bi, _ = List.findi_exn ~f:(fun _ l -> l +<+ beacon_b = 0) sorted in
  (ai + 1) * (bi + 1)
    
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day13.exe *)