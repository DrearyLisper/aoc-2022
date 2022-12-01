open Base
open Stdio

let part1 lines = 
  let rec part1' m c lines =
    match lines with
    | [] -> m
    | "" :: xs -> part1' (max m c) 0 xs
    | x :: xs -> part1' m (c + Int.of_string x) xs
  in part1' 0 0 lines

let part2 lines = 
  let rec part2' m c lines =
    match lines with
    | [] -> m
    | "" :: xs -> part2' (c :: m) 0 xs
    | x :: xs -> part2' m (c + Int.of_string x) xs in
  let snacks = List.sort ~compare:(fun x y -> -(Int.compare x y)) @@ part2' [] 0 lines in
  List.fold ~init:0 ~f:(+) @@ List.take snacks 3

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day01.exe *)