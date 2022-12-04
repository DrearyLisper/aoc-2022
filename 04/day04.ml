open Base
open Stdio

exception Exception of string

let sum = List.fold ~init:0 ~f:(+)

let parseSegment segment =
  match String.split ~on:'-' segment with
  | [a; b] -> (Int.of_string a, Int.of_string b)
  | _      -> raise @@ Exception "invalid segment"

let parseLine line =
  match String.split ~on:',' line with
  | [a; b] -> (parseSegment a, parseSegment b)
  | _      -> raise @@ Exception "invalid line"
 
let part1 lines = 
 let processSegments ((al, ar), (bl, br)) =
    if (al >= bl && ar <= br) ||
       (bl >= al && br <= ar)
    then 1
    else 0 in
  lines
  |> List.map ~f:parseLine
  |> List.map ~f:processSegments
  |> sum

let part2 lines =
  let processSegments ((al, ar), (bl, br)) =
    if al < bl
      then min (max (ar - bl + 1) 0) 1
      else min (max (br - al + 1) 0) 1 in
  lines
  |> List.map ~f:parseLine
  |> List.map ~f:processSegments
  |> sum

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day04.exe *)