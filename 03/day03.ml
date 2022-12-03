open Base
open Stdio

let scoreAnItem item =
  let (=) = Char.(=) in
  if item = (Char.lowercase item)
    then (Char.to_int item) - (Char.to_int 'a') + 1
    else (Char.to_int item) - (Char.to_int 'A') + 27

let sum = List.fold ~init:0 ~f:(+)

let part1 lines = 
  let processLine line =
    let length = String.length line in
    let leftPart = 
      String.sub line ~pos:0 ~len:(length / 2)
      |> String.to_list
      |> Set.of_list (module Char) in
    let rightPart =
      String.sub line ~pos:(length / 2) ~len:(length / 2)
      |> String.to_list
      |> Set.of_list (module Char) in
    let commonPart = 
      Set.inter leftPart rightPart
      |> Set.to_list in
    List.map ~f:scoreAnItem commonPart  
    |> sum
  in
  List.map ~f:processLine lines
  |> sum

exception Exception of string
let fold1 ~f l =
  match l with
  | []      -> raise (Exception "empty list")
  | x :: xs -> List.fold ~init:x ~f:f xs

let part2 lines =
  let stringToSet s =
    s 
    |> String.to_list 
    |> Set.of_list (module Char) in

  let processChunk chunk =
    chunk
    |> fold1 ~f:Set.inter 
    |> Set.to_list 
    |> List.map ~f:scoreAnItem 
    |> sum in

  List.chunks_of ~length:3 lines
  |> List.map ~f:(List.map ~f:stringToSet)
  |> List.map ~f:processChunk
  |> sum

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day03.exe *)