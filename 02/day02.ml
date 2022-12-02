open Base
open Stdio

let gameScore hand =
  match hand with
    | (Some "A", Some "Y") -> 6
    | (Some "A", Some "X") -> 3
    | (Some "A", _)   -> 0
    | (Some "B", Some "Z") -> 6
    | (Some "B", Some "Y") -> 3
    | (Some "B", _)   -> 0
    | (Some "C", Some "X") -> 6
    | (Some "C", Some "Z") -> 3
    | (Some "C", _)   -> 0
    | _          -> 0
let handScore hand = 
  match hand with
    | (_, Some "X") -> 1
    | (_, Some "Y") -> 2
    | (_, Some "Z") -> 3
    | _   -> 0
 
let pickResponse hand =
  match hand with
    | (Some "A", Some "X") -> Some "Z"
    | (Some "A", Some "Y") -> Some "X"
    | (Some "A", _)        -> Some "Y"
    | (Some "B", Some "X") -> Some "X"
    | (Some "B", Some "Y") -> Some "Y"
    | (Some "B", _)        -> Some "Z"
    | (Some "C", Some "X") -> Some "Y"
    | (Some "C", Some "Y") -> Some "Z"
    | (Some "C", _)        -> Some "X"
    | _                    -> None

let part1 lines =
  let processHand line =
    let parts = String.split_on_chars ~on:[' '] line in
    let hand = (List.nth parts 0, List.nth parts 1) in
    handScore hand + gameScore hand
  in
  List.map ~f:processHand lines 
  |> List.fold ~init:0 ~f:(+)
  
let part2 lines = 
  let processHand line =
    let parts = String.split_on_chars ~on:[' '] line in
    let (l, r) = (List.nth parts 0, List.nth parts 1) in
    let newHand = (l, pickResponse (l, r)) in
    handScore newHand + gameScore newHand
  in
  List.map ~f:processHand lines 
  |> List.fold ~init:0 ~f:(+)

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day02.exe *)