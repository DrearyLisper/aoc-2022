open Base
open Stdio
(*

input.txt

[B]                     [N]     [H]
[V]         [P] [T]     [V]     [P]
[W]     [C] [T] [S]     [H]     [N]
[T]     [J] [Z] [M] [N] [F]     [L]
[Q]     [W] [N] [J] [T] [Q] [R] [B]
[N] [B] [Q] [R] [V] [F] [D] [F] [M]
[H] [W] [S] [J] [P] [W] [L] [P] [S]
[D] [D] [T] [F] [G] [B] [B] [H] [Z]
 1   2   3   4   5   6   7   8   9 

 test.txt

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

*)

(* exception Exception of string *)

let createStacks stacks =
  stacks
  |> List.map ~f:String.to_list

let inputStacks = 
  List.to_array @@ createStacks [
    "BVWTQNHD";
    "BWD";
    "CJWQST";
    "PTZNRJF";
    "TSMJVPG";
    "NTFWB";
    "NVHFQDLB";
    "RFPH";
    "HPNLBMSZ"
  ]

(*
let testStacks =
  List.to_array @@ createStacks [
    "NZ";
    "DCM";
    "P"
  ]
*)

type command = 
{
  crates_num : int;
  move_from  : int;
  move_to    : int;
}

let parseCommand line = 
  let matches =
    let pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)" in
    Re.exec (Re.Posix.compile_pat pattern) line
  in
  {
    crates_num = Int.of_string @@ Re.Group.get matches 1;
    move_from  = (Int.of_string @@ Re.Group.get matches 2) - 1;
    move_to    = (Int.of_string @@ Re.Group.get matches 3) - 1;
  }

let applyCommand order_fn stacks { crates_num; move_from; move_to } =
  let fromStack = stacks.(move_from) in
  let toStack = stacks.(move_to) in
  let crates = List.take fromStack (crates_num) in
  stacks.(move_to)    <- (order_fn crates) @ toStack;
  stacks.(move_from) <- List.drop fromStack (crates_num);
  stacks;;

let part1 lines = 
  let stacks = Array.copy @@ inputStacks in
  lines
  |> List.map ~f:parseCommand
  |> List.fold ~init:stacks ~f:(applyCommand List.rev)
  |> Array.map ~f:List.hd_exn
  |> Array.to_list
  |> String.of_char_list

let part2 lines = 
  let stacks = Array.copy @@ inputStacks in
  lines
  |> List.map ~f:parseCommand
  |> List.fold ~init:stacks ~f:(applyCommand Fn.id)
  |> Array.map ~f:List.hd_exn
  |> Array.to_list
  |> String.of_char_list

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%s\n" @@ part1 lines;
  printf "%s\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day05.exe *)