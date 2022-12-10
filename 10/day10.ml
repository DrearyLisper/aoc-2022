open Base
open Stdio

exception Exception of string

type command =
  | Noop
  | Addx of int

type cpu = {
  clock : int;
  x : int;
}

let parse lines =
  let parse line =
    match String.split ~on:' ' line with
    | ["noop"] -> [Noop]
    | ["addx"; x] -> [Noop; Addx (Int.of_string x)]
    | _ -> raise @@ Exception "Unknown command" in
  List.concat_map ~f:parse lines

let apply {clock; x} = function 
| Noop -> {clock = clock + 1; x = x}
| (Addx x') -> {clock = clock + 1; x = x + x'}

let part1 lines = 
  let cpu = {clock = 1; x = 1} in
  let commands = parse lines in
  let cpus = List.fold ~init:[cpu] ~f:(fun cpus c -> apply (List.hd_exn cpus) c :: cpus) commands in
  List.rev cpus
  |> List.filter ~f:(fun cpu -> (cpu.clock - 20) % 40 = 0)
  |> List.map ~f:(fun cpu -> cpu.clock * cpu.x)
  |> List.fold ~init:0 ~f:(+);;

let part2 lines =
  let cpu = {clock = 1; x = 1} in
  let commands = parse lines in
  let cpus = List.fold ~init:[cpu] ~f:(fun cpus c -> apply (List.hd_exn cpus) c :: cpus) commands in
  let output cpu = if abs (cpu.x - (((cpu.clock - 1) % 40))) <= 1 then '#' else '.' in
  List.take (List.rev cpus) 240
  |> List.map ~f:output
  |> List.chunks_of ~length:40
  |> List.map ~f:String.of_char_list
  |> List.iter ~f:(printf "%s\n");;
   
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day10.exe *)