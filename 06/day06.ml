open Base
open Stdio

exception Exception of string

let rec tails l =
  match l with
  | []       -> []
  | _ :: xs  -> l :: (tails xs)

let marker length line = 
    line
    |> String.to_list
    |> tails
    |> List.map ~f:(fun l -> List.take l length)
    |> List.filter ~f:(fun l -> List.length l = length)
    |> List.findi ~f:(fun _ l -> Set.length (Set.of_list (module Char) l) = length)
    |> function 
       | None -> raise @@ Exception "No string found"
       | Some (i, _) -> length + i

let part1 lines =
  lines
  |> List.map ~f:(marker 4)
  |> List.iter ~f:(fun x -> printf "%d\n" x)

let part2 lines =
  lines
  |> List.map ~f:(marker 14)
  |> List.iter ~f:(fun x -> printf "%d\n" x)


let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  part1 lines;
  part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day06.exe *)