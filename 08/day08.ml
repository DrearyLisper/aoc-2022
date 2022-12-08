open Base
open Stdio

(* exception Exception of string *)

type tree = {
  height: int;
  mutable visible: bool;
  mutable scenic_score : int;
}

let parse = 
  let parseCell c = {height = Int.of_string @@ String.of_char c; visible = false; scenic_score = 1} in
  let parseLine line = List.map ~f:parseCell (String.to_list line) in
  List.map ~f:parseLine;;

let visible_of_tree t = if t.visible then 1 else 0
let scenic_score_of_tree t = t.scenic_score


let mark_visible trees =
 let height = ref (-1) in
 let f tree = 
  if tree.height > !height
    then
      (tree.visible <- true;
      height := tree.height) in
  List.iter ~f:f trees;;

let compute_scenic_score = 
  let f' = function
    | [t] -> t.scenic_score <- 0
    | t :: ts ->
      let multiplier = (List.length (List.take_while ~f:(fun x -> x.height < t.height) ts)) in
      let multiplier' = (multiplier + (if List.length ts = multiplier then 0 else 1)) in
      t.scenic_score <- t.scenic_score * multiplier'
    | _ -> () in
  List.iter ~f:f' 

let sum = List.fold ~init:0 ~f:(+)

let max_agg = List.fold ~init:0 ~f:max

let rec tails l =
  match l with
  | []       -> []
  | _ :: xs  -> l :: (tails xs)

let part1 lines = 
  let trees = parse lines in
  List.iter ~f:mark_visible trees;
  List.iter ~f:(fun x -> x |> List.rev |> mark_visible) trees;
  List.iter ~f:mark_visible (List.transpose_exn trees);
  List.iter ~f:(fun x -> x |> List.rev |> mark_visible) (List.transpose_exn trees);
  trees
  |> List.map ~f:(List.map ~f:visible_of_tree)
  |> List.map ~f:sum
  |> sum

let part2 lines =
  let trees = parse lines in
  List.iter ~f:(fun x -> x |> tails |> compute_scenic_score) trees;
  List.iter ~f:(fun x -> x |> List.rev |> tails |> compute_scenic_score) trees;
  List.iter ~f:(fun x -> x |> tails |> compute_scenic_score) (List.transpose_exn trees);
  List.iter ~f:(fun x -> x |> List.rev |> tails |> compute_scenic_score) (List.transpose_exn trees);
  trees
  |> List.map ~f:(List.map ~f:scenic_score_of_tree)
  |> List.map ~f:max_agg
  |> max_agg
   
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day08.exe *)