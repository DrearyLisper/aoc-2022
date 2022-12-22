open Base
open Stdio

exception Exception of string

type point = {
  x: int;
  y: int;
}

type rock = 
  | Minus
  | Plus
  | Corner
  | Pipe
  | Square 

let tuple_to_point (x, y) = {x = x; y = y};;

let create_rock {x;y} = function
  | Minus  -> Hash_set.Poly.of_list @@ List.map ~f:tuple_to_point [(x + 3, y); (x + 4, y); (x + 5, y); (x + 6, y)]
  | Plus   -> Hash_set.Poly.of_list @@ List.map ~f:tuple_to_point [(x + 4, y); (x + 3, y + 1); (x + 4, y + 1); (x + 5, y + 1); (x + 4, y + 2)]
  | Corner -> Hash_set.Poly.of_list @@ List.map ~f:tuple_to_point [(x + 3, y); (x + 4, y); (x + 5, y); (x + 5, y + 1); (x + 5, y + 2)]
  | Pipe   -> Hash_set.Poly.of_list @@ List.map ~f:tuple_to_point [(x + 3, y); (x + 3, y + 1); (x + 3, y + 2); (x + 3, y + 3)]
  | Square -> Hash_set.Poly.of_list @@ List.map ~f:tuple_to_point [(x + 3, y); (x + 4, y); (x + 3, y + 1); (x + 4, y + 1)]

type move =
  | Left
  | Right
  | Down

type map = {
  mutable points: point Hash_set.t;
  mutable highest: int;
}

type moves = {
  move_items: move array;
  mutable next_move: int;
}

type rocks = {
  rock_items: rock list;
  mutable next_rock: int;
}

let get_next_move moves =
  if moves.next_move = Array.length moves.move_items
    then 
      moves.next_move <- 1
    else
      moves.next_move <- moves.next_move + 1;
  Array.get moves.move_items (moves.next_move - 1);;

let get_next_rock rocks =
  if rocks.next_rock = List.length rocks.rock_items
    then 
      rocks.next_rock <- 1
    else
      rocks.next_rock <- rocks.next_rock + 1;
  List.nth_exn rocks.rock_items (rocks.next_rock - 1);;

let move_fn = function
  | Left  -> fun {x;y} -> {x = x - 1; y = y}
  | Right -> fun {x;y} -> {x = x + 1; y = y}
  | Down  -> fun {x;y} -> {x = x; y = y - 1} 

let move_rock rock move = 
  let points = Hash_set.to_list rock in
  let fn = move_fn move in
  Hash_set.Poly.of_list @@ List.map ~f:fn points

let does_collide rock map = 
  let left = Option.value_exn @@ Hash_set.min_elt ~compare:(fun a b -> Int.compare a.x b.x) rock in
  let right = Option.value_exn @@ Hash_set.max_elt ~compare:(fun a b -> Int.compare a.x b.x) rock in
  let bottom = Option.value_exn @@ Hash_set.min_elt ~compare:(fun a b -> Int.compare a.y b.y) rock in
  left.x <= 0 || right.x >= 8 || bottom.y <= 0 || Hash_set.length @@ Hash_set.inter rock map.points > 0

let add_rock_to_map rock map = 
  map.points <- Hash_set.union rock map.points;
  Hash_set.filter_inplace ~f:(fun p -> p.y > map.highest - 50) map.points;;

let create_figures : (unit -> rocks)= fun _ -> {rock_items = [Minus; Plus; Corner; Pipe; Square]; next_rock = 0};;

let create_map : (unit -> map) = fun _ -> {points = Hash_set.Poly.create (); highest = 0};;

let parse line = 
  let f = function
    | '<' -> Left
    | '>' -> Right
    | _   -> raise @@ Exception "Unknown move" in
  {move_items = Array.of_list @@ List.map ~f:f @@ String.to_list line; next_move = 0}

let rec place_rock rock map moves =
  let next_move = get_next_move moves in
  let next_rock = move_rock rock next_move in
  let next_rock = if does_collide next_rock map then rock else next_rock in
  let next_rock' = move_rock next_rock Down in
  if does_collide next_rock' map
    then (
      add_rock_to_map next_rock map;
      map.highest <- (Option.value_exn @@ Hash_set.max_elt ~compare:(fun a b -> Int.compare a.y b.y) map.points).y
    )
    else (
      place_rock next_rock' map moves;
    );;

let part1 lines = 
  let moves = parse @@ List.hd_exn lines in
  let figures = create_figures () in
  let map = create_map () in
  for _ = 1 to 2022 do
    let rock = create_rock {x = 0; y = map.highest + 4} (get_next_rock figures) in
    place_rock rock map moves;
  done;
  map.highest;;

let part2 lines = 
  let moves = parse @@ List.hd_exn lines in
  let figures = create_figures () in
  let map = create_map () in
  let iters = 1_000_000_000_000 in
  let period = 1715 in
  let period_value = 2613 in
  (* period and period_value for test.txt
  let period = 7 * 5 * (Array.length moves.move_items) in
  let period_value = 2120 in
  *)
  for _ = 1 to period + (iters % period) do
    let next_rock = get_next_rock figures in
    let rock = create_rock {x = 0; y = map.highest + 4} next_rock in
    place_rock rock map moves;
  done;
  ((iters - period) / period) * period_value + map.highest;;


let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day17.exe *)