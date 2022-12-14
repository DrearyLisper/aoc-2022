open Base
open Stdio

exception Exception of string

type point = {
  x: int;
  y: int;
}

type world = {
  walls : (point) Hash_set.t;
  sand : (point) Hash_set.t;
  max_y : int;
}

let init l =
  l
  |> List.rev
  |> List.tl_exn
  |> List.rev

let wall_points (a, b) = 
  match (a.x = b.x, a.y = b.y) with
  | (true, _) -> List.map ~f:(fun y -> {x = a.x; y = y}) (List.range (min a.y b.y) ((max a.y b.y) + 1))
  | (_, true) -> List.map ~f:(fun x -> {x = x; y = a.y}) (List.range (min a.x b.x) ((max a.x b.x) + 1))
  | _ -> raise @@ Exception "Can't happen (2)"

let parse line = 
  let get_point s = 
    match String.split ~on:',' s with
    | [x;y] -> {x = Int.of_string x; y = Int.of_string y}
    | _ -> raise @@ Exception "Can't happen" in
  let points = line
    |> String.split_on_chars ~on:[' '; '-'; '>']
    |> List.filter ~f:(Fn.compose (not) (String.is_empty))
    |> List.map ~f:get_point in
  List.zip_exn (init points) (List.tl_exn points)
  |> List.concat_map ~f:wall_points

let is_free world p = 
  not (Hash_set.mem world.walls p)
  &&
  not (Hash_set.mem world.sand p)

let rec add_sand world {x; y} =
  match y > world.max_y with
  | true -> false
  | false ->
    match (is_free world {x = x; y = y + 1},
           is_free world {x = x - 1; y = y + 1},
           is_free world {x = x + 1; y = y + 1}) with
      | (true, _, _) -> add_sand world {x = x; y = y + 1}
      | (false, true, _) -> add_sand world {x = x - 1; y = y + 1}
      | (false, false, true) -> add_sand world {x = x + 1; y = y + 1}
      | (false, false, false) -> 
        Hash_set.add world.sand {x = x; y = y};
        true

let is_free' world p = 
  not (Hash_set.mem world.walls p)
  &&
  not (Hash_set.mem world.sand p)
  &&
  not (p.y = world.max_y + 2)

let rec add_sand' world {x; y} =
  match (x = 500 && y = 0 && not (is_free' world {x; y})) with
  | true -> false
  | false ->
    match (is_free' world {x = x; y = y + 1},
           is_free' world {x = x - 1; y = y + 1},
           is_free' world {x = x + 1; y = y + 1}) with
      | (true, _, _) -> add_sand' world {x = x; y = y + 1}
      | (false, true, _) -> add_sand' world {x = x - 1; y = y + 1}
      | (false, false, true) -> add_sand' world {x = x + 1; y = y + 1}
      | (false, false, false) -> 
        Hash_set.add world.sand {x = x; y = y};
        true

let part1 lines = 
  let points = lines
    |> List.concat_map ~f:parse in
  let world = {
      walls = Hash_set.Poly.of_list points;
      sand = Hash_set.Poly.create ();
      max_y = points |> List.map ~f:(fun p -> p.y) |> List.fold ~init:(-1) ~f:max
    } in
  while add_sand world {x = 500; y = 0} do () done;
  Hash_set.length world.sand

let part2 lines =
  let points = lines
    |> List.concat_map ~f:parse in
  let world = {
      walls = Hash_set.Poly.of_list points;
      sand = Hash_set.Poly.create ();
      max_y = points |> List.map ~f:(fun p -> p.y) |> List.fold ~init:(-1) ~f:max
    } in
  while add_sand' world {x = 500; y = 0} do () done;
  Hash_set.length world.sand

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day14.exe *)