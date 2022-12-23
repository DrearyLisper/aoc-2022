open Base
open Stdio

exception Exception of string

type point = {
  x: int;
  y: int;
  z: int;
}

type grid = {
  cubes: point Hash_set.t;
  mutable free_sides: int;
}

let create_grid : (unit -> grid) = fun _ -> {cubes = Hash_set.Poly.create (); free_sides = 0};;

let neighbours {x;y;z} = 
  [
    (x - 1,y,z);
    (x + 1,y,z);
    (x,y + 1,z);
    (x,y - 1,z);
    (x,y,z + 1);
    (x,y,z - 1);
  ]
  |> List.map ~f:(fun (x,y,z) -> {x=x;y=y;z=z})
  |> Hash_set.Poly.of_list

let parse line =
  match String.split ~on:',' line with
  | [x;y;z] -> {x = Int.of_string x; y = Int.of_string y; z = Int.of_string z}
  | _       -> raise @@ Exception "Bad input";;

let add_point grid point = 
  Hash_set.add grid.cubes point;
  let n = neighbours point in
  let found = Hash_set.length @@ Hash_set.inter grid.cubes n in
  grid.free_sides <- grid.free_sides + 6 - 2 * found;
  ();;

let add_point' grid point = 
  let n = neighbours point in
  let found = Hash_set.length @@ Hash_set.inter grid.cubes n in
  grid.free_sides <- grid.free_sides - found;
  ();;

let bfs p grid = 
  let queue = Queue.create () in
  let added = ref (Hash_set.Poly.create ()) in
  let is_good_point {x;y;z} =
    x >= -1 && y >= -1 && z >= -1 && x <= 25 && y <= 25 && z <= 25 in
  let bfs' queue = 
    while Queue.length queue > 0 do
      let point = Queue.dequeue_exn queue in
      let neighbours = neighbours point in
      let neighbours = Hash_set.diff neighbours grid.cubes in
      let neighbours = Hash_set.diff neighbours !added in
      let neighbours = Hash_set.filter ~f:is_good_point neighbours in
      Queue.enqueue_all queue (Hash_set.to_list neighbours);
      added := Hash_set.union !added neighbours;
    done in
  Queue.enqueue queue p;
  bfs' queue;
  !added;;

let part1 lines = 
  let points = List.map ~f:parse lines in
  let grid = create_grid () in
  List.iter ~f:(add_point grid) points;
  grid.free_sides;;

let part2 lines = 
  let points = List.map ~f:parse lines in
  let grid = create_grid () in
  List.iter ~f:(add_point grid) points;
  let outer_cubes = bfs {x=0;y=0;z=0} grid in
  let all_sides = grid.free_sides in
  List.iter ~f:(add_point' grid) (Hash_set.to_list outer_cubes);
  all_sides - grid.free_sides;;
 
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day18.exe *)