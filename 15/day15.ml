open Base
open Stdio

(* exception Exception of string *)

type point = {
  x: int;
  y: int;
}

type rectangle = {
  sensor: point;
  r: int;
}

let manhattan a b = abs (a.x - b.x) + abs (a.y - b.y)

(* Sensor at x=1943362, y=12808: closest beacon is at x=1861152, y=-42022 *)
let parse line =
  let matches =
    let pattern = "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)" in
    Re.exec (Re.Posix.compile_pat pattern) line
  in
  { x = Int.of_string @@ Re.Group.get matches 1; y = Int.of_string @@ Re.Group.get matches 2},
  { x = Int.of_string @@ Re.Group.get matches 3; y = Int.of_string @@ Re.Group.get matches 4}

let create_rect (sensor, beacon) = 
  let distance = manhattan sensor beacon in
  {sensor = sensor; r = distance}

let slice y rect =
  let distance' = manhattan rect.sensor {x = rect.sensor.x; y = y} in
  if abs (y - rect.sensor.y) > rect.r 
    then None 
    else Some (rect.sensor.x - (rect.r - distance'), rect.sensor.x + (rect.r - distance'))

let merge (al, ar) (bl, br) = 
  if ar < bl 
    then [(bl, br); (al, ar)]
    else [(al, max ar br)]

let part1 lines = 
  let slices = lines
    |> List.map ~f:parse
    |> List.map ~f:create_rect
    |> List.map ~f:(slice 2000000)
    |> List.filter ~f:Option.is_some
    |> List.map ~f:(fun v -> Option.value_exn v)
    |> List.sort ~compare:Poly.compare in
  List.tl_exn slices 
  |> List.fold ~init:[List.hd_exn slices] ~f:(fun ass b -> (merge (List.hd_exn ass) b) @ (List.tl_exn ass)) 
  |> List.map ~f:(fun (a, b) -> b - a)
  |> List.fold ~init:0 ~f:(+)

let part2 lines =
  let rects = lines
      |> List.map ~f:parse
      |> List.map ~f:create_rect in
  let slice_all y =
    let slices =
      rects
      |> List.map ~f:(slice y)
      |> List.filter ~f:Option.is_some
      |> List.map ~f:(fun v -> Option.value_exn v)
      |> List.sort ~compare:Poly.compare in
    List.tl_exn slices 
    |> List.fold ~init:[List.hd_exn slices] ~f:(fun ass b -> (merge (List.hd_exn ass) b) @ (List.tl_exn ass)) in
  let (row, slices) = List.range 0 4000001
    |> List.map ~f:(fun y -> (y, slice_all y))
    |> List.filter ~f:(fun (_, s) -> List.length s > 1)
    |> List.hd_exn in
  let (column, _) = List.hd_exn slices in
  4000000 * (column - 1) + row 
    
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day15.exe *)