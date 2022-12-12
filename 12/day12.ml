open Base
open Stdio

exception Exception of string

type map = {
  mutable start : int * int;
  mutable finish : int * int;
  cells : (int * int, char) Hashtbl.t;
}

let parse lines = 
  let map' = {start = (0, 0); finish = (0, 0); cells = Hashtbl.Poly.create ()} in
  let process_cell column row c = 
    Hashtbl.set map'.cells ~key:(column, row) ~data:c;
    let open Char in
      if c = 'S'
        then 
          (map'.start <- (column, row);
          Hashtbl.set map'.cells ~key:(column, row) ~data:'a';);
      if c = 'E'
        then
          (map'.finish <- (column, row);
          Hashtbl.set map'.cells ~key:(column, row) ~data:'z';)
  in
  let parse row line =
    String.to_list line
    |> List.iteri ~f:(fun column c -> process_cell column row c) in
  lines
  |> List.iteri ~f:(fun row line -> parse row line);
  map';;

let neighbours (x, y) map' seen =
  [(-1, 0); (1, 0); (0, 1); (0, -1)]
  |> List.concat_map ~f:(fun (dx, dy) ->
      let nx = x + dx in
      let ny = y + dy in
      let c = Hashtbl.find map'.cells (x, y) in
      let nc = Hashtbl.find map'.cells (nx, ny) in
      match (c, nc, Hash_set.mem seen (nx, ny)) with
      | (None, _, _) -> []
      | (_, None, _) -> []
      | (_, _, true) -> []
      | (Some c', Some nc', false) ->
        let diff = Char.to_int nc' - Char.to_int c' in
        if diff <= 1
          then [(nx, ny)]
          else []
    )

  let point_equal (ax, ay) (bx, by) = ax = bx && ay = by

let bfs map' =
  let seen = Hash_set.Poly.create () in
  let queue = Queue.create () in
  let rec bfs' queue =
    match Queue.dequeue queue with
    | None -> None
    | Some ((x, y), d) ->
      if point_equal (x, y) map'.finish
        then Some d
        else
          (let neighbours' = neighbours (x, y) map' seen in
          neighbours'
          |> List.iter ~f:(fun (nx, ny) ->
            Hash_set.add seen (nx, ny);
            Queue.enqueue queue ((nx, ny), d + 1));
          bfs' queue)
  in
  Queue.enqueue queue (map'.start, 0);
  Hash_set.add seen map'.start;
  bfs' queue;;

let part1 lines =
 let map' = parse lines in
 match bfs map' with
 | None -> raise @@ Exception "Can't find the end"
 | Some d -> d

let part2 lines =
  let map' = parse lines in
  map'.cells
  |> Hashtbl.filter_mapi ~f:(fun ~key ~data ->
    let map'' = parse lines in
    map''.start <- key;
    let open Char in 
    if data <> 'a'
      then None
      else match bfs map'' with
      | Some d -> Some (data, d)
      | _ -> None)
  |> Hashtbl.data
  |> List.filter ~f:(fun (c, _) -> let open Char in c = 'a')
  |> List.map ~f:(fun (_, d) -> d)
  |> List.min_elt ~compare:Int.compare
  |> function 
    | None -> raise @@ Exception "Can't find the min"
    | Some d -> d
  

 
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day12.exe *)