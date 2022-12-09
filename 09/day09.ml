open Base
open Stdio

(* exception Exception of string *)

type direction = L | R | U | D

type point = {
  x: int;
  y: int;
}

let compare_point a b =
  match (a.x < b.x, a.x > b.x, a.y < b.y, a.y > b.y) with
  | (true, _, _, _) -> -1
  | (false, true, _, _) -> 1
  | (false, false, true, _) -> -1
  | (false, false, false, true) -> 1
  | _ -> 0

let distance a b = max (abs (a.x - b.x)) (abs (a.y - b.y))

let move {x; y} = function
  | L -> {x = x - 1; y = y}
  | R -> {x = x + 1; y = y}
  | U -> {x = x; y = y + 1}
  | D -> {x = x; y = y - 1}

let sign a = 
  match (a > 0, a < 0) with
  | (true, _) -> 1
  | (_, true) -> -1
  | _ -> 0

let catch head tail = 
  if distance head tail <= 1 then tail else
  let hx = head.x in
  let hy = head.y in
  let tx = tail.x in
  let ty = tail.y in
  match (hx = tx, hy = ty) with
  | (true, _) -> {x = tx; y = ty + sign (hy - ty)}
  | (_, true) -> {x = tx + sign (hx - tx); y = ty}
  | _         -> {x = tx + sign (hx - tx); y = ty + sign (hy - ty)}

let step knots d = 
  let rec moveTails new_head = function
    | [] -> [] 
    | tail :: tails -> 
      let new_tail = catch new_head tail in
      new_tail :: moveTails new_tail tails in
  match knots with
    | [] -> []
    | k :: ks ->
      let new_k = move k d in 
      new_k :: moveTails new_k ks;;

let parse =
  let parseLine line =
    match String.split ~on:' ' line with
    | ["L"; x] -> List.init (Int.of_string x) ~f:(Fn.const L)
    | ["R"; x] -> List.init (Int.of_string x) ~f:(Fn.const R)
    | ["U"; x] -> List.init (Int.of_string x) ~f:(Fn.const U)
    | ["D"; x] -> List.init (Int.of_string x) ~f:(Fn.const D)
    | _ -> [] in
  List.concat_map ~f:parseLine

let part1 lines = 
  let directions = parse lines in
  let rope = List.init 2 ~f:(Fn.const {x = 0; y = 0}) in
  let ropes = List.fold ~init:[rope] ~f:(fun rs d -> (step (List.hd_exn rs) d) :: rs) directions in
  let tails = List.map ~f:(fun r -> List.last_exn r) ropes in
  List.length @@ List.dedup_and_sort ~compare:compare_point tails

let part2 lines =
  let directions = parse lines in
  let rope = List.init 10 ~f:(Fn.const {x = 0; y = 0}) in
  let ropes = List.fold ~init:[rope] ~f:(fun rs d -> (step (List.hd_exn rs) d) :: rs) directions in
  let tails = List.map ~f:(fun r -> List.last_exn r) ropes in
  List.length @@ List.dedup_and_sort ~compare:compare_point tails

   
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day09.exe *)