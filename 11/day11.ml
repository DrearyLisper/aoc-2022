open Base
open Stdio

(* exception Exception of string *)
type monkey = {
  mutable processed : int;
  mutable items : int list;
  operation : int -> int;
  next : int -> int;
}

(*
let testMonkeys = Array.of_list [
  {processed = 0; items = [79; 98]; operation = (fun x -> x * 19); next = (fun x -> if x % 23 = 0 then 2 else 3)};
  {processed = 0; items = [54; 65; 75; 74]; operation = (fun x -> x + 6); next = (fun x -> if x % 19 = 0 then 2 else 0)};
  {processed = 0; items = [79; 60; 97]; operation = (fun x -> x * x); next = (fun x -> if x % 13 = 0 then 1 else 3)};
  {processed = 0; items = [74]; operation = (fun x -> x + 3); next = (fun x -> if x % 17 = 0 then 0 else 1)};
];;
*)


let process_one_monkey m monkeys level_f =
  m.items
  |> List.iter ~f:(fun item ->
    let new_item = level_f (m.operation item) in
    let next_monkey = monkeys.(m.next new_item) in
    next_monkey.items <- List.append next_monkey.items [new_item];
    m.processed <- m.processed + 1;
  );
  m.items <- []

let process_all_monkeys monkeys level_f =
  monkeys
  |> Array.iter ~f:(fun m -> process_one_monkey m monkeys level_f)

let part1 _ = 
  let inputMonkeys = Array.of_list [
    {processed = 0; items = [52; 60; 85; 69; 75; 75]; operation = (fun x -> x * 17); next = (fun x -> if x % 13 = 0 then 6 else 7)};
    {processed = 0; items = [96; 82; 61; 99; 82; 84; 85]; operation = (fun x -> x + 8); next = (fun x -> if x % 7 = 0 then 0 else 7)};
    {processed = 0; items = [95; 79]; operation = (fun x -> x + 6); next = (fun x -> if x % 19 = 0 then 5 else 3)};
    {processed = 0; items = [88; 50; 82; 65; 77]; operation = (fun x -> x * 19); next = (fun x -> if x % 2 = 0 then 4 else 1)};
    {processed = 0; items = [66; 90; 59; 90; 87; 63; 53; 88]; operation = (fun x -> x + 7); next = (fun x -> if x % 5 = 0 then 1 else 0)};
    {processed = 0; items = [92; 75; 62]; operation = (fun x -> x * x); next = (fun x -> if x % 3 = 0 then 3 else 4)};
    {processed = 0; items = [94; 86; 76; 67]; operation = (fun x -> x + 1); next = (fun x -> if x % 11 = 0 then 5 else 2)};
    {processed = 0; items = [57]; operation = (fun x -> x + 2); next = (fun x -> if x % 17 = 0 then 6 else 2)};
  ] in
  let monkeys = Array.copy inputMonkeys in
  for _ = 0 to 19 do
    process_all_monkeys monkeys (fun x -> x / 3)
  done;
  monkeys
  |> Array.map ~f:(fun m -> m.processed)
  |> Array.sorted_copy ~compare:(fun a b -> - Int.compare a b)
  |> fun ms -> ms.(0) * ms.(1)

let part2 _ =
  let inputMonkeys = Array.of_list [
    {processed = 0; items = [52; 60; 85; 69; 75; 75]; operation = (fun x -> x * 17); next = (fun x -> if x % 13 = 0 then 6 else 7)};
    {processed = 0; items = [96; 82; 61; 99; 82; 84; 85]; operation = (fun x -> x + 8); next = (fun x -> if x % 7 = 0 then 0 else 7)};
    {processed = 0; items = [95; 79]; operation = (fun x -> x + 6); next = (fun x -> if x % 19 = 0 then 5 else 3)};
    {processed = 0; items = [88; 50; 82; 65; 77]; operation = (fun x -> x * 19); next = (fun x -> if x % 2 = 0 then 4 else 1)};
    {processed = 0; items = [66; 90; 59; 90; 87; 63; 53; 88]; operation = (fun x -> x + 7); next = (fun x -> if x % 5 = 0 then 1 else 0)};
    {processed = 0; items = [92; 75; 62]; operation = (fun x -> x * x); next = (fun x -> if x % 3 = 0 then 3 else 4)};
    {processed = 0; items = [94; 86; 76; 67]; operation = (fun x -> x + 1); next = (fun x -> if x % 11 = 0 then 5 else 2)};
    {processed = 0; items = [57]; operation = (fun x -> x + 2); next = (fun x -> if x % 17 = 0 then 6 else 2)};
  ] in
  let monkeys = Array.copy inputMonkeys in
  for _ = 0 to 9999 do
    process_all_monkeys monkeys (fun x -> x % (2 * 3 * 5 * 7 * 11 * 13 * 17 * 19))
  done;
  monkeys
  |> Array.map ~f:(fun m -> m.processed)
  |> Array.sorted_copy ~compare:(fun a b -> - Int.compare a b)
  |> fun ms -> ms.(0) * ms.(1)

 
let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day11.exe *)