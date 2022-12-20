open Base
open Stdio

(* exception Exception of string *)

type map = {
  mutable tunnels: (string, (string * int) list) Hashtbl.t;
  rates: (string, int) Hashtbl.t;
  mutable rate_sum : int;
}

let bfs map start = 
  let queue = Queue.create () in
  let distances = Hashtbl.Poly.create () in
  Queue.enqueue queue (0, start);
  let rec loop queue = 
    if Queue.length queue = 0
      then ()
      else (
        let (distance, valve) = Queue.dequeue_exn queue in
        let neighbours = Hashtbl.find_exn map.tunnels valve in
        neighbours
        |> List.map ~f:(fun (a, _) -> a)
        |> List.filter ~f:(fun n -> not (Hashtbl.mem distances n))
        |> List.iter ~f:(fun n -> Hashtbl.set distances ~key:n ~data:(distance + 1); Queue.enqueue queue (distance + 1, n));
        loop queue;
      ) in
  loop queue;
  distances;;

let enrich map = 
  let new_hash = Hashtbl.Poly.create () in
  let important = "AA" :: (Hashtbl.keys map.tunnels |> List.filter ~f:(fun t -> Hashtbl.find_exn map.rates t > 0)) in
  important
  |> List.iter ~f:(fun n -> 
    let distances = bfs map n in
    let new_tunnels = 
      important
      |> List.filter ~f:(fun n' -> String.(<>) "AA" n')
      |> List.filter ~f:(fun n' -> String.(<>) n n')
      |> List.map ~f:(fun n' -> (n', Hashtbl.find_exn distances n')) in
    Hashtbl.set new_hash ~key:n ~data:new_tunnels
  );
  map.tunnels <- new_hash;
  map;;

(* Valve AA has flow rate=0; tunnels lead to valves DD, II, BB *)
let (<->) f g = Fn.compose f g

let parse line =
  let matches =
    let pattern = "^Valve ([A-Z]+) has flow rate=([0-9]+); tunnels* leads* to valves* ([A-Z, ]+)$" in
    Re.exec (Re.Posix.compile_pat pattern) line
  in
  (Re.Group.get matches 1,
   Int.of_string @@ Re.Group.get matches 2,
   Re.Group.get matches 3
   |> String.split_on_chars ~on:[' '; ',']
   |> List.filter ~f:(not <-> String.is_empty)
   |> List.map ~f:(fun t -> (t, 1)))

let add_tunnels map (from, rate, to') = 
  Hashtbl.set map.tunnels ~key:from ~data:to';
  Hashtbl.set map.rates ~key:from ~data:rate;
  map.rate_sum <- map.rate_sum + rate;
  map

let find map =
  let max_rates = ref 0 in
  let rec find' (rate, time, used_rate, path) = 
    if rate + (map.rate_sum - used_rate) * (time - 2) < !max_rates
      then rate
      else (
        max_rates := max !max_rates rate;
    let current = List.hd_exn path in
    let neighbours = Hashtbl.find_exn map.tunnels current in
    if time <= 0
      then rate
      else (
        let next (gate, distance) =
          let gate_rate = Hashtbl.find_exn map.rates gate in
          let new_rate = rate + (time - 1 - distance) * (if (List.mem path gate ~equal:String.equal) then 0 else gate_rate) in
          find' (new_rate, time - 1 - distance, used_rate + gate_rate, gate :: path) in
        List.map ~f:next neighbours
        |> List.fold ~init:0 ~f:max
      )) in
  find' (0, 30, 0, ["AA"])

let find2 map =
  let max_rates = ref 0 in
  let rec find' (rate, (time, elephant_time), used_rate, (current, current_elephant), path) = 
   if rate + (map.rate_sum - used_rate) * (min time elephant_time - 2) < !max_rates
      then rate
      else (
        max_rates := max !max_rates rate;
    let neighbours = Hashtbl.find_exn map.tunnels current in
    let elephant_neighbours = Hashtbl.find_exn map.tunnels current_elephant in
    if time <= 0 || elephant_time <= 0
      then rate
      else (
        let next next_time is_elephant (gate, distance) =
          let gate_rate = Hashtbl.find_exn map.rates gate in
          let new_rate = rate + (next_time - 1 - distance) * (if (List.mem path gate ~equal:String.equal) then 0 else gate_rate) in
          if is_elephant
            then [ find' (new_rate, (time, next_time - 1 - distance), used_rate + gate_rate, (current, gate), gate :: path); ]
            else [ find' (new_rate, (next_time - 1 - distance, elephant_time), used_rate + gate_rate, (gate, current_elephant), gate :: path); ]
         in
        List.concat [List.concat_map ~f:(next time false) neighbours; List.concat_map ~f:(next elephant_time true) elephant_neighbours]
        |> List.fold ~init:0 ~f:max
      )) in
  find' (0, (26, 26), 0, ("AA", "AA"), ["AA"])

let part1 lines = 
  let map = {tunnels = Hashtbl.Poly.create (); rates = Hashtbl.Poly.create (); rate_sum = 0} in
  lines
  |> List.map ~f:parse
  |> List.fold ~init:map ~f:add_tunnels
  |> enrich
  |> find

let part2 lines = 
  let map = {tunnels = Hashtbl.Poly.create (); rates = Hashtbl.Poly.create (); rate_sum = 0} in
  lines
  |> List.map ~f:parse
  |> List.fold ~init:map ~f:add_tunnels
  |> enrich
  |> find2

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day16.exe *)