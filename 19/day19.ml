open Base
open Stdio

type blueprint = {
  id       : int;
  ore      : int * int * int;
  clay     : int * int * int;
  obsidian : int * int * int;
  geode    : int * int * int;
}

let parse line =
  let matches =
    let pattern = "^Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.$" in
    Re.exec (Re.Posix.compile_pat pattern) line
  in
  {
    id       = Int.of_string @@ Re.Group.get matches 1;
    ore      = (Int.of_string @@ Re.Group.get matches 2, 0, 0);
    clay     = (Int.of_string @@ Re.Group.get matches 3, 0, 0);
    obsidian = (Int.of_string @@ Re.Group.get matches 4, Int.of_string @@ Re.Group.get matches 5, 0);
    geode    = (Int.of_string @@ Re.Group.get matches 6, 0, Int.of_string @@ Re.Group.get matches 7);
  }

type state = {
  resources : int * int * int * int;
  robots    : int * int * int * int;
}

let create_state : unit -> state = fun _ ->
  {resources = (0, 0, 0, 0); robots = (1, 0, 0, 0)};;

let build_ore b s ns =
  let (ore, _, _, _) = s.resources in
  let (ns_ore, ns_clay, ns_obsidian, ns_geode) = ns.resources in
  let (r_ore, r_clay, r_obsidian, r_geode) = s.robots in
  let (b_ore, _, _) = b.ore in
  match ore >= b_ore with
  | true -> true, {resources = (ns_ore - b_ore, ns_clay, ns_obsidian, ns_geode); robots = (r_ore + 1, r_clay, r_obsidian, r_geode)}
  | false -> false, s

let build_clay b s ns =
  let (ore, _, _, _) = s.resources in
  let (ns_ore, ns_clay, ns_obsidian, ns_geode) = ns.resources in
  let (r_ore, r_clay, r_obsidian, r_geode) = s.robots in
  let (b_ore, _, _) = b.clay in
  match ore >= b_ore with
  | true -> true, {resources = (ns_ore - b_ore, ns_clay, ns_obsidian, ns_geode); robots = (r_ore, r_clay + 1, r_obsidian, r_geode)}
  | false -> false, s

let build_obsidian b s ns =
  let (ore, clay, _, _) = s.resources in
  let (ns_ore, ns_clay, ns_obsidian, ns_geode) = ns.resources in
  let (r_ore, r_clay, r_obsidian, r_geode) = s.robots in
  let (b_ore, b_clay, _) = b.obsidian in
  match ore >= b_ore && clay >= b_clay with
  | true -> true, {resources = (ns_ore - b_ore, ns_clay - b_clay, ns_obsidian, ns_geode); robots = (r_ore, r_clay, r_obsidian + 1, r_geode)}
  | false -> false, s

let build_geode b s ns =
  let (ore, _, obsidian, _) = s.resources in
  let (ns_ore, ns_clay, ns_obsidian, ns_geode) = ns.resources in
  let (r_ore, r_clay, r_obsidian, r_geode) = s.robots in
  let (b_ore, _, b_obsidian) = b.geode in
  match ore >= b_ore && obsidian >= b_obsidian with
  | true -> true, {resources = (ns_ore - b_ore, ns_clay , ns_obsidian - b_obsidian, ns_geode); robots = (r_ore, r_clay, r_obsidian, r_geode + 1)}
  | false -> false, s

let do_nothing _ _ ns =
  true, ns

let update_state s = 
  let (ore, clay, obsidian, geode) = s.resources in
  let (r_ore, r_clay, r_obsidian, r_geode) = s.robots in
  {resources = (ore + r_ore, clay + r_clay, obsidian + r_obsidian, geode + r_geode); robots = s.robots};;

let potential n b s =
    let (_, _, _, geode) = s.resources in
    let (_, _, _, r_geode) = s.robots in
    let (_, _, _) = b.geode in

    geode + (r_geode + r_geode + n - 1) * (n + 1) / 2 
 
let clay_cost (_, a, _) = a

let find_max_geodes max_m b m s =
  let max_geodes = ref 0 in
  let rec find_max_geodes' b m s =
    let new_s = update_state s in
    let (_, _, _, geode) = new_s.resources in
    let (r_ore, r_clay, _, _) = new_s.robots in
    max_geodes := max (!max_geodes) geode;
    let n = (max_m - m) in
    if m = max_m || potential n b new_s < !max_geodes
                 || r_ore > 4 
                 || r_clay > clay_cost b.obsidian 
      then geode
      else (List.rev [do_nothing b s new_s; build_ore b s new_s; build_clay b s new_s; build_obsidian b s new_s; build_geode b s new_s]
            |> List.filter ~f:(fun (a, _) -> a) 
            |> List.map ~f:(fun (_, b) -> b)
            |> List.map ~f:(find_max_geodes' b (m + 1))
            |> List.max_elt ~compare:Int.compare
            |> Option.value_exn
           ) in
  find_max_geodes' b m s       

let part1 lines = 
  let blueprints = List.map ~f:parse lines in
  let solve_for b = 
    let state = create_state () in
    let geode = find_max_geodes 24 b 1 state in
    (* printf "%d %d\n" b.id geode; *)
    Stdlib.flush_all ();
    b.id * geode in
  blueprints
  |> List.map ~f:solve_for
  |> List.fold ~init:0 ~f:(+)


let part2 lines = 
  let blueprints = List.map ~f:parse lines in
  let solve_for b = 
    let state = create_state () in
    let geode = find_max_geodes 32 b 1 state in
    (* printf "%d %d\n" b.id geode; *)
    Stdlib.flush_all ();
    geode in
  blueprints
  |> fun l -> List.take l 3
  |> List.map ~f:solve_for
  |> List.fold ~init:1 ~f:( * )

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day19.exe *)
