open Base
open Stdio

exception Exception of string

type item =
  | File of (string * int)
  | Dir of string 

type dir = {
  name: string;
  files: item list;
}

type command = 
  | Cd of string
  | Ls

type state = {
  mutable pwd : string list;
  fs : (string, dir) Hashtbl.t;
}

let parse_command line = 
  if not @@ String.is_prefix ~prefix:"$" line 
    then None
    else match String.split ~on:' ' line with
      | ["$"; "cd"; x] -> Some (Cd x)
      | ["$"; "ls"]    -> Some Ls 
      | _              -> None

let rec parse_ls_output = function
| [] -> []
| line :: lines ->
  match String.split ~on:' ' line with
    | ["dir"; x] -> Dir x :: (parse_ls_output lines)
    | "$" :: _   -> []
    | [size; x]  -> File (x, Int.of_string size) :: (parse_ls_output lines)
    | _          -> []


let sum = List.fold ~init:0 ~f:(+)

let rec parse state = function
| [] -> ();
| line :: lines ->
  match parse_command line with
    | Some (Cd x) -> 
      let open String in
      if x = ".."
        then state.pwd <- List.tl_exn state.pwd
        else state.pwd <- x :: state.pwd;
      parse state lines;
    | Some (Ls) ->
      let items = parse_ls_output lines in
      let dir' = { name = List.hd_exn state.pwd; files = items } in
      Hashtbl.set state.fs ~key:(String.concat ~sep:":" state.pwd) ~data:dir';
      parse state (List.drop_while ~f:(fun x -> not @@ String.is_prefix ~prefix:"$" x) lines);
    | None -> raise @@ Exception "Can't parse command";;

let rec sizes ?(start_dir = "/") state' = 
  let dir = Hashtbl.find_exn state'.fs start_dir in

  let item_size = function
  | File (_, size) -> 
    [("$f$", size)];
  | Dir name ->
    sizes ?start_dir:(Some (String.concat ~sep:":" [name; start_dir])) state' in

  let all_sizes = List.map ~f:item_size dir.files in
  let all_sizes_concat = List.concat all_sizes in

  let second (_, b) = b in
  let dir_size = sum @@ List.map ~f:(fun xs -> second @@ List.hd_exn xs) all_sizes in
  (dir.name, dir_size) :: (List.filter ~f:(fun (t, _) -> String.(<>) t "$f$") all_sizes_concat)

let part1 lines = 
  let state = { pwd = []; fs = Hashtbl.create ~size:100 (module String)} in
  parse state lines;
  let all_sizes = sizes state in
  all_sizes
  |> List.map ~f:(fun (_, s) -> s)
  |> List.filter ~f:(fun s -> s <= 100000)
  |> sum

let part2 lines =
  let state = { pwd = []; fs = Hashtbl.create ~size:100 (module String)} in
  parse state lines;
  let all_sizes = sizes state in
  let second (_, b) = b in
  let unused_space = 70_000_000 - (second @@ List.hd_exn all_sizes) in
  all_sizes
  |> List.filter ~f:(fun (_, s) -> s >= 30_000_000 - unused_space)
  |> List.rev
  |> List.hd_exn
  |> second

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day07.exe *)