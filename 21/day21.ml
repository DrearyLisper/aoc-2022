open Base
open Stdio

exception Exception of string

type value = 
  | Constant of int
  | Add of string * string
  | Sub of string * string 
  | Mul of string * string
  | Div of string * string

let parse_line line = 
  let matches =
    let pattern = "^([a-z]+): (([a-z]+) ([\\/*+-]) ([a-z]+)|([0-9]+))$" in
    Re.exec (Re.Perl.compile_pat pattern) line
  in
    match Re.Group.all matches with
      | [|_; n1; _; n2; "+"; n3; _|] -> (n1, Add (n2, n3))
      | [|_; n1; _; n2; "-"; n3; _|] -> (n1, Sub (n2, n3))
      | [|_; n1; _; n2; "/"; n3; _|] -> (n1, Div (n2, n3))
      | [|_; n1; _; n2; "*"; n3; _|] -> (n1, Mul (n2, n3))
      | [|_; n1; v; _; _; _; _|] -> (n1, Constant (Int.of_string v))
      | _ -> raise @@ Exception "Can't parse line";;

let create_state lines =
  let state = Hashtbl.Poly.create () in
  let add_to_state (name, value) = 
    Hashtbl.set state ~key:name ~data:value in 
  List.map lines ~f:parse_line
  |> List.iter ~f:add_to_state;
  state;;

let rec eval state name =
  let value = Hashtbl.find_exn state name in
  let unboxed = 
    match value with 
    | Constant v -> v
    | Add (v1, v2) -> (eval state v1) + (eval state v2)
    | Sub (v1, v2) -> (eval state v1) - (eval state v2)
    | Div (v1, v2) -> (eval state v1) / (eval state v2)
    | Mul (v1, v2) -> (eval state v1) * (eval state v2)
  in
  Hashtbl.set state ~key:name ~data:(Constant unboxed);
  unboxed;;

let part1 lines = 
  let state = create_state lines in
  eval state "root";;


let rec pivot name state new_state =
  let any_of_value_is name value = 
    let open String in
    match value with
    | Add (a, b) -> a = name || b = name
    | Sub (a, b) -> a = name || b = name
    | Mul (a, b) -> a = name || b = name
    | Div (a, b) -> a = name || b = name
    | _          -> false in
  let k = List.hd_exn @@ Hashtbl.keys @@ Hashtbl.filter state ~f:(any_of_value_is name) in
  let v = Hashtbl.find_exn state k in
  let new_v = 
    let open String in
    match v with 
    | Add (a, b) ->
      let vv = match a = name, k <> "root" with
      | (true, true)   -> Sub (k, b)
      | (false, true)  -> Sub (k, a)
      | (true, false)  -> Sub (b, k)
      | (false, false) -> Sub (a, k)
      in vv
    | Sub (a, b) ->
      if a = name
        then Add (k, b)
        else Sub (a, k)
    | Mul (a, b) ->
      if a = name
        then Div (k, b)
        else Div (k, a)
    | Div (a, b) ->
      if a = name
        then Mul (k, b)
        else Div (a, k)
    | other -> other
  in
  let open String in
  if k <> "root"
    then begin
      Hashtbl.remove new_state k;
      Hashtbl.remove state k;
      Hashtbl.set new_state ~key:name ~data:new_v;
      pivot k state new_state;
    end else begin
      Hashtbl.remove new_state k;
      Hashtbl.remove state k;
      Hashtbl.set new_state ~key:name ~data:new_v;
      Hashtbl.set new_state ~key:k ~data:(Constant 0);
    end;;

let postprocess state new_state =
  Hashtbl.remove state "humn";
  pivot "humn" state new_state;;
 
let part2 lines = 
  let state = create_state lines in 
  let new_state = Hashtbl.copy state in
  postprocess state new_state;
  eval new_state "humn";;

let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day21.exe *)
