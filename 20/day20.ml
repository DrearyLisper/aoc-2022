open Base
open Stdio

type node = {
  mutable index : int;
  value         : int;
}

let create_node i v m =
  ref {index = i; value = v * m;}

let compute_shift _ value length =
  value % (length - 1)

let rec move a i n =
  if n <> 0
    then
      let l = Array.get a (i % Array.length a) in
      let r = Array.get a ((i + 1) % Array.length a) in
      Array.set a (i % Array.length a) r;
      Array.set a ((i + 1) % Array.length a) l;
      (!l).index <- (i + 1) % Array.length a; 
      (!r).index <- i % Array.length a; 
      move a ((i + 1) % Array.length a) (n - 1);;

let perform a r =
    let index = !r.index in
    let value = !r.value in
    move a index (compute_shift index value (Array.length a));;

let parse lines m = 
  lines
  |> List.map ~f:Int.of_string
  |> List.mapi ~f:(fun i v -> create_node i v m)
  |> Array.of_list

let part1 l = 
  let lines = Array.copy @@ parse l 1 in
  let refs = Array.copy lines in

  Array.iter ~f:(fun r -> perform lines r;) refs;
 
  let zero = Array.find_exn ~f:(fun n -> (!n.value) = 0) lines in
  let solution index =
    let a = Array.get lines ((index + 1000) % Array.length lines) in
    let b = Array.get lines ((index + 2000) % Array.length lines) in
    let c = Array.get lines ((index + 3000) % Array.length lines) in
    (!a).value + (!b).value + (!c).value in
  solution (!zero).index;;  

let part2 l = 
  let lines = Array.copy @@ parse l 811589153 in
  let refs = Array.copy lines in

  for _ = 1 to 10 do
    Array.iter ~f:(fun r -> perform lines r;) refs;
  done;

  let zero = Array.find_exn ~f:(fun n -> (!n.value) = 0) lines in
  let solution index =
    let a = Array.get lines ((index + 1000) % Array.length lines) in
    let b = Array.get lines ((index + 2000) % Array.length lines) in
    let c = Array.get lines ((index + 3000) % Array.length lines) in
    (!a).value + (!b).value + (!c).value in
  solution (!zero).index;;  


let solve filename =
  let content = In_channel.read_all filename in
  let lines = String.split_lines content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines;;

let () =
  solve "input.txt"

(* dune exec --display quiet --no-print-directory ./day20.exe *)
