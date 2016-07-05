

let read_file =
  let file = open_in "cpt1.ml" in
  let read_line_from_file = try Some (input_line file) with End_of_file -> None in
  let rec read_all =
    match read_line_from_file with
    | None -> []
    | Some l -> l :: read_all in
  read_all



module type incs = sig
  type t
  val inc : t -> t
end

module Incstr = struct
  type t = string
  let inc s = s ^ "*"
end

module Incss : incs = Incstr


let inc1 a = a+1
open Incss

let incsss = Incss.inc









