let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;


#use "topfind";;
#camlp4o;;
#thread;;
#require "core.top";;
#require "core.syntax";;


open Core.Std

let do_hash path =
  In_channel.with_file path ~f:(fun ic ->
      Digest.input ic
      |> Digest.to_hex
      |> print_endline
  )

let () =
  do_hash "cpt14.ml"

