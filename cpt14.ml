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

let do_hash path () =
  try
    In_channel.with_file path ~f:(fun ic ->
        Digest.input ic
        |> Digest.to_hex
        |> print_endline
      )
  with Sys_error _ -> raise Not_found (* TODO: only can catch exception in here *)

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"print MD5 of file."
    ~readme:(fun () -> "this line is more info of MD5 and this command.")
    Command.Spec.(empty +> anon ("filename" %: file))
    do_hash

let f () =
  (* TODO: cannt handle exception in Command.run *)
  Command.run
    ~version:"0.1"
    ~build_info:"RWO"
    command

let _ = f ()














