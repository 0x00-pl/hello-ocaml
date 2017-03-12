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
  (* cannt handle exception in Command.run *)
  (* because it's exit 1 *)
  Command.run
    ~version:"0.1"
    ~build_info:"RWO"
    command

let regular_file =
  Command.Arg_type.create
    (fun filename ->
       match Sys.is_file ~follow_symlinks:true filename with
       | `Yes -> filename
       | `No | `Unknown ->
         eprintf "'%s' is not a regular file.\n%!" filename;
         exit 1)

let command_1 =
  Command.basic
    ~summary:"print MD5 of file."
    ~readme:(fun () -> "this line is more info of MD5 with file type check and this command.")
    Command.Spec.(empty +> anon ("filename" %: regular_file))
    do_hash


let command_2 =
  Command.basic
    ~summary:"print MD5 of file."
    ~readme:(fun () -> "this line is more info of MD5 with file type check and this command.")
    Command.Spec.(empty +> anon (maybe ("filename" %: regular_file)))
    (fun filename_option () -> match filename_option with
       | None -> do_hash "stdin" ()
       | Some filename -> do_hash filename ())


let command_3_echo =
  Command.basic
    "echo argv"
    Command.Spec.(empty +> anon (sequence ("argv" %: string)))
    (fun l () -> l |> String.concat ~sep:" " |> print_endline)

let command_4_echo =
  Command.basic
    "echo argv"
    Command.Spec.(
      empty
      +> flag "-v" no_arg ~doc:"show version."
      +> anon (sequence ("argv" %: string))
    )
    (fun show_version l () ->
       if show_version then
         print_endline "version 0.1"
       else
         l |> String.concat ~sep:" " |> print_endline)

