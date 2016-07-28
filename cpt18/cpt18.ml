#require "async"

open Async.Std

let with_def dv = function
  | None -> dv
  | Some v -> "Some " ^ v

let cpt18 = Reader.file_contents "cpt18.ml"
;;
let peeks = Deferred.peek cpt18
;;
print_endline (with_def "None" peeks)
;;

let uppercase_file f_defeer =
  Deferred.bind
    f_defeer
    (fun x -> Deferred.return (String.uppercase_ascii x))

let uppercase_file_ret x =
  Deferred.return (String.uppercase_ascii x)

let lowercase_file_native x =
  String.lowercase_ascii x

let map_file f filename =
  Reader.file_contents filename
  >>= f
  >>= (fun x -> Writer.save filename ~contents:x)

let map_file_nf nf filename =
  Reader.file_contents filename
  >>| nf
  >>= (fun x -> Writer.save filename ~contents:x)

;;
let up_file =
  map_file uppercase_file_ret "test.txt"
;;

let up_file_2 =
  map_file_nf lowercase_file_native "test.txt"
;;

let _ = up_file;;
let _ = up_file_2;;

let ivar = Ivar.create ();;
let def = Ivar.read ivar;;
Ivar.fill ivar "hello";;
Deferred.peek def;; (* string option = Some "hello" *)

upon;;

let () =
   Core.Std.never_returns (Scheduler.go ());;

