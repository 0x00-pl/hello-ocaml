let lz2 = lazy (1 + 1)

let flz2 = Lazy.force lz2

let time f =
  let start = Sys.time () in
  let _ = f () in
  let end_ = Sys.time () in
  print_string "Time is: ";
  print_float (end_ -. start);
  print_newline ()


let rec x = lazy ((Lazy.force x) + 1)

;;

print_int (Lazy.force x) (* will be error *)

;;

