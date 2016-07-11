
let r = ref 0;;

!r ;;

r := !r + 1;;


for i = 0 to 3 do
  print_string "i = "; print_endline (string_of_int i); print_newline ()
done
;;


