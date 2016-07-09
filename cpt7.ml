
let arr1 = [2;3;5;7;11;13]
;;

let ev d =
  (d mod 2) == 0
;;

let find_ev = List.find ev arr1
let find_100 =
  try List.find ((=) 100) arr1
  with exn -> 0

let bind_option opt f =
  match opt with
  | None -> None
  | Some x -> Some (f x)

;;
bind_option (Some 1) (fun x-> Some (x+1))
