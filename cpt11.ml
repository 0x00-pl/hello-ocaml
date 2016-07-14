let stack_1 = object
  val mutable v = []

  method pop =
    match v with
    | [] -> None
    | h::t ->
      v <- t;
      Some h

  method push a =
    v <- a::v

end

let default a = function
  | None -> a
  | Some b -> b
;;

print_int (stack_1#pop |> (default 0));
print_newline ()
;;

type 'a stack = <pop: 'a option; push: 'a -> unit>

let cowsay = object
  method say s =
    print_endline ("cowsay: " ^ s)

end

let catsay = object
  method say s =
    print_endline "catsay: nya~"

  method nya =
    "nya~"

end
;;
let map_say =
  List.map (fun (x : <say:string->unit>) -> x#say "hello")
;;
let r =
  map_say [(catsay :> <say:string->unit>); cowsay]
(* must use type cast (catsay :> <say:string->unit>) *)


let const_conter = object
  val c = 0

  method inc =
    {< c = c+1 >}

  method get =
    c
end

let c1 = const_conter#inc

let c2 = c1#inc

(* c1#get = 1 ; c2#get = 2 *)

let arr = [|0;1|]
let f_arr a =
  match a with
  | [||] -> None
  | [| x |] -> None
  | [| x ; y |] -> None
  | _ -> Some 0



module Either = struct
  type ('a, 'b) t
    = Left of 'a
    | Right of 'b

  let left x = Left x
  let right x = Right x

end

module Either_plus: sig
  type (+'a, +'b) t
  val left : 'a -> ('a, 'b) t
  val right : 'b -> ('a, 'b) t
end = Either
(* 
module Either_minus: sig
  type (-'a, -'b) t
  val left : 'a -> ('a, 'b) t
  val right : 'b -> ('a, 'b) t
end = Either
*)


type ('a, 'b) t_abs = 'a -> 'b
type (+'a, +'b) t_sum = A of 'a | B of 'b
type (+'a, +'b) t_mult = 'a * 'b
type (-'a, +'b) t_pow = 'a -> 'b
type (-'a, -'b) t_pow2 = 'a -> 'b -> unit

;;

