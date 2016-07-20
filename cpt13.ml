let weekdays = [
  0, "sunday";
  1, "monday";
  2, "tuesday";
  3, "wednesday";
  4, "thursday";
  5, "friday";
  6, "saturdau"
]
;;
print_endline (List.assoc 5 weekdays)

(* --- *)

type 'a pp = P of 'a
let b = function P x -> x

module type B = sig
  type 't fc
  (* val app : ('a->'b) fc -> 'a fc -> 'b fc *)
  val app : ('a->'b) fc -> ('a) fc -> ('b) fc
  val lam : (('a->'a) fc -> ('a->'b) fc) -> ('a->'b) fc
end

let id x = x


module B_nicked
  : B with type 't fc = 't pp = struct
  type 't fc = 't pp
  let app f x = P ((b f) (b x))
  let lam lm = (lm (P id)) (* P (fun x -> b (lm (P (fun c -> x))) x)*)
end

module B_nicked_example = struct
  open B_nicked

  let u x = P (fun a -> b x)

  let fid = lam(fun x->x)
  let r0 = app (u fid) (u (P 0.))
  ;;
  print_float ((b r0) 0.)

  let rapp = app (u (P (fun x-> float_of_int x))) (u (P 1))
  let rapp2 = app (u (lam (fun x-> app (u (P float_of_int)) x))) (u (P 1))
  ;;
  print_float ((b rapp2) 0)
(*
  let fx = (fun f -> fun x -> f x)
  let lfx = lam(fun f-> (lam(fun x-> app (u f) x)))
  let r2 = app (app (u lfx) (u (P float_of_int))) (u (P 2))
  ;;
  print_float ((b r2) 0)
*)
  let lxf = lam(fun x-> P(fun a -> a))
  let r3 = (app lxf ((P 1)))
  ;;
  print_int (b r3)

  let lxy = lam(fun x-> lam(fun y-> P(fun a b -> b)))
  let r4 = app (app lxy (P 41)) (P 42)
  ;;
  print_int (b r4)

  let appfx = app (P (fun v a b-> a (v(b)))) (P(fun a -> print_int a; a))
  let lxy_ = lam(fun x->
      print_float ((b x) float_of_int 5); lam(fun y->
          print_float ((b y) float_of_int 6); appfx ))
  let r5 = app (app lxy_ (P float_of_int)) (P 42)
  ;;
  print_float (b r5)
  ;;
  print_newline ();;

  let app2 (f:('a->'b->'c)pp) (x:('a->'b)pp) (a:'a pp) : 'c pp
    = P (((b f) (b a)) ((b x) (b a)))

  let app3 (f:'a->'b->'c->'d) (x:'a->'b->'c) (a:'a) (b:'b) : 'd
    = (f a b) (x a b)

  let app_3 (abc:('a->'a->'a)pp) a = (app2 abc (u a))
  let lxyz = lam(fun x->
      print_float ((b x) float_of_int 5); lam(fun y->
          print_float ((b y) float_of_int 6); appfx ))
  let r6 = app (app lxyz (P float_of_int)) (P 42)
  ;;
  print_float (b r5)
end

(*
module B_opt
  : B with type 't fc = 't option = struct
  type 't fc = 't option
  let app f x =
    match f,x with
    | None, _ | _,None -> None
    | Some f', Some x'-> Some (f' x')

  let lam (lm: ('a->'a) fc -> ('a->'b) fc) = lm None
end

module B_opt_example = struct
  open B_opt
  let u = function
    | None -> None
    | Some x -> Some (fun y -> x)

  let print_int_opt o=
    print_endline
      (match o with
       | None -> "None"
       | Some v -> string_of_int v)

  let arg32:(int->int->int) fc =
    lam (fun (x:('a->'a) fc) -> (lam (fun y -> x)))
  let ret32 = app (app arg32 (Some 0)) (Some 1)
  ;;
  print_int_opt ret32
end *)
(*
open B_opt
let arg32opt =
  lam (fun (f:(int->int) option) -> u_opt (app f (Some 0)))
let ret32opt =
  app (app arg32opt (Some 9)) (Some 8)

print_int_opt ret32opt


module B_opt
  : B with
    type 't fc = 't option = struct
  type 't fc = 't option
  let app ff fa =
    match ff,fa with
    | None,_ -> None
    | _,None -> None
    | Some a, Some b -> Some (a b)

  let lam (lm : ('a->'a) fc -> ('a->'b) fc) =
    lm (Some id)
end

let oadd x = function
  | None -> None
  | Some y ->
    match x with
    | None -> None
    | Some x -> Some (x+y)

let o_float_of_int = function
  | None -> None
  | Some i -> Some (float_of_int i)


open B_opt

let f_ : (int->float) fc = Some float_of_int
let exp_f = lam (fun x -> (app f_ x))
let exp_r = app exp_f (Some 0)
;;
match exp with None -> () | Some i -> print_int i
*)


















