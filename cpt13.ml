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
   ABORT
   *)

;;


open Core.Std

let week_days_= weekdays

let finded2 = List.Assoc.find weekdays 2


type t = int String.Map.t

let empty = String.Map.empty

let to_list a = Map.to_alist a

let touch a s =
  let count =
    match Map.find a s with
    | None -> 0
    | Some x -> x
  in
  Map.add a ~key:s ~data:(count + 1)

;;
let _ = match Map.of_alist Int.comparator weekdays with
  | `Duplicate_key x -> ()
  | `Ok weekdays_map ->

    let _weekdays_map_fix6 = Map.add weekdays_map 6 "sunday" in ()

let left = String.Map.of_alist_exn ["foo",1; "bar",3; "snoo",0]
let right = String.Map.of_alist_exn ["foo",0; "snoo",0]
let lr_diff = Map.symmetric_diff left right Int.equal

module ReverseStringComparator = Comparator.Make(struct
    type t = string
    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let compare x y = String.compare y x
  end)

let comparator_reverse = ReverseStringComparator.comparator

let ord_map = Map.of_alist_exn String.comparator ["foo",1; "bar",3; "snoo",0]
let ord_tree = Map.to_tree ord_map

let left_snoo = Map.Tree.find String.comparator ord_tree "snoo"


let ord_comparator_poly = Map.of_alist_exn Comparator.Poly.comparator ["foo",1; "bar",3; "snoo",0]
let ord_map_poly = Map.Poly.of_alist_exn ["foo",1; "bar",3; "snoo",0]

let dedup ~comparator l =
  (List.fold l ~init:(Set.empty comparator) ~f:Set.add)
  |> Set.to_list

let ex_dedup = dedup ~comparator:(Int.comparator) [1;1;1;1;3;3;3;3;5;5;5;5]
;;

Caml.Hashtbl.hash (List.range 0 9) |> print_int;
(* hash List if length over 10 is only hash first 10 element *)
Caml.Hashtbl.hash (List.range 0 10) |> print_int;
Caml.Hashtbl.hash (List.range 0 11) |> print_int;
Caml.Hashtbl.hash (List.range 0 100) |> print_int;

(* Map.change will preserve old data for copies, Hashtbl.change will break old data but faster *)






