
type colors =
  | Red of int
  | Green
  | Blue of int

let cr:colors = Red 0
let cg:colors = Green


module Point2D = struct
  type t = {
    x: int;
    y: int;
  }
  let zero:t = {x=0; y=0}

  let x_axis:t = {zero with x=1}

  let y_axis:t = {zero with y=1}
end

type move_action =
  | ToPoint2D of Point2D.t
  | ToLocation of string


type 'ty point = {
  x: 'ty;
  y: 'ty;
}


module Exp = struct
  type 'ty t =
    | Porposition of 'ty
    | Const of bool
    | Not of 'ty t
    | And of 'ty t list
    | Or of 'ty t list

  let rec simplify (exp:'ty t) :'ty t =
    let simpl_list l def m =
      match l with
      | [] -> def
      | [x] -> x
      | a::d -> m l
    in
    match exp with
    | Porposition p -> Porposition p
    | Const b -> Const b
    | Not (Not v) -> (simplify v)
    | Not Const b -> Const (not b)
    | Not (And l) -> simplify (
        Or (
          l
          |> List.map (fun v-> Not v)
          |> List.map simplify
        )
      )
    | Not (Or l) -> simplify (
        And (
          l
          |> List.map (fun v-> Not v)
          |> List.map simplify
        )
      )
    | Not v -> Not (simplify v)
    | And v ->
      let vl = List.map simplify v in
      if [] = (List.filter ((=) (Const false)) vl)
      then
        let vl = List.filter ((<>) (Const true)) vl in
        simpl_list vl (Const true) (fun x->And x)
      else Const false
    | Or v ->
      let vl = List.map simplify v in
      if [] = (List.filter ((=) (Const true)) vl)
      then
        let vl = List.filter ((<>) (Const false)) vl in
        simpl_list vl (Const false) (fun x->Or x)
      else Const true

  let rec to_string fmt exp =
    match exp with
    | Porposition p -> "<" ^ fmt p ^ ">"
    | Const b -> string_of_bool b
    | Not e -> "~" ^ to_string fmt e
    | And l -> let sl = List.map (to_string fmt) l in
      "[& " ^ (String.concat " " sl) ^ "]"
    | Or l -> let sl = List.map (to_string fmt) l in
      "[| " ^ (String.concat " " sl) ^ "]"
end


let exp1 = Exp.And [Exp.Porposition "P"; Exp.Const true]
let exp1_ = Exp.simplify exp1
;;
print_endline (Exp.to_string (fun x->x) exp1)
;;
print_endline (Exp.to_string (fun x->x) exp1_)
;;

open Exp
let exp2 = Not (And [Or [Porposition "it snowing"; (Const true)];
                     (Not (Not (Porposition "it raining")))])

let exp2_ = Exp.simplify exp2
;;
print_endline (Exp.to_string (fun x->x) exp2)
;;
print_endline (Exp.to_string (fun x->x) exp2_)
;;

let arr1 = [`I 0; `F 1.1]
let fun1 = function
  | `I x -> x > 0
  | `F x -> x > 0.
  | `G -> true

let positive1 = List.filter fun1 arr1
(* positive1 :: [< `F of float | `G | `I of int > `F `I ] list *)

let fun2 = function
  | `H h -> fun1 h
  | (`F _|`G|`I _) as a -> fun1 a

