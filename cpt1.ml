
(*1.1*)
let hello = "world";;

let inc x = x + 1;;

print_endline hello;;

(*1.2*)
let square x = x*x;;

print_endline (string_of_int (square 2));;

let ratio x y = float_of_int x /. float_of_int y;;

let sum2_if_true f a b =
  (if f a then a else 0)
  +
  (if f b then b else 0);;

let first_if_true f a b =
  if f a then a else b;;


let is_cat s = s=="cat";;

let first_if_cat = first_if_true is_cat "miao" "nya";;
print_endline first_if_cat;;

(*1.3*)
let tuple1 = (1,"1");;
let one_int, one_str = tuple1;;
let (one_int, one_str) = tuple1;;

let hello_length = String.length "hello";;

let distance (x1,y1) (x2,y2) =
  sqrt (
    (x1 -. x2)**2. +.
    (y1 -. y2)**2.
  );;

let list1 = [1;2;3;4;5];;
let list1_inc = List.map inc list1;;
(*let list1_inc1 = List.map ~f:String.length ["TODO"];; (*WHY NOT??*)*)

let list2 = 0::list1;;
let list4 = 0::[];;
let list4 = [-2;-1]@list2;;

let rec match_list l f =
  match l with
  | [] -> []
  | h::t -> f h :: match_list t f;;

let divide x y =
  if y = 0
  then None
  else
    let d = (x / y)
    in Some d;;

(*1.4*)
type point2d = {x:float; y:float};;
let point2d_Z = {x=0.; y=0.};;

let point2d_distance {x=x1;y=y1} {x=x2;y=y2} =
  distance (x1,y1) (x2,y2);;

let point2d_get_x {x=r} = r;;
let point2d_get_y {y} = y;;

type circle_t = {center:point2d; radius:float};;
type square_t = {left_top:point2d; right_bottom:point2d};;

type simple_shape =
  | Circle(*NOTICE: upper case*) of circle_t  (*of style*)
  | Square of square_t;;


let inside_shape point shape =
  let inside_circle p circle =
    (point2d_distance p circle.center) <= circle.radius
  in
  let inside_square {x;y} {left_top;right_bottom} =
    left_top.x <= x && x < right_bottom.x &&
    left_top.y <= y && y < right_bottom.y
  in
  match shape with
  | Circle c -> inside_circle point c
  | Square s -> inside_square point s;;


let inside_scene point scene =
  List.exists (fun shape -> inside_shape point shape) scene;;

print_endline (string_of_bool
                 (inside_scene
                    point2d_Z
                    [Circle {center=point2d_Z; radius=1.}]));;

 
