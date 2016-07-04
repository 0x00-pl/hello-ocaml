let str1 = "1:2:3:4"

let str2 =
  let sl = String.split_on_char ':' str1 in
  String.concat "-" sl


let rec unzip2 l =
  match l with
  | [] -> [],[]
  | (a,b)::t ->
    let ta,tb = unzip2 t in
    a::ta, b::tb




let string_unzip () =
  let i = unzip2 [(1,"2");(3,"4")] in
  i

;;

let rec is_even x =
  if x=0 then true else is_odd (x-1)
and is_odd x =
  if x=0 then false else is_even (x-1)

let test_let () =
  let rec x=1 and y=2 in
  x,y
;;

let (+|) x y = x+y

let r = 1 +| 2

;;

let (|>) x f = f x

type ab = A|B

let ab_id1 ab =
  match ab with
  | A -> A
  | B -> B

let ab_id2 = function
  | A -> A
  | B -> B

let ab_def default = function
  | A -> default
  | B -> default

let add12 ?y x =
  let yy = match y with None->0 | Some x->x in
  x+yy

let add12_t1 = add12 1
let add12_t2 = add12 ~y:1 2

let add123 ?(x=0) ?(y=0) ?(z=0) =
  x+y+z

let add123_t0 = add123
let add123_t1 = add123 ~x:1
let add123_t2 = add123 ~y:1 ~x:2
let add123_t3 = (add123 ~x:1 ~z:2)(*curring*) ~y:3

