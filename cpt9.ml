
let arr = [|1;2|]

module type X_int = sig
  val x : int
end

module Increment (B : X_int) : X_int = struct
  let x = B.x + 1
end

module Increment_include (B: X_int) = struct
  include B (* does not have B.more ; sad *)
  let x = B.x + 1
end

module Three : X_int = struct
  let x = 3
end

module Three_and_more = struct
  let x = 3
  let more = "more"
end

module Four = Increment(Three)

module Four_m = Increment(Three_and_more) (* no member more *)
module Four_mi = Increment_include(Three_and_more) (* no member more *)
;;

print_int (Four_mi.x);

;;


module type Compareable = sig
  type t
  val compare : t -> t -> int
end

module Interval (C:Compareable) = struct
  type t = Interval of C.t * C.t
         | Empty


  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t p =
    match t with
    | Empty -> false
    | Interval (l,r) ->
      C.compare l p <= 0 && C.compare p r <= 0

  let intersect_gcd t1 t2 =
    let min x y = if C.compare x y < 0 then x else y in
    let max x y = if C.compare x y > 0 then x else y in
    match t1,t2 with
    | _,Empty -> t1
    | Empty,_ -> t2
    | Interval (l1,r1), Interval (l2,r2) ->
      Interval (min l1 l2, max r1 r2)

end

module Interval_Int32 = Interval(struct
    type t = int32
    let compare = Int32.compare
  end)
;;

(*if Interval_Int32.is_empty Interval_Int32.Empty then
  print_int 1
else
  print_int 0
*)

;;

module type Point2d = sig
  type v
  type t
  val getx : t -> v
  val gety : t -> v
end


(* this type has type t *)
module type Point2d_float =
  Point2d with type v = float

(* this type does not have type t *)
module type Point2d_int_without_t =
  Point2d with type v := int

module Point2d_float_m : Point2d_float = struct
  type v = float
  type t = float * float
  let getx (x, _) = x
  let gety (_, y) = y
end


module With_d = struct
  module type Base = sig
    type t
    type v
    val getx : t -> v
  end

  module type Ext = sig
    type p
    type v
    val getd : p -> v
  end
(*:(Ext with type p := B.t with type v := B.v) *)
  module Ext_m (B : Base)
    : (Ext with type p := B.t with type v := B.v) =
  struct
    let getd (v:B.t) = B.getx v
  end

end
;;
module type Point2d_with_d = sig
  type v
  type t
  include Point2d with type v := v with type t := t
  include With_d.Ext with type p := t with type v := v
end

module Point2d_float_with_d : Point2d_with_d = struct
  include Point2d_float_m
  include With_d.Ext_m(Point2d_float_m)
end






















