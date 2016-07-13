
module type x_int = sig
  val x : int
end

module Three = struct
  let x = 3
end

(* module_to_value *)
let four = (module struct let x = 4 end : x_int)

(* value_to_module *)
module Four = (val four)

module type handler = sig
  type a
  type b
  val f : a -> b
end

let moudle_map (type a) (type b) (module M: handler with type a = a and type b = b ) l  =
  List.map M.f l

module Handler_inc : handler with type a = int and type b = int = struct
  type a = int
  type b = int
  let f x = x + 1
end

module Handler_float = struct
  type a = float
  type b = float
  let f x = x +. 1.
end

module Handler_base (A : sig type t end) (B : sig type t end) = struct
  type a = A.t
  type b = B.t
end

module Handler_string = struct
  include Handler_base(String)(String)
  let f x = x ^ "s"
end

;;

let m = (module Handler_inc : handler)

let arr_inc_i = moudle_map (module Handler_inc) [1;2;3]
let arr_inc_f = moudle_map (module Handler_float) [1.;2.;3.]
let arr_inc_s = moudle_map (module Handler_string) ["";"s";"ss"]



