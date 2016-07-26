
type value = [
  | `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string
]


let rec string_of_value = function
  | `Assoc l -> l
    |> List.map (fun (s, v) -> s ^ ": " ^ (string_of_value v))
    |> String.concat ", "
    |> (fun a -> "{" ^ a ^ "}")
  | `Bool b -> string_of_bool b
  | `Float f -> string_of_float f
  | `Int i -> string_of_int i
  | `List l -> l
               |> List.map string_of_value
               |> String.concat ", "
               |> (fun a -> "[" ^ a ^ "]" )

  | `Null -> "null"
  | `String s -> s

let output_value out v =
  output_string out (string_of_value v)

