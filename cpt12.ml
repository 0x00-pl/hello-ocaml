
class cstack = object
  val v = [0]

  method pop =
    match v with
    | [] -> None, {<>}
    | h::t -> Some h, {< v = t >}

  method push h =
    {< v = h::v >}

end


let vstack = object
  val v = [0]

  method pop =
    match v with
    | [] -> None, {<>}
    | h::t -> Some h, {< v = t >}

  method push h =
    {< v = h::v >}

end

let cs1 = new cstack#push 0
let vs1 = vstack#push 0

class ['a] cistack init = object
  val mutable v : 'a list = init

  method pop : 'a option =
    match v with
    | [] -> None
    | h::t -> v <- t; Some h

  method push h =
    v <- h::v
end

type 'a iter = < get: 'a; has_next: bool; next: unit>

exception ValueError

class ['a] list_iter init = object
  val mutable current: 'a list = init
  method get =
    match current with
    | [] -> raise ValueError
    | h::t -> h

  method has_next = current <> []

  method next =
    match current with
    | [] -> raise ValueError
    | h::t -> current <- t

end


let vstack_with_iter = object
  val v = [0]

  method pop =
    match v with
    | [] -> None, {<>}
    | h::t -> Some h, {< v = t >}

  method push h =
    {< v = h::v >}

  method iter =
    new list_iter v

  method fold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
    (fun f init -> List.fold_left f init v)

end

class double_stack init = object
  inherit [int] cistack init as super

  method push hd =
    super#push (hd*2)

end

class incinc = object(self)
  method private inc1 x = x + 1

  method inc2 (x:int) : int = x |> self#inc1 |> self#inc1
end







