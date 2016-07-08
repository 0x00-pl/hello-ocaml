
type pos = {
  x: int;
  y: int;
}


let pos_delta (p:pos) (d:pos) :pos =
  let x = p.x + d.x in
  let y = p.y + d.y in
  {x; y=y}

let pos_setx p x =
  {p with x=x}

type mut_pos = {
  mutable x: float;
  mutable y: float;
}



let mpos_delta (p:mut_pos) (d:mut_pos) =
  p.x <- (p.x +. d.x);
  p.y <- (p.y +. d.y);
  p


module Mpos = struct
  type t = {
    x: float;
    y: float;
  }
end








