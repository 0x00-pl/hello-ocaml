
module type ty = sig type t end

module Int_ty:ty with type t = int = struct
  type t = int
end

module ExampleAndTest = struct

  module type b = sig
    type d
    type a
    type t = d -> a
  end

  module C(A:ty)(B:ty):ty with type t = A.t->B.t = struct
    type t = A.t->B.t
  end
  module I:ty with type t = int = struct
    type t = int
  end
  module II = C(I)(I)
  let f2:II.t = fun x->x
  module III = C(I)(II)
  let f3:III.t = fun x y->x

  module type bii = b with type a:=int with type d:=int

  type ('h, 't) tt = 'h->'t

  type tii = (int, int) tt

  type tiii = (tii, int) tt
end

module type mapp = sig
  type a
  type b
  type 'a v
  val app : (a->b) v -> a v -> b v
end
module type ms = sig
  type a
  type b
  type c
  type 'a v
  val s : ((a->b->c)->(a->b)->a->c) v
end
module type mk = sig
  type a
  type b
  type 'a v
  val k : (a->b->a) v
end
module type mi = sig
  type a
  type 'a v
  val i : (a->a) v
end

module type ast = sig
  type 'a v
  (*
  val app : ('a->'b) v -> 'a v -> 'b v
  val s : (('x->'y->'z)->('x->'y)->'x->'z) v
  val k : ('a->'b->'a) v*)

  module APP(A:ty)(B:ty) : mapp
    with type 'a v := 'a v
    with type a := A.t
    with type b := B.t
  module S(A:ty)(B:ty)(C:ty) : ms
    with type 'a v := 'a v
    with type a := A.t
    with type b := B.t
    with type c := C.t
  module K(A:ty)(B:ty) : mk
    with type 'a v := 'a v
    with type a := A.t
    with type b := B.t
  module I(A:ty) : mi
    with type 'a v := 'a v
    with type a := A.t
end

module ShowAst:ast with type 'a v = string = struct
  type 'a v = string
  module APP(A:ty)(B:ty):mapp with type 'a v := 'a v = struct
    type a = A.t
    type b = B.t
    let app f x = "(" ^ f ^ " " ^ x ^ ")"
  end
  module S(A:ty)(B:ty)(C:ty):ms with type 'a v := 'a v = struct
    type a = ()
    type b = ()
    type c = ()
    let s = "S"
  end
  module K(A:ty)(B:ty):mk with type 'a v := 'a v = struct
    type a = () type b = ()
    let k = "K"
  end
  module I(A:ty):mi with type 'a v := 'a v = struct
    type a = A.t
    let i = "I"
  end
end

module NackedAst:ast with type 'a v = 'a = struct
  type 'a v = 'a
  module APP(A:ty)(B:ty) = struct
    let app f x = f x
  end
  module S(A:ty)(B:ty)(C:ty) = struct
    let s = (fun x y a -> x a (y a))
  end
  module K(A:ty)(B:ty) = struct
    let k = (fun x y -> x)
  end
  module I(A:ty) = struct
    let i = (fun x -> x)
  end
end


module AST(BB:ast)(T:ty):ast with type 'a v = (T.t->'a) BB.v = struct
  type 'a v = (T.t->'a) BB.v

  module APP(A:ty)(B:ty) = struct
    module App_tab_tatb = BB.APP
        (struct type t = T.t->(A.t->B.t) end)
        (struct type t = (T.t->A.t)->(T.t->B.t) end)
    module App_ta_tb = BB.APP
        (struct type t = T.t->A.t end)
        (struct type t = T.t->B.t end)
    module STAB = BB.S(T)(A)(B)
(*
app (app s f) x
           : (e->a->b)v
              : (e->a)v
         : ((e->a->b) -> (e->a)->e->b)v
     ::: ((e->a->b) -> (e->a)->e->b)v -> (e->a->b)v -> ((e->a)->e->b)v
    ::::::::: ((e->a) -> e->b)v
::: ((e->a) -> e->b)v -> (e->a)v -> (e->b)v
*)
    let app (f:(A.t->B.t) v) (x:A.t v) =
      let inner = App_tab_tatb.app STAB.s f in
      App_ta_tb.app inner x
  end

  module S(A:ty)(B:ty)(C:ty) = struct
    module SABC = BB.S(A)(B)(C)
    module KABCABAC = BB.K
        (struct type t = (A.t->B.t->C.t)->(A.t->B.t)->(A.t->C.t) end)
        (T)
    module APPXX = BB.APP
        (struct type t = (A.t->B.t->C.t)->(A.t->B.t)->(A.t->C.t) end)
        (struct type t = T.t->(A.t->B.t->C.t)->(A.t->B.t)->(A.t->C.t) end)
        (*
app k s
      s: ((a->b->c)->(a->b)->(a->c))v
    k: (((a->b->c)->(a->b)->(a->c))->e->((a->b->c)->(a->b)->(a->c)))v
app: xxx -> yyy -> (e->((a->b->c)->(a->b)->(a->c)))v
*)
    let s = APPXX.app KABCABAC.k SABC.s
  end

  module K(A:ty)(B:ty) = struct
    module KAB = BB.K(A)(B)
    module KABAT = BB.K
        (struct type t = A.t->B.t->A.t end)
        (T)
    module APPXX = BB.APP
        (struct type t = A.t->B.t->A.t end)
        (struct type t = T.t->A.t->B.t->A.t end)
        (*
app k k
      k: (a->b->a)v
    k: ((a->b->a)->e->(a->b->a))v
app: ((a->b->a)->e->(a->b->a))v -> (a->b->a)v -> (e->(a->b->a))v
*)
    let k = APPXX.app KABAT.k KAB.k
  end

  module I(A:ty) = struct
    module APPAATAA = BB.APP(struct type t = A.t->A.t end)
        (struct type t = T.t->A.t->A.t end)
    module KAAT = BB.K(struct type t = A.t->A.t end)(T)
    module IA = BB.I(A)
    let i = APPAATAA.app KAAT.k IA.i
  end
end

module Unit = struct type t = unit end

module IMS = ShowAst.S(Unit)(Unit)(Unit)
module IMK = ShowAst.K(Unit)(Unit)
module IMI = ShowAst.I(Unit)
;;
print_endline IMI.i
;;
print_endline IMS.s
;;

module M1(F:ast) = struct
  module F_APP = F.APP(Int_ty)(Int_ty)
  module F_I = F.I(Int_ty)
  let lam f = F_APP.app F_I.i
  let idk = lam (fun x -> F_APP.app F_I.i x)
end

module Ms = M1(ShowAst)
let id_string = "id"
let mss = Ms.idk "99"

module Mn = M1(NackedAst)
let msn = Mn.idk 1
;;
print_endline (mss);
print_endline (string_of_int msn)
;;

module MC(F:ast) = struct

  module I_to_I = struct type t = int->int end
  module I_to_I_to_I = struct type t = int->int->int end
  module I_to_I_to_I_to_I = struct type t = int->int->int->int end
  module II_to_II = struct type t = (int->int)->int->int end
  module II_to_III = struct type t = (int->int)->int->int->int end
  module II_to_II_to_II = struct type t = (int->int)->(int->int)->int->int end
  module F_APP3 = F.APP(I_to_I)(II_to_II)
  module F_APP2 = F.APP(I_to_I)(I_to_I)
  module F_APP1 = F.APP(Int_ty)(Int_ty)
  module F_K = F.K(Int_ty)(Int_ty)
  module F_K_II = F.K(I_to_I)(I_to_I)
  module F_K_III = F.K(I_to_I_to_I)(I_to_I)
  module F_I = F.I(Int_ty)
  module F_I_II = F.I(I_to_I)
  module AST_F = AST(F)(I_to_I)
  module AST_FF = AST(AST_F)(Int_ty)
  module AST_F_K = AST_F.K(Int_ty)(Int_ty)
  module AST_FF_APP = AST_FF.APP(Int_ty)(Int_ty)
  module AST_FF_K = AST_FF.K(Int_ty)(Int_ty)
  module AST_F_APP1 = AST_F.APP(Int_ty)(Int_ty)
  module AST_FF_APP1 = AST_FF.APP(Int_ty)(Int_ty)
  module AST_FF_APP2 = AST_FF.APP(Int_ty)(I_to_I)

  let k_ab ab = AST_FF_APP2.app AST_FF_K.k ab  (* (ii->iii) -> ii -> iii *)
  let k_a a = F_APP3.app F_K_II.k a
  let lam1 f = f F_I.i
  let lam2 f = f F_I_II.i
  let aa = lam2(fun ab-> lam1(fun a-> AST_FF_APP.app (k_ab ab) (k_a a)))

(*
   app f x
   app: (ii->iii) -> (ii->ii) -> (ii->ii)
*)
end

module MCs = MC(ShowAst)
let mcns =  MCs.F_APP1.app (MCs.F_APP2.app MCs.aa "inc") "1"

module MCn = MC(NackedAst)
let mcnn = MCn.F_APP1.app (MCn.F_APP2.app MCn.aa (fun x->x+1)) 1

;;
print_endline "mcnn: ";
print_endline (mcns);
print_endline (string_of_int mcnn)
