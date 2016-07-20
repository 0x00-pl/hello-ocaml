

(* 定义携带类型的模块类型 *)
module type ty = sig type t end

(* ocaml里对于高阶依赖类型有限制 所以用模块这个黑科技绕一下, 不过有个坏处就是需要手动推导类型.
   这里定义几个模块类型来为函数引入类型,后面会用到. *)
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

(* 这里定义一个show模块用来打印程序 *)
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

(* 这里定义一个裸模块来执行函数调用 *)
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



(* 这里定义一个高阶模块: Ast(Base),用来实现把一个类型转成携带参数的类型 比如int 转成'a->int然后实现这些携带新参数的app,s,k,i *)

(* 这里解释一些为什么除了app 还需要ski 函数 *)
(* 首先 app是必须的, 实现AST.app 需要用一个
   ('a->'b) v -> 'a v -> 'b v
   类型的Base.app实现一个
   ('t->'a->'b) v -> ('t->'a) v -> ('t->'b) v
   类型的函数.
   简单的说就是给普通app的f和x携带一个新参数n变成 (f n) 和 (x n)
   这里正好可以使用s组合子把 (s f x n) 变成 ((f n)(x n)).
   下面推导一下这个类型, 假设f的类型是(t->a->b)v, x的类型是(t->a)v:
   Ast.app f x = Base.app (Base.app Base.s f) x
                                           f: (t->a->b)v
                                    Base.s: ((t->a->b)->(t->a)->(t->b))v
                           Base.app: ('a->'b)v -> 'a v -> 'b v
                           Base.app: ((t->a->b) -> ((t->a)->t->b))v -> (t->a->b)v -> ((t->a)->t->b)v
                          (^^^^^^^^^^^^^^^^^): ((t->a)->t->b)v
                                              x: (t->a)v
                 Base.app: ('a->'b)v -> 'a v -> 'b v
                 Base.app: ((t->a)->(t->b)v -> (t->a)v -> (t->b)v
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^: (t->b)v
   Ast.app: (t->a->b)v -> (t->a)v -> (t->b)v
   如果排版没乱的话希望大家能看懂.
   上面使用了Base.s 那么Ast也必须实现Ast.s
   也就是用
   (('a->'b->'c) -> ('a->'b) -> ('a->'c))v
   实现
   ('t->('a->'b->'c) -> ('a->'b) -> ('a->'c))v
   同样的,在每个类型前面携带一个参数.
   下面推导一下类型, 假设f
   Ast.s = Base.app Base.k Base.s
                           Base.s: ((a->b->c) -> (a->b) -> (a->c))v
                    Base.k: ('a->'b->'a)v
                    Base.k: ((a->b->c)->(a->b)->(a->c)) -> t -> ((a->b->c)->(a->b)->(a->c))v
           Base.app: (((a->b->c)->(a->b)->(a->c)) -> t -> ((a->b->c)->(a->b)->(a->c)))v -> ((a->b->c)->(a->b)->(a->c))v -> (t->(a->b->c)->(a->b)->(a->c))v
   Ast.s: (t->(a->b->c)->(a->b)->(a->c))v

   k和i的构造也是类似的方式.
*)


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


(* 下面是原帖testcase的部分,不过不用太仔细看,基本上都是手工在做类型推导 *)

module MC3(F:ast) = struct
  module I = struct type t = int end
  module II = struct type t = int->int end
  module III = struct type t = int->int->int end
  module IIII = struct type t = int->int->int->int end
  module II_II = struct type t = (int->int)->int->int end
  module II_III = struct type t = (int->int)->int->int->int end
  module III_III = struct type t = (int->int->int)->int->int->int end
  module II_II_II = struct type t = (int->int)->(int->int)->int->int end
  module SF = AST(F)(III)
  module SSF = AST(SF)(I)
  module SSSF = AST(SSF)(I)
  module APP_I_I = F.APP(I)(I)
  module APP_I_II = F.APP(I)(II)
  module APP_III_III = F.APP(III)(III)
  module SAPP_III_IIII = SF.APP(III)(IIII)
  module SSAPP_I_II = SSF.APP(I)(II)
  module SSAPP_III_IIII = SSF.APP(III)(IIII)
  module SSSAPP_I_I = SSSF.APP(I)(I)
  module SSSAPP_I_II = SSSF.APP(I)(II)
  module SK_III_I = SF.K(III)(I)
  module SSK_I_I = SSF.K(I)(I)
  module SSK_III_I = SSF.K(III)(I)
  module I_III = F.I(III)
  module SI_I = SF.I(I)
  module SSI_I = SSF.I(I)

  let id x = x
  let lam1 f = f I_III.i
  let lam2 f = f SI_I.i
  let lam3 f = f SSI_I.i
  let k_abc_1 (abc:III.t SF.v) : III.t SSF.v =
    SAPP_III_IIII.app SK_III_I.k abc
  let k_abc (abc:III.t SF.v) : III.t SSSF.v =
    SSAPP_III_IIII.app SSK_III_I.k (k_abc_1 abc)
  let k_a a = a
  let k_b (b:I.t SSF.v): I.t SSSF.v =
    SSAPP_I_II.app SSK_I_I.k b

  (* 这里就是原帖中的app表达式 *)
  let cc = lam1(fun abc-> lam2(fun b-> lam3(fun a->
      SSSAPP_I_I.app (SSSAPP_I_II.app (k_abc abc) (a)) (k_b b))))
end

(* 这里用ShowAst的方式来运行上面的例子 *)
module MC3s = MC3(ShowAst)
let mc3s = MC3s.APP_I_I.app (MC3s.APP_I_II.app (MC3s.APP_III_III.app
                                            MC3s.cc "-") "1") "2"
(* 这里直接执行上面的例子 *)
module MC3n = MC3(NackedAst)
let minus x y = x - y
let mc3n = MC3n.APP_I_I.app (MC3n.APP_I_II.app (MC3n.APP_III_III.app
                                            MC3n.cc minus) 1) 2

;;
print_newline ();
print_newline ();
(* 输出结果 *)
print_endline ("mc3s: " ^ (mc3s));
print_endline ("mc3n: " ^ (string_of_int mc3n))
;;

