

type ('a, 'b) t = {
  mutable length: int;
  buckets: ('a * 'b) list array;
}

let num_backets = 17

let hash_bucket key = (Hashtbl.hash key) mod num_backets

let create () = {
  length = 0;
  buckets = Array.make num_backets []
}

let length d = d.length

let rec list_add l ((k, v) as kv) =
  match l with
  | [] -> [kv]
  | ((hk, hv)as h)::t -> if hk = k then kv::t else h :: list_add t kv

let add d k v =
  let h = hash_bucket k in
  let l = list_add d.buckets.(h) (k, v) in
  d.buckets.(h) <- l; d.length <- d.length + 1

let rec list_find_option l k =
  match l with
  | [] -> None
  | (hk, hv)::t -> if hk = k then Some hv else list_find_option t k

let find d k =
  let h = hash_bucket k in
  let l = d.buckets.(h) in
  list_find_option l k

let iter d f =
  let iter_list l = List.iter (fun (k,v) -> f k v) l in
  Array.iter iter_list d.buckets

let remove d k =
  let h = hash_bucket k in
  let l = List.filter (fun (ik,_) -> k<>ik) d.buckets.(h) in
  d.buckets.(h) <- l; d.length <- d.length - 1



