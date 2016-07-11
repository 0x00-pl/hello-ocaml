

type 'a dlist_node = {
  mutable data : 'a;
  mutable prev : 'a dlist_node;
  mutable next : 'a dlist_node;
}

type 'a dlist = 'a dlist_node option ref

let create () = ref None

let create_node v =
  let rec e = {data = v; prev = e; next = e} in
  e

let is_empty l = !l = None

let first l = !l

let next e = if e.next = e then None else Some e.next

let prev e = if e.prev = e then None else Some e.prev

let value e = e.data

let iter f (l: 'a dlist) =
  let rec iter_e oe =
    match oe with
    | None -> ()
    | Some e -> f e.data; iter_e (next e) in
  let e = first l in
  iter_e e

let find_el f (l: 'a dlist) =
  let rec first_of oe =
    match oe with
    | None -> None
    | Some e -> if f e.data then oe else first_of (next e) in
  let e = first l in
  first_of e

let insert_first l v =
  match !l with
  | None ->
    l := Some (create_node v)
  | Some le ->
    let rec e = {data = v; prev = e; next = le} in
    le.prev <- e;
    l := Some e

let insert_after e v =
  let ne = create_node v in
  match next e with
  | None ->
    ne.prev <- e;
    e.next <- ne
  | Some nne ->
    ne.prev <- e;
    ne.next <- nne;
    e.next <- ne;
    nne.prev <- ne

let remove l e =
  match prev e with
  | None ->
    (match next e with
     | None -> l := None
     | Some ne -> ne.prev <- ne; l := Some ne)
  | Some pe ->
    match next e with
    | None -> pe.next <- pe
    | Some ne -> pe.next <- ne; ne.prev <- pe




