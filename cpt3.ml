
let list1 = [1;2;3]

let list2 = (1::(2::(3::[])))

let rec sum = function
  | [] -> 0
  | h::t -> h + sum t


let rec drop_value v = function
  | [] -> []
  | a::t ->
    if a=v
    then drop_value v t
    else a :: (drop_value v t)


let print_table h t =
  let print_sep n =
    let data = List.map (fun x -> String.make x '-') n in
    let inner = String.concat "+" data in
    String.concat inner ["+"; "+"] in
  let counting head data =
    let m2f n s = n + String.length s in
    let hl = List.map String.length head in
    List.fold_left (List.map2 m2f) hl data in
  let padding m s =
    s ^ String.make (m - (String.length s)) ' ' in
  let print_data_line counts data_line =
    let data_line_p = List.map2 padding counts data_line in
    let inner = String.concat " | " data_line_p in
    String.concat inner ["| "; " |"] in

  let countingt = counting h t in
  let counting2 = List.map ((+) 2) countingt in
  let sep = print_sep counting2 in
  let lines = List.concat [
      [sep; print_data_line countingt h; sep];
      List.map (print_data_line countingt) t;
      [sep]
    ] in
  let result = String.concat "\n" lines in
  print_endline result

;;

print_table ["a"; "bbb"] [["111";"2"];["3";"4"]]
;;

let string_ab = "a" ^ "b"

let list_12 = [1] @ [2]

;;
let s1 = string_of_bool (Sys.file_exists ".")

let s2 = "." |> Sys.file_exists |> string_of_bool
;;


let rec ls_rec path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      Sys.readdir path |> Array.to_list
      |> List.map (fun r -> ls_rec (path ^ "/" ^ r))
      |> List.concat
    else
      [path]
  else []
;;

ls_rec ".git/hooks" |> String.concat "\n" |> print_endline


let rec destutter = function
  | [] | [_] as l -> l
  | h::(h'::t' as t) ->
    if h=h'
    then h' :: destutter t'
    else h :: destutter t

