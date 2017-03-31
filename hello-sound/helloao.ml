
let open_f name = Ao.open_file name

let play_f a =
  let buf = Bytes.create 41029 in
  Ao.play a buf

let _ = open_f "340.wav" |> play_f
