
let play_sound path =
  let cmd = "aplay " ^ path in
  Sys.command cmd


let _ =
  play_sound "340.wav"
