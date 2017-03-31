
let playSound wav_file =
  (* Load a sound buffer from a wav file *)
  let buffer = SFSoundBuffer.loadFromFile wav_file in

  (* Create a sound instance and play it *)
  let sound = SFSound.create () in
  SFSound.setBuffer sound buffer;
  SFSound.play sound;

  (* Loop while the sound is playing *)
  while (SFSound.getStatus sound) = SFSound.Playing
  do
    (* Leave some CPU time for other processes *)
    SFTime.sleep (SFTime.of_seconds 0.1);

    (* Display the playing position *)
    Printf.printf "\rPlaying... %8g sec   %!"
      (s (SFSound.getPlayingOffset sound));
  done;
  print_newline ()

