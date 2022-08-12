(* Execute the game engine *)
let _ = 
  Random.self_init ();
  Game.Driver.run_game ()