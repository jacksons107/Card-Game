(* open Card_game.Types
open Card_game.Game
open Card_game.Enemies
open Card_game.Players

let enemies_simple = IntMap.add 0 enemy_simple IntMap.empty
let game_simple = {player = player_simple; enemies = enemies_simple}
let () = 
    Random.self_init ();
    game_loop game_simple *)

open Raylib

let () =
  init_window 800 600 "Card Game";
  set_target_fps 60;

  while not (window_should_close ()) do
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "Hello, raylib + OCaml!" 190 200 20 Color.darkgray;
    end_drawing ()
  done;

  close_window ()
