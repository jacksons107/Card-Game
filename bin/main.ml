open Card_game.Types
open Card_game.Game
open Card_game.Enemies
open Card_game.Players

let enemies_simple = IntMap.add 0 enemy_simple IntMap.empty
let game_simple = {player = player_simple; enemies = enemies_simple}
let () = 
    Random.self_init ();
    game_loop game_simple
