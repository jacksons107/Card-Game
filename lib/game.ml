open Core.Types
open Ui.Ui_types
open Ui.Ui_logic
open Ui.Draw
open Game_logic.Rules
open Raylib

let init_enemies (enemies : enemy list) = 
    let enemy_map = ref IntMap.empty in
    (List.iteri
        (fun i e -> enemy_map := IntMap.add i e !enemy_map)
        enemies);
    !enemy_map

let run_battle (player : player) (enemies : enemy list) = 
    (* initialize game starting state *)
    Random.self_init ();
    let enemy_map = init_enemies enemies in
    let game_simple = {player = player; enemies = enemy_map; end_state = Ongoing} in
    let og_game_state = {game = pre_turn_processing game_simple; selected = NoSelection} in
    let og_layout = gen_layout og_game_state in

    (* initialize window *)
    init_window 1280 720 "OCaml Card Game";
    set_target_fps 60;

    let rec loop (layout : ui_layout) game_state =
        if window_should_close () then layout
        else begin
            (* calculate new layout and game state *)
            let interactions = collect_interactions layout in
            let events = interpret_interactions interactions game_state in
            let new_game_state = event_handler events game_state in
            let new_layout = gen_layout new_game_state in

            begin_drawing ();
            clear_background Color.raywhite;

            (* draw new layout *)
            draw_layout new_layout;

            end_drawing ();

            loop new_layout new_game_state
        end
    in

    ignore (loop og_layout og_game_state);
    close_window ()