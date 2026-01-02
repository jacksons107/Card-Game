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

let init_card (card_temp : card_template) (game : game) =
    let id, new_game = Game_logic.Actions.fresh_card_id game in
    let card = {id = id; cost = card_temp.cost; action_type = card_temp.action_type} in
    (card, new_game)
    
(* turn list of card templates into list of cards, then set that as player's deck and return new game *)
let init_deck templates game =
    let deck, new_game = 
        List.fold_left
            (fun (cards, game) tpl ->
            let (card, new_game) = init_card tpl game in
            (card :: cards, new_game))
            ([], game)
            templates
    in
    (* toggle on for random deck *)
    let shuffled_deck = shuffle_deck deck in
    let new_player = {new_game.player with deck = shuffled_deck} in
    (* toggle on for deterministic deck *)
    (* let new_player = {new_game.player with deck = List.rev deck} in *)
    {new_game with player = new_player}

(* draw num cards from deck to create initial hand *)
let init_hand num game = 
    draw_cards num game

let run_battle (player : player) (deck_tpl : card_template list) (enemies : enemy list) = 
    (* initialize game starting state *)
    Random.self_init ();
    let enemy_map = init_enemies enemies in
    let game_simple = {player = player; enemies = enemy_map; end_state = Ongoing; 
                       timeline = [||]; time_idx = 0; next_card_id = 0} in
    (* let deck_game = init_deck game_simple deck_tpl in
    let hand_game = init_hand deck_game in *)
    let inited_game = 
        game_simple |> (init_deck deck_tpl) |> (init_hand 2)
    in
    let og_game_state = {game = pre_turn_processing inited_game; selected = NoSelection} in
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