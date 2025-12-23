open Raylib
open Card_game.Types
open Card_game.Enemies
open Card_game.Players


type card_selection = 
    | Selection of card
    | NoSelection

type game_state = {
    game : game;
    selected : card_selection
}

type interaction = 
    | ClickCard of card

type event = 
    | SelectCard of card

type hitbox = {
    x : int;
    y : int;
    w : int;
    h : int
}
type ui_element = 
    | CardUI of {card : card; box : hitbox}

type draw_cmd = 
    | DrawCard of {x : int; y : int; w : int; h : int; sel : bool; cost : int}

type ui_layout = {
    draw_cmds : draw_cmd list;
    ui_elements : ui_element list
}

(* constants for the dimensions of cards *)
let card_w = 120
let card_h = 180
let hand_x_spacing i = 200 + i * 140
let hand_y = 500


let point_in_box mx my (box : hitbox) = 
    mx >= box.x &&
    mx <= box.x + box.w &&
    my >= box.y &&
    my <= box.y + box.h

let collect_interactions (layout : ui_layout) = 
    let mouse = (get_mouse_x (), get_mouse_y ()) in 
    let pressed = is_mouse_button_pressed MouseButton.Left in
    let interactions = ref [] in
    let check_element element = 
        match element with
            | CardUI c ->
                if point_in_box (fst mouse) (snd mouse) c.box then
                    if pressed then
                        interactions := ClickCard c.card :: !interactions
    in
    List.iter check_element layout.ui_elements;
    !interactions

let interpret_interactions interactions (* game *) = 
    let interpret acc interaction (* game *) = 
        match interaction with
            | ClickCard c -> 
                SelectCard c :: acc
    in
    List.fold_left interpret [] interactions 

let event_handler events game_state = 
    let handle state event =
        match event with
            | SelectCard c ->
                {state with selected = Selection c}
    in
    List.fold_left handle game_state events

let gen_layout game_state = 
    let draw_cmds = ref [] in
    let ui_elements = ref [] in
    let game = game_state.game in
    let selected = game_state.selected in
    let gen_card_cmd i card =
        let x = hand_x_spacing i in
        let y = hand_y in
        let card_ui = CardUI {card = card; box = {x=x; y=y; w=card_w; h=card_h}} in
        ui_elements := !ui_elements @ [card_ui];
        match selected with
            | Selection c when c.id = card.id ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = true; cost = card.cost}]
            | _ ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = false; cost = card.cost}]
    in
    (* draw each card in hand *)
    List.iteri gen_card_cmd game.player.hand;
    {draw_cmds = !draw_cmds; ui_elements = !ui_elements}
    
let draw_card x y w h sel cost =
    (if sel then
        draw_rectangle x y w h Color.green
    else
        draw_rectangle x y w h Color.lightgray);
    draw_rectangle_lines x y w h Color.darkgray;
    draw_text (string_of_int cost) (x + 100) (y + 10) 20 Color.blue

let draw_layout layout = 
    let draw_cmd cmd = 
        match cmd with
            | DrawCard c ->
                draw_card c.x c.y c.w c.h c.sel c.cost
    in
    List.iter draw_cmd layout.draw_cmds

let () =
    (* initialize game starting state *)
    Random.self_init ();
    let enemies_simple = IntMap.add 0 enemy_simple IntMap.empty in
    let game_simple = {player = player_simple; enemies = enemies_simple} in
    let game_state = {game = game_simple; selected = NoSelection} in
    let layout = gen_layout game_state in

    (* initialize window *)
    init_window 1280 720 "OCaml Card Game";
    set_target_fps 60;

    let rec loop (layout : ui_layout) game_state =
        if window_should_close () then layout
        else begin
            (* calculate new layout and game state *)
            let interactions = collect_interactions layout in
            let events = interpret_interactions interactions in
            let new_game_state = event_handler events game_state in
            let new_layout = gen_layout game_state in

            begin_drawing ();
            clear_background Color.raywhite;

            (* draw new layout *)
            draw_layout new_layout;

            end_drawing ();

            loop new_layout new_game_state
        end
        in

        ignore (loop layout game_state);
        close_window ()
