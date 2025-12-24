open Raylib
open Card_game.Types
open Card_game.Enemies
open Card_game.Players
open Card_game.Rules
open Card_game.Render


type card_selection = 
    | Selection of card
    | NoSelection

type game_state = {
    game : game;
    selected : card_selection
}

type interaction = 
    | ClickCard of card
    | ClickPlayer
    | ClickEnemy of int

type event = 
    | SelectCard of card
    | TargetPlayer of card
    | TargetEnemy of int * card

type hitbox = {
    x : int;
    y : int;
    w : int;
    h : int
}
type ui_element = 
    | CardUI of {card : card; box : hitbox}
    | PlayerUI of {player : player; box : hitbox}
    | EnemyUI of {id : int; box : hitbox}

type draw_cmd = 
    | DrawCard of {x : int; y : int; w : int; h : int; sel : bool; cost : int; act : action_type}
    | DrawPlayer of {x : int; y : int; w : int; h : int; hp : int; block : int}
    | DrawEnemy of {x : int; y : int; w : int; h : int; hp : int; block : int; act : action_type}

type ui_layout = {
    draw_cmds : draw_cmd list;
    ui_elements : ui_element list
}

(* constants for the drawing cards *)
let card_w = 120
let card_h = 180
let hand_x_spacing i = 500 + i * 140
let hand_y = 500

(* constants for drawing player *)
let player_w = 120
let player_h = 120
let player_x = 100
let player_y = 250

(* constants for drawing enemies *)
let enemy_w = 120
let enemy_h = 120
let enemy_x = 1100
let enemy_y = 250

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
            | PlayerUI p ->
                if point_in_box (fst mouse) (snd mouse) p.box then
                    if pressed then
                        interactions := ClickPlayer :: !interactions
            | EnemyUI e ->
                if point_in_box (fst mouse) (snd mouse) e.box then
                    if pressed then
                        interactions := ClickEnemy e.id :: !interactions
    in
    List.iter check_element layout.ui_elements;
    !interactions

let interpret_interactions interactions game = 
    let interpret game acc interaction = 
        match interaction with
            | ClickCard c -> 
                SelectCard c :: acc
            | ClickPlayer ->
                (match game.selected with
                    | Selection c -> TargetPlayer c:: acc
                    | NoSelection -> acc)
            | ClickEnemy id ->
                (match game.selected with
                    | Selection c -> TargetEnemy (id, c) :: acc
                    | NoSelection -> acc)
    in
    List.fold_left (interpret game) [] interactions 

let event_handler events game_state = 
    let handle state event =
        match event with
            | SelectCard c ->
                {state with selected = Selection c}
            | TargetPlayer c ->
                {game = apply_card c Player state.game; selected = NoSelection }
            | TargetEnemy (id, c) ->
                {game = apply_card c (Enemy id) state.game; selected = NoSelection }
    in
    List.fold_left handle game_state events

let gen_layout game_state = 
    let draw_cmds = ref [] in
    let ui_elements = ref [] in
    let game = game_state.game in

    (* draw cards *)
    let selected = game_state.selected in
    let gen_card_cmd i card =
        let x = hand_x_spacing i in
        let y = hand_y in
        let card_ui = CardUI {card = card; box = {x=x; y=y; w=card_w; h=card_h}} in
        ui_elements := !ui_elements @ [card_ui];
        match selected with
            | Selection c when c.id = card.id ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = true; cost = card.cost; act = card.action_type}]
            | _ ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = false; cost = card.cost; act = card.action_type}]
    in
    (* draw each card in hand *)
    List.iteri gen_card_cmd game.player.hand;

    (* draw player *)
    let player = game_state.game.player in
    let player_cmd = DrawPlayer {x=player_x; y=player_y; w=player_w; h=player_h; hp=player.hp; block=player.block} in
    let player_ui = PlayerUI {player=player; box = {x=player_x; y=player_y; w=player_w; h=player_h}} in
    draw_cmds := !draw_cmds @ [player_cmd];
    ui_elements := !ui_elements @ [player_ui];

    (* draw enemies *)
    let gen_enemy_cmd id _ = 
        let enemy = IntMap.find id game.enemies in
        let enemy_cmd = DrawEnemy {x=enemy_x; y=enemy_y; w=enemy_w; h=enemy_h; hp=enemy.hp; block=enemy.block; act=enemy.selected_action} in
        let enemy_ui = EnemyUI {id=id; box = {x=enemy_x; y=enemy_y; w=enemy_w; h=enemy_h}} in
        draw_cmds := !draw_cmds @ [enemy_cmd];
        ui_elements := !ui_elements @ [enemy_ui]
    in
    (* draw each enemy *)
    IntMap.iter gen_enemy_cmd game.enemies;

    {draw_cmds = !draw_cmds; ui_elements = !ui_elements}
    
let draw_card x y w h sel cost act =
    (if sel then
        draw_rectangle x y w h Color.green
    else
        draw_rectangle x y w h Color.lightgray);
    draw_rectangle_lines x y w h Color.darkgray;
    draw_text (string_of_int cost) (x + 100) (y + 10) 20 Color.blue;
    let action_string = string_of_action_type act in
    draw_text action_string (x + 15) (y + h/2) 5 Color.black

let draw_player x y w h hp block = 
    draw_rectangle x y w h Color.blue;
    draw_text (string_of_int hp) (x + 10) (y + 10) 20 Color.white;
    draw_text (string_of_int block) (x + w - 20) (y + 10) 20 Color.white

let draw_enemy x y w h hp block act = 
    draw_rectangle x y w h Color.red;
    draw_text (string_of_int hp) (x + 10) (y + 10) 20 Color.black;
    draw_text (string_of_int block) (x + w - 20) (y + 10) 20 Color.black;
    let action_string = string_of_action_type act in
    draw_text action_string (x + 15) (y + h/2) 5 Color.black

let draw_layout layout = 
    let draw_cmd cmd = 
        match cmd with
            | DrawCard c ->
                draw_card c.x c.y c.w c.h c.sel c.cost c.act
            | DrawPlayer p ->
                draw_player p.x p.y p.w p.h p.hp p.block
            | DrawEnemy e ->
                draw_enemy e.x e.y e.w e.h e.hp e.block e.act
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
            let events = interpret_interactions interactions game_state in
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
