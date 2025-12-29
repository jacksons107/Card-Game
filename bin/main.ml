open Raylib
open Card_game.Types
open Card_game.Enemies
open Card_game.Players
open Card_game.Rules
open Card_game.Render


(* constants for the drawing cards *)
let card_w = 120
let card_h = 180
let hand_x = 300
let hand_x_spacing i = (hand_x + 50) + i * 140
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

(* constants for drawing end turn button *)
let end_turn_x = 1000
let end_turn_y = 600
let end_turn_w = 120
let end_turn_h = 50

(* constants for drawing mana bar *)
let mana_bar_x = 100
let mana_bar_y = 50
let mana_bar_w = 100
let mana_bar_h = 50

(* constants for drawing deck *)
let deck_x = 100
let deck_y = 550
let deck_w = 50
let deck_h = 50

(* constants for drawing end state screens *)
let end_message_x = 540
let end_message_y = 360

let point_in_box mx my (box : hitbox) = 
    mx >= box.x &&
    mx <= box.x + box.w &&
    my >= box.y &&
    my <= box.y + box.h

let collect_interactions (layout : ui_layout) = 
    let mouse_x, mouse_y = (get_mouse_x (), get_mouse_y ()) in 
    let pressed = is_mouse_button_pressed MouseButton.Left in
    let interactions = ref [] in
    let hit_something = ref false in
    let check_element element = 
        let hit box = 
            point_in_box mouse_x mouse_y box
        in
        match element with
            | CardUI c when hit c.box ->
                hit_something := true;
                if pressed then interactions := ClickCard c.card :: !interactions
            | PlayerUI p when hit p.box ->
                hit_something := true;
                if pressed then interactions := ClickPlayer :: !interactions
            | EnemyUI e when hit e.box ->
                hit_something := true;
                if pressed then interactions := ClickEnemy e.id :: !interactions
            | HandUI h when hit h.box ->
                hit_something := true;
                if pressed then interactions := ClickHand :: !interactions
            | DeckUI d when hit d.box ->
                hit_something := true;
                if pressed then interactions := ClickDeck :: !interactions
            | EndTurnUI b when hit b.box ->
                hit_something := true;
                if pressed then interactions := ClickEndTurn :: !interactions
            | _ -> ()

    in
    List.iter check_element layout.ui_elements;

    (* if click was not on any ui element *)
    if pressed && not !hit_something then 
        interactions := ClickNothing :: !interactions;

    !interactions

let interpret_interactions interactions game = 
    let interpret game acc interaction = 
        match interaction with
            | ClickCard c -> 
                (match game.selected with
                    | Selection ({action_type = Modifier _; _} as s) -> (* if selected card is a modifier interpret a card click as targeting *)
                        TargetCard (s, c) :: acc
                    | _ -> 
                        SelectCard c :: acc)
            | ClickPlayer ->
                (match game.selected with
                    | Selection c -> TargetPlayer c :: acc
                    | NoSelection -> acc)
            | ClickEnemy id ->
                (match game.selected with
                    | Selection c -> TargetEnemy (id, c) :: acc
                    | NoSelection -> acc)
            | ClickHand ->
                (match game.selected with
                    | Selection c -> TargetHand c :: acc
                    | NoSelection -> acc)
            | ClickDeck ->
                (match game.selected with
                    | Selection c -> TargetDeck c :: acc
                    | NoSelection -> acc)
            | ClickEndTurn ->
                EndTurn :: acc
            | ClickNothing ->
                (match game.selected with
                    | Selection _ -> Unselect :: acc
                    | NoSelection -> acc)
    in
    List.fold_left (interpret game) [] interactions 

let event_handler events game_state = 
    let handle state event =
        match event with
            | SelectCard c ->
                {state with selected = Selection c}
            | Unselect ->
                {state with selected = NoSelection}
            | TargetPlayer c ->
                {game = apply_card c Player state.game; selected = NoSelection}
            | TargetEnemy (id, c) ->
                {game = apply_card c (Enemy id) state.game; selected = NoSelection}
            | TargetCard (c, t) ->
                {game = apply_card c (Card t) state.game; selected = NoSelection} 
            | TargetHand c ->
                {game = apply_card c Hand state.game; selected = NoSelection}
            | TargetDeck c ->
                {game = apply_card c Deck state.game; selected = NoSelection}
            | EndTurn ->
                let new_game = pre_turn_processing (apply_enemy_actions state.game (get_enemy_actions state.game)) in
                {game = new_game; selected = NoSelection} 
    in
    (* Freeze game state if end state is reached *)
    match game_state.game.end_state with
        | Victory | Defeat ->
            game_state
        | Ongoing ->
            let new_state = List.fold_left handle game_state events in
            (* TODO maybe inefficient to do this processing if a card was not actually played *)
            (* remove dead enemies and check for end states *)
            {new_state with game = post_action_check new_state.game}

(* determine if an action type is a valid target based on a list of target kinds *)
let is_valid_target_kind ui_type target_kinds = 
    List.exists 
        (fun kind -> 
            match ui_type, kind with
                | EnemyUI _, TKEnemy -> true
                | PlayerUI _, TKPlayer -> true
                | CardUI _, TKCard -> true
                | HandUI _, TKHand -> true
                | DeckUI _, TKDeck -> true
                | _ -> false)
        target_kinds

let gen_layout game_state = 
    let draw_cmds = ref [] in
    let ui_elements = ref [] in
    let game = game_state.game in
    let selected = game_state.selected in
    let valid_targets = 
        match selected with
            | Selection c -> get_target_kinds c.action_type
            | NoSelection -> []
    in
    (* indicates if a card is selected *)
    let is_selection = 
        match selected with
            | Selection _ -> true
            | NoSelection -> false
    in
    (* TODO might be a better way to determine toggling of these variables *)
    let targeting_player = List.mem TKPlayer valid_targets in
    let targeting_enemy = List.mem TKEnemy valid_targets in
    let targeting_hand = List.mem TKHand valid_targets in
    let targeting_deck = List.mem TKDeck valid_targets in

    (* draw deck *)
    let deck_ui = DeckUI {box = {x=deck_x; y=deck_y; w=deck_w; h=deck_h}} in
    let deck_cmd = 
        match selected with
            | Selection _ when is_valid_target_kind deck_ui valid_targets ->
                DrawDeck {x=deck_x; y=deck_y; w=deck_w; h=deck_h; hil = true; size=List.length game.player.deck}
            | _ ->
                DrawDeck {x=deck_x; y=deck_y; w=deck_w; h=deck_h; hil = false; size=List.length game.player.deck}
    in
    draw_cmds := !draw_cmds @ [deck_cmd];
    if targeting_deck then ui_elements := !ui_elements @ [deck_ui];

    (* draw hand *)
    let hand_w =
        match List.length game.player.hand with
        | 0 -> 0
        | n ->
            let last_card_x = hand_x_spacing (n - 1) in
            (last_card_x + card_w + 50) - hand_x
    in
    (* TODO should have the box is these situations be a variable to keep the ui element and the draw command coupled *)
    let hand_ui = HandUI {box = {x=hand_x; y=hand_y - 25; w=hand_w; h=card_h + 50}} in
    let hand_cmd = 
        match selected with
            | Selection _ when is_valid_target_kind hand_ui valid_targets ->
                DrawHand {x=hand_x; y=hand_y - 25; w=hand_w; h=card_h + 50; hil=true}
            | _ ->
                DrawHand {x=hand_x; y=hand_y; w=hand_w; h=card_h + 50; hil=false}
    in
    draw_cmds := !draw_cmds @ [hand_cmd];
    (* only add the hand ui element if the selected card is targeting the hand *)
    if targeting_hand then ui_elements := !ui_elements @ [hand_ui];

    (* draw cards *)
    let gen_card_cmd i card =
        let x = hand_x_spacing i in
        let y = hand_y in
        let card_ui = CardUI {card = card; box = {x=x; y=y; w=card_w; h=card_h}} in
        (* TODO can probably refactor this logic better since selected is matched against twice *)
        let is_selected_card =
        match selected with
            | Selection c when c.id = card.id -> true
            | _ -> false
        in
        (* only add ui element if selected card is not targeting the hand *)
        if not targeting_hand && not is_selected_card then ui_elements := !ui_elements @ [card_ui];
        match selected with
            (* this card is the selected card *)
            | Selection c when c.id = card.id ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = true; hil = false; cost = card.cost; act = card.action_type}]
            (* the selected card targets individual cards *)
            | Selection _ when is_valid_target_kind card_ui valid_targets ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = false; hil = true; cost = card.cost; act = card.action_type}]

            | _ ->
                draw_cmds :=  !draw_cmds @ [DrawCard {x=x; y=y; w=card_w; h=card_h; sel = false; hil = false; cost = card.cost; act = card.action_type}]
    in
    (* draw each card in hand *)
    List.iteri gen_card_cmd game.player.hand;

    (* draw player *)
    let player = game_state.game.player in
    let player_ui = PlayerUI {player=player; box = {x=player_x; y=player_y; w=player_w; h=player_h}} in
    let player_cmd =
        match selected with
            | Selection _ when is_valid_target_kind player_ui valid_targets ->
                DrawPlayer {x=player_x; y=player_y; w=player_w; h=player_h; hil=true ;hp=player.hp; block=player.block}
            | _ -> 
                DrawPlayer {x=player_x; y=player_y; w=player_w; h=player_h; hil=false ;hp=player.hp; block=player.block}
    in
    draw_cmds := !draw_cmds @ [player_cmd];
    if targeting_player then ui_elements := !ui_elements @ [player_ui];

    (* draw enemies *)
    let gen_enemy_cmd id _ = 
        let enemy = IntMap.find id game.enemies in
        let enemy_ui = EnemyUI {id=id; box = {x=enemy_x; y=enemy_y; w=enemy_w; h=enemy_h}} in
        let enemy_cmd =
            match selected with
                | Selection _ when is_valid_target_kind enemy_ui valid_targets ->
                    DrawEnemy {x=enemy_x; y=enemy_y; w=enemy_w; h=enemy_h; hil=true; hp=enemy.hp; block=enemy.block; act=enemy.selected_action}
                | _ ->
                    DrawEnemy {x=enemy_x; y=enemy_y; w=enemy_w; h=enemy_h; hil=false; hp=enemy.hp; block=enemy.block; act=enemy.selected_action}
        in
        draw_cmds := !draw_cmds @ [enemy_cmd];
        if targeting_enemy then ui_elements := !ui_elements @ [enemy_ui]
    in
    (* draw each enemy *)
    IntMap.iter gen_enemy_cmd game.enemies;

    (* draw mana bar *)
    draw_cmds := !draw_cmds @ [DrawManaBar {x=mana_bar_x; y=mana_bar_y; w=mana_bar_w; h=mana_bar_h; remain=game.player.mana; cap=game.player.mana_cap}];

    (* draw end turn button *)
    let end_turn_cmd = DrawEndTurn {x=end_turn_x; y=end_turn_y; w=end_turn_w; h=end_turn_h} in
    let end_turn_ui = EndTurnUI {box = {x=end_turn_x; y=end_turn_y; w=end_turn_w; h=end_turn_h}} in
    draw_cmds := !draw_cmds @ [end_turn_cmd];
    (* disable end turn button if a card is selected *)
    if not is_selection then ui_elements := !ui_elements @ [end_turn_ui];

    (* draw victory or defeat screens *)
    (match game.end_state with
        | Victory -> draw_cmds := !draw_cmds @ [DrawVictoryScreen {x=end_message_x; y=end_message_y}]
        | Defeat -> draw_cmds := !draw_cmds @ [DrawDefeatScreen {x=end_message_x; y=end_message_y}]
        | Ongoing -> ());

    {draw_cmds = !draw_cmds; ui_elements = !ui_elements}

let draw_card x y w h sel hil cost act =
    (if sel then
        draw_rectangle x y w h Color.green
    else if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.lightgray);
    draw_rectangle_lines x y w h Color.darkgray;
    draw_text (string_of_int cost) (x + 100) (y + 10) 20 Color.blue;
    let action_string = string_of_action_type act in
    draw_text action_string (x + 15) (y + h/2) 5 Color.black

let draw_player x y w h hil hp block = 
    (if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.blue);
    draw_text (string_of_int hp) (x + 10) (y + 10) 20 Color.white;
    draw_text (string_of_int block) (x + w - 20) (y + 10) 20 Color.white

let draw_enemy x y w h hil hp block act = 
    (if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.red);
    draw_text (string_of_int hp) (x + 10) (y + 10) 20 Color.black;
    draw_text (string_of_int block) (x + w - 20) (y + 10) 20 Color.black;
    let action_string = string_of_action_type act in
    draw_text action_string (x + 15) (y + h/2) 5 Color.black

let draw_hand x y w h hil = 
    if hil then
        draw_rectangle x y w h Color.gold

let draw_deck x y w h hil size = 
    (if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.darkgray);
    draw_text (Printf.sprintf "%d" size) (x + 10) (y + 10) 20 Color.black
    

let draw_end_turn x y w h = 
    draw_rectangle x y w h Color.darkbrown;
    draw_text "End Turn" (x + 10) (y + 10) 20 Color.white

let draw_mana_bar x y w h remain cap = 
    draw_rectangle x y w h Color.blue;
    draw_text (Printf.sprintf "%d / %d" remain cap) (x + 10) (y + 10) 20 Color.white

let draw_victory x y =  
    draw_text "Victory" x y 50 Color.blue

let draw_defeat x y =  
    draw_text "Defeat" x y 50 Color.red

let draw_layout layout = 
    let draw_cmd cmd = 
        match cmd with
            | DrawCard c ->
                draw_card c.x c.y c.w c.h c.sel c.hil c.cost c.act
            | DrawPlayer p ->
                draw_player p.x p.y p.w p.h p.hil p.hp p.block
            | DrawEnemy e ->
                draw_enemy e.x e.y e.w e.h e.hil e.hp e.block e.act
            | DrawHand h ->
                draw_hand h.x h.y h.w h.h h.hil
            | DrawDeck d ->
                draw_deck d.x d.y d.w d.h d.hil d.size
            | DrawEndTurn b ->
                draw_end_turn b.x b.y b.w b.h
            | DrawManaBar m ->
                draw_mana_bar m.x m.y m.w m.h m.remain m.cap
            | DrawVictoryScreen s ->
                draw_victory s.x s.y
            | DrawDefeatScreen s ->
                draw_defeat s.x s.y
    in
    List.iter draw_cmd layout.draw_cmds

let () =
    (* initialize game starting state *)
    Random.self_init ();
    let enemies_simple = IntMap.add 0 enemy_simple IntMap.empty in
    let game_simple = {player = player_simple; enemies = enemies_simple; end_state = Ongoing} in
    let og_game_state = {game = pre_turn_processing game_simple; selected = NoSelection} in
    let og_layout = gen_layout og_game_state in

    (* initialize window *)
    init_window 1280 720 "OCaml Card Game";
    set_target_fps 60;

    let rec loop (layout : ui_layout) game_state =
        if window_should_close () then layout
        else begin
            (* win/lose check *)

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
