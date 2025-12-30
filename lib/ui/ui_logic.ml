open Ui_types
open Raylib
open Game_logic.Rules
open Core.Types
open Constants


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

(* TODO refactor this into multiple functions *)
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