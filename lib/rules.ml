open Types
open Actions

(* TODO organize this into individual files for related utilities and aggregate here *)

(* helper that pics a random element from an array, used for picking enemy actions and blocks *)
let pick_rand_element arr = 
    let length = Array.length arr in
    let idx = Random.int(length) in
    arr.(idx)

(* enemy randomly picks one of their actions and set it as their selected action *)
let enemy_pick_action (enemy : enemy) = 
    let selected_action = pick_rand_element enemy.actions in
    {enemy with selected_action = selected_action}

(* has each enemy select a new selected action *)
let select_enemy_actions (game : game) = 
    let new_enemies = IntMap.map enemy_pick_action game.enemies in
    {game with enemies = new_enemies}

(* collect all enemy actions into a list *)
let get_enemy_actions (game : game) = 
    IntMap.fold 
        (fun _ e acc -> e.selected_action::acc)
        game.enemies
        []

(* applies all the enemies' chosen actions to the player *)
let rec apply_enemy_actions (g : game) actions = 
    match actions with
        | [] -> g
        | x::xs -> 
            let action = instantiate_action x in
            apply_enemy_actions (action g Player) xs

(* enemy randomly picks one of their blocks *)
let enemy_pick_block (enemy : enemy) = 
    pick_rand_element enemy.block_vals

(* helper to set the player's mana *)
let set_mana new_mana (game : game) =
    let new_player = {game.player with mana = new_mana} in
    {game with player = new_player}

(* helper to set the block of a player or enemy *)
let set_block new_block target (game : game) = 
    match target with
        | Player ->
            let new_player = {game.player with block = new_block} in
            {game with player = new_player}
        | Enemy id ->
            let new_enemies = IntMap.update id (update_enemy_block new_block) game.enemies in
            {game with enemies = new_enemies}
        | _ -> 
            failwith "Can only set block of player or enemy." 

(* TODO should this be involved in discarding? *)
(* removes a specific card from the player's hand, return the new hand *)
let rec remove_from_hand (c : card) (h : hand) = 
    match h with
    | [] -> []
    | x :: xs ->
        if x.id = c.id then
        xs
        else
        x :: remove_from_hand c xs

(* applies a card (the card's action) to the specified target *)
let apply_card (c : card) (t : target) (g : game) = 
    let new_mana = g.player.mana - c.cost in
    let new_hand = remove_from_hand c g.player.hand in
    let new_player = {g.player with mana = new_mana; hand = new_hand} in
    let action = instantiate_action c.action_type in
    action {g with player = new_player} t

(* gets the top card of the deck (if it exists), and returns that card and the new deck *)
let take_top_deck (d : deck) = 
    match d with
        | [] -> (None, d)
        | x::xs -> (Some x, xs)

(* draws the specified number of cards from the deck, returns a new game state *)
let rec draw_cards num_cards (game : game) = 
    if num_cards = 0 then game else
    let top_card = take_top_deck game.player.deck in
    match top_card with
        | (None, _) -> game
        | (Some c, new_d) ->
            let new_p = {game.player with 
                            hand = c::game.player.hand;
                            deck = new_d}
            in
            let new_game = {game with player = new_p} in
            draw_cards (num_cards - 1) new_game

(* Removes all dead enemies (hp <= 0) from the game *)
let remove_dead_enemies (game : game)=
    let alive_enemies =
        IntMap.filter
            (fun _ (e : enemy) -> e.hp > 0)
            game.enemies
    in
    { game with enemies = alive_enemies }

(* set the block for each enemy *)
let set_enemies_block (game : game) = 
    let new_enemies = IntMap.map (fun e -> ({e with block = enemy_pick_block e} : enemy)) game.enemies in
    {game with enemies = new_enemies}

(* get the target kinds of an action type *)
let get_target_kinds action_type = 
    match action_type with
        | Attack _ -> [TKEnemy]
        | Block _ -> [TKPlayer]
        | Modifier m ->
            match m with
                | PowInc _ -> [TKCard]
                | Map _ -> [TKHand; TKDeck]

let set_end_state (game : game) = 
    if game.player.hp <= 0 then 
        {game with end_state = Defeat}
    else if IntMap.is_empty game.enemies then 
        {game with end_state = Victory}
    else 
        game

(* steps to do after every action *)
let post_action_check (game : game) = 
    game
    |> remove_dead_enemies
    |> set_end_state
    
(* do all the between-turn steps *)
let pre_turn_processing (game : game) = 
    game
    |> remove_dead_enemies
    |> select_enemy_actions
    |> (draw_cards 1)
    |> set_enemies_block
    |> (set_block 0 Player)
    |> (set_mana game.player.mana_cap)
    |> set_end_state
