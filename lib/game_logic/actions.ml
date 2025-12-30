open Core.Types

(* TODO should eventually separate actions into their own files and aggregate here *)

(* General helpers to update an enemy stats. *)
let update_enemy_hp new_hp (enemy_opt : enemy option) = 
    match enemy_opt with
        | Some enemy -> Some {enemy with hp = new_hp}
        | None -> None

let update_enemy_block new_block (enemy_opt : enemy option) = 
    match enemy_opt with
        | Some enemy -> Some {enemy with block = new_block}
        | None -> None

(* General helper to apply an action x times. *)
let rec apply_x_times (action : action) x game target = 
    if x = 0 then game else apply_x_times action (x-1) (action game target) target

(* --- Attack action builders --- *)
let attack damage game target = 
    let apply_attack damage hp block = 
        let diff = block - damage in
        let new_block = if diff <= 0 then 0 else diff in
        let new_hp = if diff < 0 then hp + diff else hp in
        (new_hp, new_block)
    in
    match target with
        | Player -> 
            let new_hp, new_block = apply_attack damage game.player.hp game.player.block in
            let p = game.player in
            {game with player = {p with hp = new_hp; block = new_block}}
        | Enemy id ->
            let e = IntMap.find id game.enemies in
            let new_hp, new_block = apply_attack damage e.hp e.block in 
            let new_enemies_hp = IntMap.update id (update_enemy_hp new_hp) game.enemies in
            let new_enemes_block = IntMap.update id (update_enemy_block new_block) new_enemies_hp in
            {game with enemies = new_enemes_block}
        | _ ->
            failwith "Can only attack enemies."

let attack_multiple damage times = 
    let attack_d = attack damage in 
    ((apply_x_times attack_d times) : action)

(* --- Block action builders --- *)
let block block game target = 
    match target with
        | Player -> 
            let new_block = game.player.block + block in
            let p = game.player in
            {game with player = {p with block = new_block}}
        | Enemy id ->
            let e = IntMap.find id game.enemies in
            let new_block = e.block - block in
            let new_enemies = IntMap.update id (update_enemy_block new_block) game.enemies in
            {game with enemies = new_enemies}
        | _ ->
            failwith "Cannot add block to cards."

(* --- Card modifying action builders --- *)
let incr_card_power power card = 
    match card.action_type with
        | Attack (d, t) -> {card with action_type = Attack (d+power, t)}
        | Block b -> {card with action_type = Block (b+power)}
        | Modifier m ->
            match m with
                | PowInc p -> {card with action_type = Modifier (PowInc (p+power))}
                | Map (a, t) -> {card with action_type = Modifier (Map (a, t+power))}

(* TODO inefficient and sketchy to do the mapping on hand and deck relying on card id *)
let replace_card (new_card : card) (game : game )= 
    let replace cards = 
        List.map (fun current_element ->
            if current_element.id = new_card.id then new_card else current_element
        ) cards
    in
    let new_hand = replace game.player.hand in
    let new_deck = replace game.player.deck in
    {game with player = {game.player with hand = new_hand; deck = new_deck}}

let incr_power power game target = 
    match target with
        | Card c ->
            let new_card = incr_card_power power c in
            replace_card new_card game
        | _ -> failwith "Can only increase power of a card."

(* Map a card modifying action to hand or deck 'times' amount of times. *)
let rec map_modifier (mod_action : action) times game (cards : target) = 
    if times = 0 then game else
    let rec _map_modifier (mod_action : action) game cards =
    match cards with
        | [] -> game
        (* | x::xs -> _map_modifier (apply_x_times mod_action times) (mod_action game (Card x)) xs *)
        | x::xs -> _map_modifier mod_action (mod_action game (Card x)) xs
    in
    let new_game = match cards with
        | Hand ->
            _map_modifier mod_action game game.player.hand
        | Deck ->
            _map_modifier mod_action game game.player.deck
        | _ -> 
            failwith "Can only map a modifier onto hand or deck."
    in
    map_modifier mod_action (times - 1) new_game cards

(* Instantiate an action from an action_type. To create a new action you have to
   write a function determining what the action does, create an action_type to
   represent it, and then add a new case to this function to map from the
   action_type to the function call. *)
let rec instantiate_action action_type = 
    match action_type with
        | Attack (d, t) -> attack_multiple d t
        | Block b -> block b
        | Modifier m ->
            match m with
                | PowInc p -> incr_power p
                | Map (a, t) -> map_modifier (instantiate_action (Modifier a)) t


(* ======= Example actions ========= *)

(* Attack for 2 damage 1 time. *)
let attack_2 = instantiate_action (Attack (2, 1))
(* Attack for 2 damage 4 times. *)
let attack_2_4 = instantiate_action (Attack (2, 4))
(* Add 3 block. *)
let block_1 = instantiate_action (Block 1)
(* Increase the power stat of a card by 1. *)
let power_inc_1 = instantiate_action (Modifier (PowInc 1))
(* Map the power_inc_1 function over hand or deck 1 time. *)
let power_inc_hand_1 = instantiate_action (Modifier (Map ((PowInc 1), 1)))