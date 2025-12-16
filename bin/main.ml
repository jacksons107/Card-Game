module IntMap = Map.Make(Int)

type game = {player : player; enemies : enemy IntMap.t}

and enemy = {hp : int; block : int; actions : action list}

and player = {hp : int; block : int; hand : hand; deck : deck}

and deck = card list

and hand = card list

and card = {id : int; cost : int; action_type : action_type}

and action_type = 
    | Attack of int * int
    | Block of int
    | PowInc of int
    | Map of action_type * int

and action = game -> target -> game

and target = Player | Enemy of int | Card of card | Hand | Deck

(* General helpers to update an enemy stats. *)
let update_enemy_hp new_hp enemy_opt = 
    match enemy_opt with
        | Some enemy -> Some {enemy with hp = new_hp}
        | None -> None

let update_enemy_block new_block enemy_opt = 
    match enemy_opt with
        | Some enemy -> Some {enemy with block = new_block}
        | None -> None


(* --- Attack action builders --- *)
let attack damage game target = 
    match target with
        | Player -> 
            let new_hp = game.player.hp - damage in
            let p = game.player in
            {game with player = {p with hp = new_hp}}
        | Enemy id ->
            let e = IntMap.find id game.enemies in
            let new_hp = e.hp - damage in
            let new_enemies = IntMap.update id (update_enemy_hp new_hp) game.enemies in
            {game with enemies = new_enemies}
        | _ ->
            failwith "Can only attack enemies."

(* TODO replace now that there is apply_x_times helper *)
let attack_multiple damage times = 
    let attack_d = attack damage in 
    let rec attack_times t game target = 
        if t = 0 then game else attack_times (t-1) (attack_d game target) target
    in
    ((attack_times times) : action)

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
let rec incr_card_power power card = 
    match card.action_type with
        | Attack (d, t) -> {card with action_type = Attack (d+power, t)}
        | Block b -> {card with action_type = Block (b+power)}
        | PowInc p -> {card with action_type = PowInc (p+power)}
        | Map (a, t) -> {card with action_type = Map (a, t+power)}

let replace_card (new_card : card) (hand : hand) : hand = 
    List.map (fun current_element ->
        if current_element.id = new_card.id then new_card else current_element
    ) hand

let incr_power power game target = 
    match target with
        | Card c ->
            let new_card = incr_card_power power c in
            let new_hand = replace_card new_card game.player.hand in
            let new_player = {game.player with hand = new_hand} in
            {game with player = new_player}
        | _ -> failwith "Can only increase power of a card."

(* General helper to apply an action x times. *)
let rec apply_x_times (action : action) x game target = 
    if x = 0 then game else apply_x_times action (x-1) (action game target) target

(* Map a card modifying action to hand or deck 'times' amount of times. *)
let map_modifier (mod_action : action) times game (cards : target) = 
    let rec _map_modifier mod_action game cards =
        match cards with
            | [] -> game
            | x::xs -> _map_modifier (apply_x_times mod_action times) (mod_action game (Card x)) xs
    in
    match cards with
        | Hand -> 
            _map_modifier mod_action game game.player.hand
        | Deck ->
            _map_modifier mod_action game game.player.deck
        | _ -> failwith "Can only map a modifier onto hand or deck."


(* Instantiate an action from an action_type. *)
let rec instantiate_action action_type = 
    match action_type with
        | Attack (d, t) -> attack_multiple d t
        | Block b -> block b
        | PowInc p -> incr_power p
        | Map (a, t) -> map_modifier (instantiate_action a) t


(* ======= Example actions =========*)

(* Attack for 2 damage 1 time. *)
let attack_2 = instantiate_action (Attack (2, 1))
(* Attack for 2 damage 4 times. *)
let attack_2_4 = instantiate_action (Attack (2, 4))
(* Add 3 block. *)
let block_3 = instantiate_action (Block 3)
(* Increase the power stat of a card by 1. *)
let power_inc_1 = instantiate_action (PowInc 1)
(* Map the power_inc_1 function over hand or deck 1 time. *)
let power_inc_hand_1 = instantiate_action (Map ((PowInc 1), 1))