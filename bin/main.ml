Random.self_init ()
module IntMap = Map.Make(Int)

type game = {player : player; enemies : enemy IntMap.t}

and enemy = {hp : int; block : int; actions : action_type list}

and player = {hp : int; mana : int; mana_cap : int; block : int; hand : hand; deck : deck}

and deck = card list

and hand = card list

and card = {id : int; cost : int; action_type : action_type; desc : string}

and action_type = 
    | Attack of int * int
    | Block of int
    | Modifier of modifier_type 
    (* TODO template type? *)

and modifier_type = 
    | PowInc of int
    | Map of modifier_type * int

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
            (* let new_hp = game.player.hp - damage in *)
            let new_hp, new_block = apply_attack damage game.player.hp game.player.block in
            let p = game.player in
            {game with player = {p with hp = new_hp; block = new_block}}
        | Enemy id ->
            let e = IntMap.find id game.enemies in
            (* let new_hp = e.hp - damage in *)
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
        | Modifier m ->
            match m with
                | PowInc p -> incr_power p
                | Map (a, t) -> map_modifier (instantiate_action (Modifier a)) t


(* ======= Example actions ========= *)

(* Attack for 2 damage 1 time. *)
(* let attack_2 = instantiate_action (Attack (2, 1)) *)
(* Attack for 2 damage 4 times. *)
(* let attack_2_4 = instantiate_action (Attack (2, 4)) *)
(* Add 3 block. *)
(* let block_1 = instantiate_action (Block 1) *)
(* Increase the power stat of a card by 1. *)
(* let power_inc_1 = instantiate_action (Modifier (PowInc 1)) *)
(* Map the power_inc_1 function over hand or deck 1 time. *)
(* let power_inc_hand_1 = instantiate_action (Modifier (Map ((PowInc 1), 1))) *)

(* ======= Example cards ========= *)
let attack_2_card = {id = 0; cost = 1; action_type = Attack (2, 1); desc = "Attack 2"}
let block_3_card = {id = 1; cost = 2; action_type = Block 3; desc = "Block 3"}
(* let power_inc_1_card = {id = 2; cost = 1; action_type = (Modifier (PowInc 1)); desc = "Pow Inc 1"} *)
let power_inc_hand_1 = {id = 3; cost = 3; action_type = (Modifier (Map ((PowInc 1), 1))); desc = "Map Pow Inc 1"}
(* TODO tempate map card? *)

(* ======= Example player ========= *)
let player_simple = {hp = 10;  
                    mana = 0;
                    mana_cap = 5;
                    block = 0; 
                    hand = [attack_2_card; block_3_card];
                    deck = [power_inc_hand_1; power_inc_hand_1]}

(* ======= Example enemy ========= *)
let enemy_simple = {hp = 10;  
                    block = 0; 
                    actions = [Attack (2, 1); Block 1]}

(* ======= Example game starting state ========= *)
let enemies_simple = IntMap.add 0 enemy_simple IntMap.empty
let game_simple = {player = player_simple; enemies = enemies_simple}

(* ============================================= *)
let enemy_pick_action (enemy : enemy) = 
    let actions_array = Array.of_list enemy.actions in
    let length = Array.length actions_array in
    let idx = Random.int(length) in 
    actions_array.(idx)

let rec string_of_action_type a_type = 
    match a_type with
        | Attack (d, t) -> Printf.sprintf "Attack %d damage %d times" d t
        | Block b -> Printf.sprintf "Block %d" b
        | Modifier m ->
            match m with
            | PowInc p -> Printf.sprintf "Increase power %d" p
            | Map (a, t) -> Printf.sprintf "Map %s %d times" (string_of_action_type (Modifier a)) t

let print_enemy_actions enemy_actions = 
    List.iteri (fun i a -> Printf.printf "Enemy %d doing %s\n" i (string_of_action_type a))
    enemy_actions

let set_mana (game : game) new_mana =
    let new_player = {game.player with mana = new_mana} in
    {game with player = new_player}

let set_block (game : game) new_block target = 
    match target with
        | Player ->
            let new_player = {game.player with block = new_block} in
            {game with player = new_player}
        | Enemy id ->
            let new_enemies = IntMap.update id (update_enemy_block new_block) game.enemies in
            {game with enemies = new_enemies}
        | _ -> 
            failwith "Can only set block of player or enemy."


type card_selection = Card of card | EndTurn
let rec prompt_choose_card (g : game) = 
    print_string "Choose a card index or END to end turn: ";
    let p = g.player in
    let response = read_line () in
    if response = "END" then EndTurn else
    let idx = int_of_string response in
    if idx < 0 || idx >= List.length p.hand then (
        print_endline "Invalid card index.";
        prompt_choose_card g
    ) else
        let cards_array = Array.of_list p.hand in
        let selected_card = cards_array.(idx) in
        if selected_card.cost > p.mana then
            (print_endline "You do not have enough mana to play this card.";
            Printf.printf "Costs %d and only have %d mana\n" selected_card.cost p.mana;
            prompt_choose_card g)
        else 
            Card selected_card

type target_selection = Target of target | UnselectCard
let rec prompt_choose_target (c : card) (g : game) =
    match c.action_type with
        | Attack _ -> 
            print_string "Choose an enemy: ";
            let idx = read_int () in
            Target (Enemy idx)
        | Block _ -> print_string "Choose yourself (y/n): ";
            let response = read_line () in
            if response = "y" then 
                Target (Player) 
            else 
                UnselectCard   
        | Modifier m -> 
            match m with
                | PowInc _ -> 
                    print_string "Choose a card: ";
                    let idx = read_int () in
                    if idx < 0 || idx >= List.length g.player.hand then (
                        print_endline "Invalid card index.";
                        prompt_choose_target c g
                    ) else
                        let cards_array = Array.of_list g.player.hand in
                        let selected_card = cards_array.(idx) in
                        Target (Card selected_card)
                | Map _ -> 
                    print_string "Choose hand or deck: ";
                    let response = read_line () in
                    if response = "hand" then
                        Target Hand
                    else if response = "deck" then
                        Target Deck
                    else
                        UnselectCard

let print_player (p : player) = 
    Printf.printf "Player HP: %d\n" p.hp;
    Printf.printf "Mana: %d\n" p.mana;
    Printf.printf "Block: %d\n" p.block;
    Printf.printf "Hand:\n";
    List.iteri
        (fun i c -> Printf.printf "  [%d] ID %d %s\n" i c.id c.desc)
        p.hand;
    Printf.printf "Deck: %d\n" (List.length p.deck)

let print_enemy (e : enemy) = 
    Printf.printf "HP: %d Block: %d\n" e.hp e.block

let print_enemies enemies = 
    IntMap.iter (fun k v -> Printf.printf "Enemy %d " k; print_enemy v) enemies

(* TODO modify to also take enemy actions as input *)
let print_game_state (g : game) = 
    print_endline "=== Player ===";
    print_player g.player;
    print_endline "=== Enemies ===";
    print_enemies g.enemies

let rec apply_enemy_actions (g : game) actions = 
    match actions with
        | [] -> g
        | x::xs -> 
            let action = instantiate_action x in
            apply_enemy_actions (action g Player) xs

let remove_from_hand (c : card) (h : hand) = 
    let rec _remove c h (acc : hand) = 
        match h with
            | [] -> acc
            | x::xs -> 
                if x.id = c.id then
                    acc@xs
                else
                    _remove c xs acc@[x]
    in
    _remove c h []

let apply_card (c : card) (t : target) (g : game) = 
    let new_mana = g.player.mana - c.cost in
    let new_hand = remove_from_hand c g.player.hand in
    let new_player = {g.player with mana = new_mana; hand = new_hand} in
    let action = instantiate_action c.action_type in
    action {g with player = new_player} t

(* TODO should allow playing multiple cards in one turn *)
let rec play_cards (game : game) = 
    print_game_state game;
    let chosen_card = prompt_choose_card game in
    match chosen_card with
        | EndTurn -> game
        | Card c ->
            let target = prompt_choose_target c game in
            match target with
                | UnselectCard -> play_cards game
                | Target t -> 
                    let applied = apply_card c t game in
                    play_cards applied

let take_top_deck (d : deck) = 
    match d with
        | [] -> (None, d)
        | x::xs -> (Some x, xs)

let rec draw_cards (game : game) num_cards = 
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
            draw_cards new_game (num_cards - 1)

(* ==== Game Loop ==== *)
let rec game_loop (game : game) = 
    if game.enemies = IntMap.empty then 
       print_string  "Player wins"
    else if game.player.hp <= 0 then 
        print_string "Player loses"
    else
        (* Player mana restored *)
        let mana_restored = set_mana game game.player.mana_cap in
        (* Player block reset *)
        let player_block_reset = set_block mana_restored 0 Player in
        (* TODO change this to be setting enemy block *)
        (* Enemies block reset *)
        let enemies_block_reset = 
            let new_enemies = IntMap.map (fun e -> {e with block = 0}) player_block_reset.enemies in
            {player_block_reset with enemies = new_enemies}
        in
        let cards_drawn = draw_cards enemies_block_reset 1 in
        (* print_game_state cards_drawn; *)
        (* Enemies pick action *)
        let enemy_actions = 
            IntMap.fold 
            (fun _ v acc -> acc@[enemy_pick_action v]) 
            cards_drawn.enemies [] 
        in
        (* Show player enemy actions *)
        print_endline "=== Enemy Actions ===";
        print_enemy_actions enemy_actions;
        (* Player plays their cards *)
        let cards_played = play_cards cards_drawn in
        (* Apply enemey actions *)
        let enemies_acted = apply_enemy_actions cards_played enemy_actions in
        (* Loop *)
        game_loop enemies_acted

let () = 
    game_loop game_simple