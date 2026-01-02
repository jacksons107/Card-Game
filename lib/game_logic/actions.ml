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

(* TODO should this be involved in discarding? *)
(* general helper to remove a specific card from the player's hand, return the new hand *)
let rec remove_from_hand (c : card) (h : hand) = 
    match h with
    | [] -> []
    | x :: xs ->
        if x.id = c.id then
        xs
        else
        x :: remove_from_hand c xs

let remove_group_from_hand cards (hand : hand) = 
    List.fold_left
        (fun h c -> remove_from_hand c h)
        hand
        cards


(* helper to generate a fresh card id, returns id and new game *)
let fresh_card_id (game : game) =
    let id = game.next_card_id in
    (id, {game with next_card_id = id + 1})

(* clone a card and give it a new id, returns the new card and new game
   recursively clones any cards contained within the card also *)
let rec clone_card (card : card) (game : game) : card * game =
    let id = game.next_card_id in
    let game = { game with next_card_id = id + 1 } in
    let action_type, game = clone_action_type card.action_type game in
    ({ card with id; action_type }, game)

and clone_action_type (act : action_type) (game : game) =
    match act with
    | Attack _ | Block _ | EmptyBag _ ->
        (act, game)

    | FullBag cards ->
        let cards, game = clone_card_list cards game in
        (FullBag cards, game)

    | Modifier m ->
        let m, game = clone_modifier m game in
        (Modifier m, game)

    | Time t ->
        let t, game = clone_time t game in
        (Time t, game)

and clone_modifier (m : modifier_type) (game : game) =
    match m with
    | PowInc _ | Clone _ ->
        (m, game)

    | Map (inner, n) ->
        let inner, game = clone_modifier inner game in
        (Map (inner, n), game)

and clone_time (t : time_type) (game : game) =
    match t with
    | Backward _ ->
        (t, game)

(* clone a list of cards, used as a helper for clone_card and by other functions *)
and clone_card_list (cards : card list) (game : game) =
    List.fold_left
        (fun (acc, game) card ->
        let card, game = clone_card card game in
        (card :: acc, game))
        ([], game)
        cards
    |> fun (rev_cards, game) ->
        (List.rev rev_cards, game)


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
let rec incr_card_power power (card : card) = 
    match card.action_type with
        | Attack (d, t) -> {card with action_type = Attack (d+power, t)}
        | Block b -> {card with action_type = Block (b+power)}
        | EmptyBag (n, c) -> {card with action_type = EmptyBag (n+power, c)}
        | FullBag cs -> {card with action_type = FullBag (List.map (incr_card_power power) cs)}
        | Modifier m ->
            (match m with
                | PowInc p -> {card with action_type = Modifier (PowInc (p+power))}
                | Map (a, t) -> {card with action_type = Modifier (Map (a, t+power))}
                | Clone n -> {card with action_type = Modifier (Clone (n+power))})
        | Time t ->
            (match t with
                | Backward j -> {card with action_type = Time (Backward (j+power))})

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

(* action to clone target card num amount of times *)
let rec clone_action num (game : game) (target : target) = 
    if num <= 0 then game else
    match target with
        | Card c ->
            let clone, new_game = clone_card c game in
            let new_player = {new_game.player with hand = clone :: new_game.player.hand} in
            clone_action (num-1) {new_game with player = new_player} target
        | _ ->
            failwith "Can only clone a card."


(* --- Time travelling actions --- *)

(* TODO how to handle case when they want to travel to a non existent index? *)
(* travel backwards in time by x, if that entry exists, and restore game state with card also in hand,
   overwrite entrie's timeline, time_idx, and next_card_id with most recent  *)
let travel_back j (game : game) (target : target) =
    match target with
        (* TODO use of card group is a little jank if only ever expecting one card here *)
        | CardGroup cards ->
            let card, clone_game = clone_card (List.hd cards) game in
            let len = Array.length clone_game.timeline in
            let new_idx = clone_game.time_idx - j in
            if new_idx >= 0 && new_idx < len then
                let new_timeline = clone_game.timeline in
                let new_next_id = clone_game.next_card_id in
                let new_game = Array.get clone_game.timeline new_idx in
                let new_hand = card :: new_game.player.hand in
                {new_game with 
                    player = {new_game.player with hand = new_hand};
                    timeline = new_timeline; 
                    time_idx = new_idx;
                    next_card_id = new_next_id}
            else
                failwith "Trying to time travel (back) to non existent index."
        | _ ->
            failwith "Can only apply travel_back to game target."


(* --- Bag Actions --- *)
(* stick the target group of cards into a full bag card *)
let pack_bag num_slots cost (game : game) (target : target) = 
    match target with
        | CardGroup g ->
            if List.length g != num_slots then
                failwith "Number of card in group must exactly match number of slots in bag."
            else 
                (* clone group of cards *)
                let clones, clones_game = clone_card_list g game in
                (* create fresh id for full bag card *)
                let id, new_game = fresh_card_id clones_game in
                (* create full bag card with cloned group inside and cost *)
                let full_bag = {id = id; cost = cost; action_type = FullBag clones} in
                (* remove group cards from hand *)
                let new_hand = full_bag :: remove_group_from_hand g new_game.player.hand in
                (* return updated game *)
                {new_game with player = {new_game.player with hand = new_hand}}
        | _ ->
            failwith "Can only pack a bag with a group of cards."

(* TODO should this target game or hand or something else? *)
(* stick the contents of the bag at the front of the hand *)
let unpack_bag contents (game : game) (target : target) = 
    match target with 
        | Game ->
            {game with player = {game.player with hand = contents @ game.player.hand}}
        | _ -> 
            failwith "Unpack bag can only target game."

(* Instantiate an action from an action_type. To create a new action you have to
   write a function determining what the action does, create an action_type to
   represent it, and then add a new case to this function to map from the
   action_type to the function call. *)
let rec instantiate_action action_type = 
    match action_type with
        | Attack (d, t) -> attack_multiple d t
        | Block b -> block b
        | EmptyBag (n, c) -> pack_bag n c
        | FullBag cs -> unpack_bag cs
        | Modifier m ->
            (match m with
                | PowInc p -> incr_power p
                | Map (a, t) -> map_modifier (instantiate_action (Modifier a)) t
                | Clone n -> clone_action n)
        | Time t ->
            (match t with
                | Backward j -> travel_back j)


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