module IntMap = Map.Make(Int)

type enemy = {hp : int; block : int; acts : action list}

and player = {hp : int; block : int; mana : int; hand : hand; deck : deck}

and actor = Player of player | Enemy of enemy

and action = 
    | Attack of int
    | Block of int
    | Map of int (* increase the power number of every card in hand or deck *)

and card = {id : int; cost : int; act : action; target : card_target_type; desc : string}

(* Types of entities that can be targeted by cards. *)
and card_target_type = SingleEnemy | AllEnemies | SingleCard | PlayerHand | PlayerDeck

and hand = card list

and deck = card list

(* Entities that can be the target of actions. *)
type target = 
    | Player of player
    | Enemy of enemy
    | Enemies of enemy IntMap.t
    | Card of card
    | Hand of hand
    | Deck of deck

type game = {player : player; enemies : enemy IntMap.t}


let attack2 = {id = 0; cost = 1; act = Attack 2; target = SingleEnemy; desc = "Attack for 2 damage."}
let block2 = {id = 1; cost = 1; act = Block 2; target = SingleEnemy; desc = "Add 2 block."}

let increasePower power card = 
    match card.act with
        | Attack d -> {card with act = Attack (d + power)}
        | Block b -> {card with act = Block (b + power)}
        | Map p -> {card with act = Map (p + power)}

let instantiateAction (action : action) (actor : actor) : (target -> target) = 
    match action with
        | Attack d -> 
            fun entity -> 
                let h = entity.hp - d in {entity with hp = h}
        | Block b ->
            fun entity -> 
                let blk = entity.block + b in {entity with block = blk}
        | Map p ->
            fun cards ->
                List.map (increasePower p) cards

let print_enemy id (e : enemy) =
    Printf.printf "Enemy ID: %d HP: %d\n" id e.hp

let print_enemies (game : game) = 
    IntMap.iter (fun x -> print_enemy x) game.enemies

let read_int () =
  try int_of_string (read_line ())
  with _ ->
    print_endline "Invalid number.";
    read_int ()

let selectEnemy enemies = 
    print_string "Select an enemy to target: ";
    IntMap.iter (fun x -> print_enemy x) enemies;
    let selected_id = read_int () in IntMap.find selected_id enemies

let print_hand (hand : hand) = 
    List.iteri
        (fun i c -> Printf.printf "  [%d] ID %d %s\n" i c.id c.desc)
        hand

let rec selectCard hand = 
    print_string "Select an card to use: ";
    print_hand hand;
    let selected_idx = read_int () in 
        if selected_idx < 0 || selected_idx >= List.length hand then (
            print_endline "Invalid card index.";
            selectCard hand
        ) else
            List.nth hand selected_idx

let findCardTargets card game = 
    match card.target with
        | SingleEnemy -> Enemy (selectEnemy game.enemies)
        | AllEnemies -> Enemies game.enemies
        | SingleCard -> Card (selectCard game.player.hand)
        | PlayerHand -> Hand game.player.hand
        | PlayerDeck -> Deck game.player.deck


(* 
(* Choose the action the enemy will take. *)
let chooseEnemyAction enemy = *)

let print_player (p : player) =
    Printf.printf "Player HP: %d\n" p.hp;
    Printf.printf "Hand:\n";
    print_hand p.hand;
    Printf.printf "Deck: %d\n" (List.length p.deck)

let player_turn (game : game) =
    print_endline "\n=== Player Turn ===";
    print_player game.player;
    print_enemies game;

    (* Player selects card from hand. *)
    let selected_card = selectCard game.player.hand in
    (* Player selects a target for the card. *)
    let target = findCardTargets selected_card game in
    (* Instantiate the action from the selected card. *)
    let action = instantiateAction selected_card.act in
    (* Apply the action to the target. *)
    let new_target = action target in 
    (* Update the game with the result of appying the action to the target. *)
    let new_game = update_game game new_target

let rec game_loop (game : game) =
  if e.hp <= 0 then
    print_endline "\nYou win!"
  else if p.hp <= 0 then
    print_endline "\nYou lose!"
  else
    let new_game = player_turn game in
    game_loop new_game

let () =
  print_endline "Welcome to Caml: The Ultimate!";
  game_loop initial_game