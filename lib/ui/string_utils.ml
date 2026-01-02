open Core.Types

let rec string_of_action_type a_type = 
    match a_type with
        | Attack (d, t) -> Printf.sprintf "Attack %d damage, %d times" d t
        | Block b -> Printf.sprintf "Block %d" b
        | EmptyBag (n, c) -> Printf.sprintf "Empty bag with %d slots that will cost %d to unpack" n c
        | FullBag cs -> Printf.sprintf "Full bag with %d cards in it" (List.length cs)
        | Modifier m ->
            (match m with
            | PowInc p -> Printf.sprintf "Increase power %d" p
            | Map (a, t) -> Printf.sprintf "Map [%s] %d times" (string_of_action_type (Modifier a)) t
            | Clone n -> Printf.sprintf "Clone a card %d times" n)
        | Time t ->
            (match t with
                | Backward j -> Printf.sprintf "Travel back %d with one card" j)

(* print a list of enemy actions, assumes enemy actions are in order of enemy id *)
let print_enemy_actions enemy_actions = 
    List.iteri (fun i a -> Printf.printf "Enemy %d doing %s\n" i (string_of_action_type a))
    enemy_actions

let string_of_card (c : card) = 
    let action_string = string_of_action_type c.action_type in
    Printf.sprintf "ID %d, Cost %d, %s\n" c.id c.cost action_string

let print_player (p : player) = 
    Printf.printf "Player HP: %d\n" p.hp;
    Printf.printf "Mana: %d\n" p.mana;
    Printf.printf "Block: %d\n" p.block;
    Printf.printf "Hand:\n";
    List.iteri
        (fun i c -> Printf.printf "  [%d] %s" i (string_of_card c))
        p.hand;
    Printf.printf "Deck: %d\n" (List.length p.deck)

let print_enemy (e : enemy) = 
    Printf.printf "HP: %d Block: %d\n" e.hp e.block

let print_enemies enemies = 
    IntMap.iter 
        (fun k v -> 
            Printf.printf "Enemy %d, doing %s\n" k (string_of_action_type v.selected_action); print_enemy v) 
        enemies

let print_game_state (g : game) = 
    print_endline "=== Player ===";
    print_player g.player;
    print_endline "=== Enemies ===";
    print_enemies g.enemies;

