open Types 
open Rules

type card_selection = Card of card | EndTurn
type target_selection = Target of target | UnselectCard

(* TODO add QUIT option *)
(* prompt the player to select a card to play *)
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

(* prompt the player to select the target of a selected card,
   can also result in the player unselecting the selected card *)
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

(* TODO find a better way to render game state each time a card is played *)
(* the player's turn, a loop allowing them to select and play cards unti they choose the end their turn
   takes an optional render function that renders the game state each time a card is played *)
let rec play_cards (game : game) render_fun = 
    (match render_fun with
        | Some f -> f game
        | None -> ());
    let chosen_card = prompt_choose_card game in
    match chosen_card with
        | EndTurn -> game
        | Card c ->
            let target = prompt_choose_target c game in
            match target with
                | UnselectCard -> play_cards game render_fun
                | Target t -> 
                    let applied = apply_card c t game in
                    play_cards applied render_fun
