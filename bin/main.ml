type enemy = {hp : int}

type action =
    Attack of (enemy -> enemy)

type card = {
    id : int;
    act : action;
    desc : string
}

type hand = card list

type deck = card list

type player = {hp : int; hand : hand; deck : deck}

(* Helper function that takes card at 'idx' out of 'hand' and returns the card
   and the new hand without the card in it. *)
let takeFromHand hand idx = 
    let rec _takeFromHand (hand : hand) idx res = match hand with
        | [] -> failwith "Trying to take card at nonexistent index."
        | x::xs -> if idx = 0 then (x, (res@xs : hand)) else _takeFromHand xs (idx - 1) (res@[x])
    in
    _takeFromHand hand idx []

(* Play the card at 'idx' targeting enemy 'target', and return new version of 
   player 'p' without card in hand and new version of enemy after effect of 
   playing the card. *)
let play p idx target = match takeFromHand p.hand idx with 
    (c, new_hand) -> 
        let new_player = {hp = p.hp; hand = new_hand; deck = p.deck} in
        match c with
            {act = a; _} ->
                match a with
                    Attack (att) -> (new_player, att target)

(* Take the top card from the deck and return it with the rest of the deck. *)
let takeTopDeck (d : deck) = 
    match d with
        | [] -> (None, d)
        | x::xs -> (Some x, (xs : deck))

(* Draw the top card from player 'p' deck and put it at the end of the player's hand. *)
let draw p = 
    let c, new_deck = 
        match p with {deck = d; _} -> takeTopDeck d
    in
        match c with
            | Some crd -> {hp = p.hp; hand = p.hand@[crd]; deck = new_deck}
            | None -> {hp = p.hp; hand = p.hand; deck = new_deck}

(* General template for an attack card. *)
let attack d (e : enemy) = match e with {hp = h} -> {hp = h - d}

(* Attack card that deals 5 damage. *)
let attack5 = Attack (attack 5)

(* Attack card that deals 2 damage. *)
let attack2 = Attack (attack 2)

(* Two instances of attack5 cards. *)
let c1 = {id = 1; act = attack5; desc = "Attack 5"}
let c2 = {id = 2; act = attack5; desc = "Attack 5"}

(* Instance of a deck with three attack2 cards. *)
let d = [{id=3; act = attack2; desc = "Attack 2"}; 
         {id=4; act = attack2; desc = "Attack 2"}; 
         {id=5; act = attack2; desc = "Attack 2"}]

(* An enemey. *)
let opp = {hp = 10}

(* A player. *)
let hero = {hp = 10; hand = [c1; c2]; deck = d}

let print_enemy (e : enemy) =
  Printf.printf "Enemy HP: %d\n" e.hp

let print_player (p : player) =
    Printf.printf "Player HP: %d\n" p.hp;
    Printf.printf "Hand:\n";
    List.iteri
        (fun i c -> Printf.printf "  [%d] ID %d %s\n" i c.id c.desc)
        p.hand;
    Printf.printf "Deck: %d\n" (List.length p.deck)


let read_int () =
  try int_of_string (read_line ())
  with _ ->
    print_endline "Invalid number.";
    read_int ()

let rec choose_card (p : player) =
  print_string "Choose a card index: ";
  let idx = read_int () in
  if idx < 0 || idx >= List.length p.hand then (
    print_endline "Invalid card index.";
    choose_card p
  ) else
    idx

let player_turn (p : player) (e : enemy) =
  print_endline "\n=== Player Turn ===";
  print_player p;
  print_enemy e;

  let idx = choose_card p in
  let new_p, new_e = play p idx e in
  (new_p, new_e)

let rec game_loop (p : player) (e : enemy) =
  if e.hp <= 0 then
    print_endline "\nYou win!"
  else if p.hp <= 0 then
    print_endline "\nYou lose!"
  else
    let drawn_p = draw p in
    let new_p, new_e = player_turn drawn_p e in
    game_loop new_p new_e

let () =
  print_endline "Welcome to Caml: The Ultimate!";
  game_loop hero opp
