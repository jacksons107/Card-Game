type enemy = {hp : int}

type action =
    Attack of (enemy -> enemy)

type card = {
    id : int;
    act : action
}

type player = {hp : int; cards : card list}

(* Helper function that takes card at 'idx' out of 'hand' and returns the card
   and the new hand without the card in it. *)
let takeFromHand hand idx = 
    let rec _takeFromHand (hand : card list) idx res = match hand with
        | [] -> failwith "Hand is empty."
        | x::xs -> if idx = 0 then (x, res@xs) else _takeFromHand xs (idx - 1) (res@[x])
    in
    _takeFromHand hand idx []

(* Play the card at 'idx' targeting enemy 'target', and return new version of 
   player 'p' without card in hand and new version of enemy after effect of 
   playing the card. *)
let play p idx target = match takeFromHand p.cards idx with 
    (c, new_hand) -> 
        let new_player = {hp = p.hp; cards = new_hand} in
        match c with
            {id = _; act = a} ->
                match a with
                    Attack (att) -> (new_player, att target)

(* General template for an attack card. *)
let attack d (e : enemy) = match e with {hp = h} -> {hp = h - d}

(* Attack card that deals 5 damage. *)
let attack5 = Attack (attack 5)

(* Two instances of attack5 cards. *)
let c1 = {id = 1; act = attack5}
let c2 = {id = 2; act = attack5}

(* An enemey. *)
let opp = {hp = 10}

(* A player. *)
let hero = {hp = 10; cards = [c1; c2]}

let print_enemy (e : enemy) =
  Printf.printf "Enemy HP: %d\n" e.hp

let print_player (p : player) =
  Printf.printf "Player HP: %d\n" p.hp;
  Printf.printf "Hand:\n";
  List.iteri
    (fun i c -> Printf.printf "  [%d] Card %d\n" i c.id)
    p.cards

let read_int () =
  try int_of_string (read_line ())
  with _ ->
    print_endline "Invalid number.";
    read_int ()

let rec choose_card (p : player) =
  print_string "Choose a card index: ";
  let idx = read_int () in
  if idx < 0 || idx >= List.length p.cards then (
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
    let new_p, new_e = player_turn p e in
    game_loop new_p new_e

let () =
  print_endline "Welcome to Caml: The Ultimate!";
  game_loop hero opp
