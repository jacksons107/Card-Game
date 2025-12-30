module IntMap = Map.Make(Int)

(* === game logic types === *)

type end_state = Victory | Defeat | Ongoing

type game = {player : player; enemies : enemy IntMap.t; end_state : end_state}

and enemy = {hp : int; block : int; actions : action_type array; block_vals : int array; selected_action : action_type}

and player = {hp : int; mana : int; mana_cap : int; block : int; hand : hand; deck : deck}

and deck = card list

and hand = card list

and card = {id : int; cost : int; action_type : action_type}

(* normal actions can only target players or enemies *)
and action_type = 
    | Attack of int * int
    | Block of int
    | Modifier of modifier_type 
(* modifiers can only target cards *)
and modifier_type = 
    | PowInc of int
    | Map of modifier_type * int

and action = game -> target -> game

(* potential targets of an action *)
and target = Player | Enemy of int | Card of card | Hand | Deck

type target_kind = TKPlayer | TKEnemy | TKCard | TKHand | TKDeck
