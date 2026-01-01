module IntMap = Map.Make(Int)

(* === game logic types === *)

type end_state = Victory | Defeat | Ongoing

type game = {player : player; enemies : enemy IntMap.t; end_state : end_state; 
             timeline : game array; time_idx : int; next_card_id : int}

and enemy = {hp : int; block : int; actions : action_type array; block_vals : int array; selected_action : action_type}

and player = {hp : int; mana : int; mana_cap : int; block : int; hand : hand; deck : deck}

and deck = card list

and hand = card list

and card = {id : int; cost : int; action_type : action_type}

and card_template = {cost : int; action_type : action_type}

(* normal actions can only target players or enemies *)
and action_type = 
    | Attack of int * int (* damage and number of times *)
    | Block of int (* amount of block *)
    | EmptyBag of int * int (* number of slots in bag and cost of generated full bag *)
    | FullBag of card list
    | Modifier of modifier_type 
    | Time of time_type
(* modifiers can only target cards *)
and modifier_type = 
    | PowInc of int (* amount to increase power by *)
    | Map of modifier_type * int (* modifier to apply and number of times to apply *)
    | Clone of int (* number of clones to make *)
    | BackTemplate of int * int (* number of turns time travel card that results from template will go and cost of generated card *)
(* TODO do we actually need this separate type? *)
and time_type = 
    | Backward of card * int (* card to carry with and number of turns back *)

and action = game -> target -> game

(* potential targets of an action *)
and target = Player | Enemy of int | Card of card | CardGroup of card list | Hand | Deck | Game

type target_kind = TKPlayer | TKEnemy | TKCard | TKHand | TKDeck | TKGame
