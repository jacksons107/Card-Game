module IntMap = Map.Make(Int)


(* === game logic types === *)

type game = {player : player; enemies : enemy IntMap.t}

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


(* === ui-related types === *)

(* card currently selected by the player *)
type card_selection = 
    | Selection of card
    | NoSelection

type game_state = {
    game : game;
    selected : card_selection
}

type interaction = 
    | ClickCard of card
    | ClickPlayer
    | ClickEnemy of int
    | ClickEndTurn

type event = 
    | SelectCard of card
    | TargetPlayer of card
    | TargetEnemy of int * card
    | TargetCard of card * card
    | EndTurn

type hitbox = {
    x : int;
    y : int;
    w : int;
    h : int
}
type ui_element = 
    | CardUI of {card : card; box : hitbox}
    | PlayerUI of {player : player; box : hitbox}
    | EnemyUI of {id : int; box : hitbox}
    | EndTurnUI of {box : hitbox}

type draw_cmd = 
    | DrawCard of {x : int; y : int; w : int; h : int; sel : bool; cost : int; act : action_type}
    | DrawPlayer of {x : int; y : int; w : int; h : int; hp : int; block : int}
    | DrawEnemy of {x : int; y : int; w : int; h : int; hp : int; block : int; act : action_type}
    | DrawEndTurn of {x : int; y : int; w : int; h : int}

type ui_layout = {
    draw_cmds : draw_cmd list;
    ui_elements : ui_element list
}