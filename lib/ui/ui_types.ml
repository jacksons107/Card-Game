open Core.Types

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
    | ClickHand
    | ClickDeck
    | ClickGame
    | ClickEndTurn
    | ClickNothing

(* first card in Target constructors is the card that is doing the targeting *)
type event = 
    | SelectCard of card
    | TargetPlayer of card
    | TargetEnemy of int * card
    | TargetCard of card * card
    | TargetHand of card
    | TargetDeck of card
    | TargetGame of card
    | Unselect
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
    | HandUI of {box : hitbox}
    | DeckUI of {box : hitbox}
    | GameButtonUI of {box : hitbox}
    | EndTurnUI of {box : hitbox}

type draw_cmd = 
    | DrawCard of {x : int; y : int; w : int; h : int; sel : bool; hil : bool; cost : int; act : action_type}
    | DrawPlayer of {x : int; y : int; w : int; h : int; hil : bool; hp : int; block : int}
    | DrawEnemy of {x : int; y : int; w : int; h : int; hil : bool; hp : int; block : int; act : action_type}
    | DrawHand of {x : int; y : int; w : int; h : int; hil : bool}
    | DrawEndTurn of {x : int; y : int; w : int; h : int}
    | DrawManaBar of {x : int; y : int; w : int; h : int; remain : int; cap : int}
    | DrawDeck of {x : int; y : int; w : int; h : int; hil : bool; size : int}
    | DrawGameButton of {x : int; y : int; w : int; h : int; hil : bool}
    | DrawVictoryScreen of {x : int; y : int}
    | DrawDefeatScreen of {x : int; y : int}

type ui_layout = {
    draw_cmds : draw_cmd list;
    ui_elements : ui_element list
}