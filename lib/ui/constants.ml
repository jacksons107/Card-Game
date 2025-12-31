(* constants for the drawing cards *)
let card_w = 120
let card_h = 180
let hand_x = 300
let hand_x_spacing i = (hand_x + 50) + i * 140
let hand_y = 500

(* constants for drawing player *)
let player_w = 120
let player_h = 120
let player_x = 100
let player_y = 250

(* constants for drawing enemies *)
let enemy_w = 120
let enemy_h = 120
let enemy_x = 1100
let enemy_y = 150

let enemy_y_spacing i = (enemy_y + 50) + i * 140

(* constants for drawing end turn button *)
let end_turn_x = 1000
let end_turn_y = 600
let end_turn_w = 120
let end_turn_h = 50

(* constants for drawing mana bar *)
let mana_bar_x = 100
let mana_bar_y = 50
let mana_bar_w = 100
let mana_bar_h = 50

(* constants for drawing deck *)
let deck_x = 100
let deck_y = 550
let deck_w = 50
let deck_h = 50

(* constants for drawing target game button *)
let game_button_x = 540
let game_button_y = 360
let game_button_w = 120
let game_button_h = 50

(* constants for drawing end state screens *)
let end_message_x = 540
let end_message_y = 360