open Core.Types
open Cards

(* A simple player *)
let player_simple = {hp = 10;  
                    mana = 0;
                    mana_cap = 5;
                    block = 0; 
                    hand = [attack_2_card; block_3_card];
                    deck = [power_inc_hand_1; power_inc_1_card]}