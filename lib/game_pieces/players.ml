open Core.Types
(* open Cards *)

(* A simple player *)
let player_simple = {hp = 10;  
                    mana = 0;
                    mana_cap = 5;
                    block = 0; 
                    hand = [];
                    deck = []}