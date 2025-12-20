open Types

(* A simple enemy *)
let enemy_simple = {hp = 10;  
                    block = 0; 
                    actions = [|Attack (2, 1); Attack(1, 1)|];
                    block_vals = [|0; 1|];
                    selected_action = Attack(1, 1)}