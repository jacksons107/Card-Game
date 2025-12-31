open Core.Types

let attack_2_card = {cost = 1; action_type = Attack (2, 2)}
let block_3_card = {cost = 2; action_type = Block 3}
let power_inc_1_card = {cost = 1; action_type = (Modifier (PowInc 1))}
let power_inc_hand_1 = {cost = 2; action_type = (Modifier (Map ((PowInc 1), 1)))}
let back_1_template = {cost = 4; action_type = Modifier (BackTemplate (1, 1))}

let deck_simple = [attack_2_card; 
                   block_3_card; 
                   power_inc_1_card; 
                   power_inc_hand_1;
                   back_1_template]