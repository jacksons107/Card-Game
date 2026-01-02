open Core.Types

let attack_2_card = {cost = 1; action_type = Attack (2, 2)}
let block_3_card = {cost = 2; action_type = Block 3}
let power_inc_1_card = {cost = 1; action_type = (Modifier (PowInc 1))}
let power_inc_hand_1 = {cost = 2; action_type = (Modifier (Map ((PowInc 1), 1)))}
let back_1_card = {cost = 4; action_type = Time (Backward 1)}
let clone_1_card = {cost = 2; action_type = Modifier (Clone 1)}
let map_clone_card = {cost = 2; action_type = Modifier (Map ((Clone 1), 1))}
let bag_3_card = {cost = 1; action_type = EmptyBag (3, 1)}

let deck_simple = [attack_2_card;
                   bag_3_card; 
                   block_3_card; 
                   power_inc_1_card; 
                   power_inc_hand_1;
                   back_1_card;
                   clone_1_card]