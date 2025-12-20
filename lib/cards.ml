open Types

let attack_2_card = {id = 0; cost = 1; action_type = Attack (2, 2)}
let block_3_card = {id = 1; cost = 2; action_type = Block 3}
let power_inc_1_card = {id = 2; cost = 1; action_type = (Modifier (PowInc 1))}
let power_inc_hand_1 = {id = 3; cost = 2; action_type = (Modifier (Map ((PowInc 1), 1)))}
(* TODO tempate map card? *)