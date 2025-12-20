open Types
open Cli
open Rules
open Render

let rec game_loop (game : game) = 
    if game.enemies = IntMap.empty then 
       print_endline  "Player wins"
    else if game.player.hp <= 0 then 
        print_endline "Player loses"
    else
        (* Player mana restored *)
        let mana_restored = set_mana game game.player.mana_cap in
        (* Player block reset *)
        let player_block_reset = set_block mana_restored 0 Player in
        (* Enemies set block *)
        let enemies_block_set = 
            let new_enemies = IntMap.map (fun e -> ({e with block = enemy_pick_block e} : enemy)) player_block_reset.enemies in
            {player_block_reset with enemies = new_enemies}
        in
        (* Player draws cards *)
        let cards_drawn = draw_cards enemies_block_set 1 in
        (* Enemies pick action *)
        let enemy_actions = 
            IntMap.fold 
            (fun _ v acc -> acc@[enemy_pick_action v]) 
            cards_drawn.enemies [] 
        in
        (* Player plays their cards *)
        let cards_played = play_cards cards_drawn enemy_actions (Some print_game_state) in
        (* Apply enemey actions *)
        let enemies_acted = apply_enemy_actions cards_played enemy_actions in
        (* Loop *)
        game_loop enemies_acted