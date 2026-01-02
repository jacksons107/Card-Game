open Ui_types
open String_utils
open Raylib


let wrap_text ~font ~font_size ~spacing ~text ~max_width =
    let words = String.split_on_char ' ' text in
    let measure s =
        Vector2.x (measure_text_ex font s font_size spacing)
    in
    let rec build_lines words current_line acc =
        match words with
        | [] ->
            List.rev (current_line :: acc)
        | w :: ws ->
            let candidate =
            if current_line = "" then w else current_line ^ " " ^ w
            in
            if measure candidate <= max_width then
            build_lines ws candidate acc
            else
            build_lines ws w (current_line :: acc)
    in
    match words with
    | [] -> []
    | w :: ws -> build_lines ws w []

let draw_wrapped_text ~font ~font_size ~spacing ~x ~y ~w ~h ~color text =
    let line_height = font_size +. spacing in
    let max_lines = h / int_of_float line_height in
    wrap_text ~font ~font_size ~spacing ~text ~max_width:(float_of_int w)
    |> List.take max_lines
    |> List.iteri (fun i line ->
        draw_text_ex
            font
            line
            (Vector2.create
                (float_of_int x)
                (float_of_int y +. float_of_int i *. line_height))
            font_size
            spacing
            color)

let draw_card x y w h sel hil in_group cost act =
    (if sel then
        draw_rectangle x y w h Color.green
    else if hil then
        draw_rectangle x y w h Color.gold
    else if in_group then
        draw_rectangle x y w h Color.purple
    else
        draw_rectangle x y w h Color.lightgray);
    draw_rectangle_lines x y w h Color.darkgray;
    draw_text (string_of_int cost) (x + w - 20) (y + 10) 20 Color.blue;
    let action_string = string_of_action_type act in
    let font = get_font_default () in
    let font_size = 12. in
    let spacing = 1. in
    draw_wrapped_text
        ~font
        ~font_size
        ~spacing
        ~x:(x + 10)
        ~y:(y + 50)
        ~w:(w - 20)
        ~h:(h - 40)
        ~color:Color.black
        action_string

let draw_player x y w h hil hp block = 
    (if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.blue);
    draw_text (string_of_int hp) (x + 10) (y + 10) 20 Color.white;
    draw_text (string_of_int block) (x + w - 20) (y + 10) 20 Color.white

let draw_enemy x y w h hil hp block act = 
    (if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.red);
    draw_text (string_of_int hp) (x + 10) (y + 10) 20 Color.black;
    draw_text (string_of_int block) (x + w - 20) (y + 10) 20 Color.black;
    let action_string = string_of_action_type act in
    (* draw_text action_string (x + 15) (y + h/2) 5 Color.black *)
    let font = get_font_default () in
    let font_size = 12. in
    let spacing = 1. in
    draw_wrapped_text
        ~font
        ~font_size
        ~spacing
        ~x:(x + 10)
        ~y:(y + 50)
        ~w:(w - 20)
        ~h:(h - 40)
        ~color:Color.black
        action_string

let draw_hand x y w h hil = 
    if hil then
        draw_rectangle x y w h Color.gold

let draw_deck x y w h hil size = 
    (if hil then
        draw_rectangle x y w h Color.gold
    else
        draw_rectangle x y w h Color.darkgray);
    draw_text (Printf.sprintf "%d" size) (x + 10) (y + 10) 20 Color.black
    

let draw_end_turn x y w h = 
    draw_rectangle x y w h Color.darkbrown;
    draw_text "End Turn" (x + 10) (y + 10) 20 Color.white

let draw_game_button x y w h hil = 
    if hil then begin
        draw_rectangle x y w h Color.gold;
        draw_text "Play Card" (x + 10) (y + 10) 20 Color.black
    end

let draw_mana_bar x y w h remain cap = 
    draw_rectangle x y w h Color.blue;
    draw_text (Printf.sprintf "%d / %d" remain cap) (x + 10) (y + 10) 20 Color.white

let draw_group_remaining x y rem =
    draw_text (Printf.sprintf "Select %d card(s)" rem) x y 30 Color.black

let draw_bag_conf_button x y w h = 
    draw_rectangle x y w h Color.gold;
    draw_text "Confirm Selection" (x + 10) (y + 10) 20 Color.black

let draw_victory x y =  
    draw_text "Victory" x y 50 Color.blue

let draw_defeat x y =  
    draw_text "Defeat" x y 50 Color.red

let draw_layout layout = 
    let draw_cmd cmd = 
        match cmd with
            | DrawCard c ->
                draw_card c.x c.y c.w c.h c.sel c.hil c.in_group c.cost c.act
            | DrawPlayer p ->
                draw_player p.x p.y p.w p.h p.hil p.hp p.block
            | DrawEnemy e ->
                draw_enemy e.x e.y e.w e.h e.hil e.hp e.block e.act
            | DrawHand h ->
                draw_hand h.x h.y h.w h.h h.hil
            | DrawDeck d ->
                draw_deck d.x d.y d.w d.h d.hil d.size
            | DrawEndTurn b ->
                draw_end_turn b.x b.y b.w b.h
            | DrawGameButton g ->
                draw_game_button g.x g.y g.w g.h g.hil
            | DrawSelConfButton c ->
                draw_bag_conf_button c.x c.y c.w c.h
            | DrawGroupRemaining r ->
                draw_group_remaining r.x r.y r.rem
            | DrawManaBar m ->
                draw_mana_bar m.x m.y m.w m.h m.remain m.cap
            | DrawVictoryScreen s ->
                draw_victory s.x s.y
            | DrawDefeatScreen s ->
                draw_defeat s.x s.y
    in
    List.iter draw_cmd layout.draw_cmds