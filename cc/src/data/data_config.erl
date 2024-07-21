-module(data_config).
-export([get/1]).
get(init_hp_max) -> 60;
get(init_dmg) -> 3;
get(init_move_speed) -> 1000;
get(int_client_move_speed) -> 0.13;
get(init_dmg_speed) -> 1000;
get(init_client_dmg_speed) -> 105;
get(init_attack_recover) -> 810;
get(init_gold) -> 2000;
get(init_card) -> 0;
get(init_items) -> 110001;
get(re_alive_time) -> 4200;
get(break_interval_time) -> 3000;
get(break_recover_time) -> 6000;
get(chat_interval_time) -> 5000;
get(init_image_female) -> 1001;
get(init_image_male) -> 2001;
get(npc_num_1v1) -> 30;
get(npc_num_2v2) -> 30;
get(npc_num_3v3) -> 30;
get(npc_num_4v4) -> 30;
get(fall_score) -> 5;
get(move_speed_max) -> 1600;
get(move_speed_min) -> 400;
get(dmg_speed_max) -> 1600;
get(dmg_speed_min) -> 500;
get(ghost_start) -> 1200;
get(game_time) -> 120;
get(mode2_game_time) -> 120;
get(mode2_npc_interval_time) -> 30;
get(mode2_npc_num_1v1) -> 10;
get(mode2_npc_num_2v2) -> 15;
get(mode2_npc_num_3v3) -> 20;
get(mode2_npc_num_4v4) -> 25;
get(fall_time) -> 7000;
get(change_gold) -> 1000;
get(confirm_gold) -> 1000;
get(drop_lv) -> [{110,{1,10}},{1115,{11,15}},{1620,{16,20}},{2125,{21,25}},{2640,{26,40}}];
get(strengthen_reduce_lv) -> 6;
get(strengthen_value) -> [{170001,10},{170002,33},{170003,102},{170004,310}];
get(Key) ->
    io:format("\n[~w:~w] Key(~w) undefined!\n", [?MODULE, ?LINE, Key]),
    undefined.