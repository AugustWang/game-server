-module(data_config_ai).
-export([get/1]).
-include("rs.hrl").
get(1) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[200,300], find_enemy=0, find_npc=0, leave_enemy=0, use_daoju=0};
get(2) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[195,295], find_enemy=0, find_npc=0, leave_enemy=0, use_daoju=0};
get(3) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[190,290], find_enemy=0, find_npc=0, leave_enemy=0, use_daoju=0};
get(4) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[185,285], find_enemy=0, find_npc=0, leave_enemy=0, use_daoju=0};
get(5) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[180,280], find_enemy=0, find_npc=0, leave_enemy=0, use_daoju=0};
get(6) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[175,275], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=0};
get(7) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[170,270], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=0};
get(8) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[165,265], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=0};
get(9) -> #ai{hit_front=100, hit_back=0, hit_up=0, hit_down=0, hit_reaction=[160,260], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=0};
get(10) -> #ai{hit_front=100, hit_back=100, hit_up=0, hit_down=0, hit_reaction=[155,255], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=0};
get(11) -> #ai{hit_front=100, hit_back=100, hit_up=0, hit_down=0, hit_reaction=[150,250], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(12) -> #ai{hit_front=100, hit_back=100, hit_up=0, hit_down=0, hit_reaction=[140,240], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(13) -> #ai{hit_front=100, hit_back=100, hit_up=0, hit_down=0, hit_reaction=[130,230], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(14) -> #ai{hit_front=100, hit_back=100, hit_up=0, hit_down=0, hit_reaction=[120,220], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(15) -> #ai{hit_front=100, hit_back=100, hit_up=0, hit_down=0, hit_reaction=[110,210], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(16) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[100,200], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(17) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[90,190], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(18) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[80,180], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(19) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[70,170], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(20) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[60,160], find_enemy=0, find_npc=0, leave_enemy=100, use_daoju=100};
get(21) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[50,150], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(22) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[40,140], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(23) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[30,130], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(24) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[20,120], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(25) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[10,110], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(26) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,100], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(27) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,90], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(28) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,80], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(29) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,70], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(30) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,60], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(31) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,50], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(32) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,40], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(33) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,30], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(34) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,20], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(35) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,10], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(36) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,0], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(37) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,0], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(38) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,0], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(39) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,0], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(40) -> #ai{hit_front=100, hit_back=100, hit_up=100, hit_down=100, hit_reaction=[0,0], find_enemy=100, find_npc=100, leave_enemy=100, use_daoju=100};
get(_) -> #ai{}.