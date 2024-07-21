-module(data_map).
-export([get/1]).

get(ids) -> [33, 37, 10, 7, 19, 27, 16, 41, 12, 31, 13, 4, 20, 42, 22, 11, 15, 26, 24, 49, 43, 47, 40, 51, 23, 30, 28, 50, 32, 45, 48, 25, 38, 46, 39, 5, 44, 3, 8, 18, 29, 35, 36, 34, 17, 6, 14, 21, 9];

get(33) -> [{id,33},{width,50},{height,36},{brick_num,86}];

get(37) -> [{id,37},{width,50},{height,36},{brick_num,21},{element,[{3,700,0,3,5,3,8},{2,1000,2000,9,5,11,5}]}];

get(10) -> [{id,10},{width,50},{height,36},{brick_num,83},{element,[]}];

get(7) -> [{id,7},{width,50},{height,36},{brick_num,87},{element,[]}];

get(19) -> [{id,19},{width,50},{height,36},{brick_num,50},{element,[]}];

get(27) -> [{id,27},{width,50},{height,36},{brick_num,85}];

get(16) -> [{id,16},{width,50},{height,36},{brick_num,62},{element,[]}];

get(41) -> [{id,41},{width,50},{height,36},{brick_num,48},{element,[]}];

get(12) -> [{id,12},{width,50},{height,36},{brick_num,70},{element,[]}];

get(31) -> [{id,31},{width,50},{height,36},{brick_num,61},{element,[]}];

get(13) -> [{id,13},{width,50},{height,36},{brick_num,84}];

get(4) -> [{id,4},{width,50},{height,36},{brick_num,78}];

get(20) -> [{id,20},{width,50},{height,36},{brick_num,42},{element,[]}];

get(42) -> [{id,42},{width,50},{height,36},{brick_num,30},{element,[{2,800,1800,3,7,3,10},{2,800,1800,12,10,12,7}]}];

get(22) -> [{id,22},{width,50},{height,36},{brick_num,55}];

get(11) -> [{id,11},{width,50},{height,36},{brick_num,64},{element,[]}];

get(15) -> [{id,15},{width,50},{height,36},{brick_num,2},{element,[]}];

get(26) -> [{id,26},{width,50},{height,36},{brick_num,55}];

get(24) -> [{id,24},{width,50},{height,36},{brick_num,85}];

get(49) -> [{id,49},{width,50},{height,36},{brick_num,58},{element,[]}];

get(43) -> [{id,43},{width,50},{height,36},{brick_num,60},{element,[]}];

get(47) -> [{id,47},{width,50},{height,36},{brick_num,33},{element,[]}];

get(40) -> [{id,40},{width,50},{height,36},{brick_num,66},{element,[]}];

get(51) -> [{id,51},{width,50},{height,36},{brick_num,35},{element,[]}];

get(23) -> [{id,23},{width,50},{height,36},{brick_num,85}];

get(30) -> [{id,30},{width,50},{height,36},{brick_num,55}];

get(28) -> [{id,28},{width,50},{height,36},{brick_num,85}];

get(50) -> [{id,50},{width,50},{height,36},{brick_num,35},{element,[]}];

get(32) -> [{id,32},{width,50},{height,36},{brick_num,58},{element,[]}];

get(45) -> [{id,45},{width,50},{height,36},{brick_num,66},{element,[]}];

get(48) -> [{id,48},{width,50},{height,36},{brick_num,22},{element,[]}];

get(25) -> [{id,25},{width,50},{height,36},{brick_num,69}];

get(38) -> [{id,38},{width,50},{height,36},{brick_num,39},{element,[{3,0,0,2,4,2,16}]}];

get(46) -> [{id,46},{width,50},{height,36},{brick_num,41},{element,[]}];

get(39) -> [{id,39},{width,50},{height,36},{brick_num,26},{element,[{2,1000,1800,6,5,9,5},{2,1000,1800,9,11,6,11}]}];

get(5) -> [{id,5},{width,50},{height,36},{brick_num,87}];

get(44) -> [{id,44},{width,50},{height,36},{brick_num,34},{element,[{3,0,0,2,5,2,11},{3,0,0,13,5,13,11}]}];

get(3) -> [{id,3},{width,50},{height,36},{brick_num,62}];

get(8) -> [{id,8},{width,50},{height,36},{brick_num,41}];

get(18) -> [{id,18},{width,50},{height,36},{brick_num,52},{element,[]}];

get(29) -> [{id,29},{width,50},{height,36},{brick_num,69}];

get(35) -> [{id,35},{width,50},{height,36},{brick_num,34}];

get(36) -> [{id,36},{width,50},{height,36},{brick_num,36}];

get(34) -> [{id,34},{width,50},{height,36},{brick_num,60},{element,[]}];

get(17) -> [{id,17},{width,50},{height,36},{brick_num,54},{element,[]}];

get(6) -> [{id,6},{width,50},{height,36},{brick_num,61},{element,[]}];

get(14) -> [{id,14},{width,50},{height,36},{brick_num,54},{element,[]}];

get(21) -> [{id,21},{width,50},{height,36},{brick_num,54},{element,[]}];

get(9) -> [{id,9},{width,50},{height,36},{brick_num,60},{element,[]}];

get(_) -> [].