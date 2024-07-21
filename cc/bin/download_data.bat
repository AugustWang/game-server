@echo off

rem Ë½³×Â·¾¶
@set ppk_path=/work/tools/key/rolong.ppk
@set code_path=/work/hammer/code

pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/cc/src/data/data* %code_path%/cc/src/data/
pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/web/game/Main*    %code_path%/web/game/
pause
