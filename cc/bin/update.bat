@echo off
@set svn_bin=c:\Program Files\TortoiseSVN\bin
@set svn_work=e:\work\code
"%svn_bin%"\TortoiseProc.exe/command:update /path:"%svn_work%" /notempfile /closeonend:4
@set svn_work=e:\work\doc
"%svn_bin%"\TortoiseProc.exe/command:update /path:"%svn_work%" /notempfile /closeonend:4
rem Ë½³×Â·¾¶
@set ppk_path=/work/tools/key/rolong.ppk
@set code_path=\work\code

pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/cc/src/data/data* %code_path%/cc/src/data/
pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/web/game/Main*    %code_path%/web/game/
rem pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/cc/src/pack.erl   %code_path%/cc/src/
rem pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/cc/src/unpack.erl %code_path%/cc/src/
