@echo off

rem Ë½³×Â·¾¶
@SET /p kw=Please input file keyword:
@set ppk_path=D:/cygwin/work/tools/key/rolong.ppk
@set code_path=D:/cygwin\work\code


pscp.exe -2 -4 -i "%ppk_path%" root@58.61.153.166:/home/dev/cc/src/data/data_%kw%.erl %code_path%/cc/src/data/