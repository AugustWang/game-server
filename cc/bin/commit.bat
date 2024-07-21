@echo off
@set svn_bin=C:\Program Files\TortoiseSVN\bin
@set svn_work=e:\work\code
"%svn_bin%"\TortoiseProc.exe/command:commit /path:"%svn_work%" /logmsg:"" /closeonend:4
