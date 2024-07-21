@echo off

taskkill /F /IM werl.exe

cd /work/hammer/code/cc

escript gen_protocol.erl

start werl -pa ebin -smp enable -config rel/files/sys -args_file rel/files/vm.args -s cc start

pause