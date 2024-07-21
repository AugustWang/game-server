@echo off
c:
cd /work/hammer/code/cc
escript ./gen_protocol.erl
rebar compile
pause
