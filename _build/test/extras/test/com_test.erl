-module(com_test).
-define(DEBUG,true).
-include_lib("eunit/include/eunit.hrl").


start_link_test() ->
    {ok, _Pid} = com:start_link().

stop_test() ->
    ok = com:stop().