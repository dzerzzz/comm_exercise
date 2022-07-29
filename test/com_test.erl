-module(com_test).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    {ok, _Pid} = com:start_link().

add_user_test() ->
    _Pid = com:add_user(ala),
    "This user already exists" = com:add_user(ala).

send_test() ->
    "Sender doesn't exist." = com:send(andrzej, ala, "siema"),
    com:add_user(ola),
    {message,ala,"siema ola"} = com:send(ala,ola,"siema ola"),
    "Recipient doesn't exist." = com:send(ala, andrzej, "siema").

send_bot_test() ->
    {message,ala,"siema bocie"} = com:send(ala,"siema bocie"),
    receive_next_message = com:receive_next_message(ala),
    "Sender doesn't exist." = com:send(andrzej,"siema").

receive_next_message_test() ->
    "User doesn't exist." = com:receive_next_message(andrzej),
    com:send(ala,ola,"siema ola"),
    receive_next_message = com:receive_next_message(ola).
      
handle_cast_test() ->
    {stop, normal, state} = com:handle_cast(stop, state),
    ok == com:handle_cast(whatever, state).

handle_call_test() ->
    {reply, ok, state} = com:handle_call(request, from, state).

handle_info_test() ->
    {noreply, state} = com:handle_info(info, state).

terminate_test() ->
    ok = com:terminate(reason, state).

code_change_test() ->
    {ok, state} = com:code_change(oldVsn, state, extra).

stop_test() ->
    ok = com:stop().