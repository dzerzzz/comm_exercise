-module(com).
-behaviour(gen_server).
-define(SERVER, com).
%% API
-export([start_link/0, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_user/1, send/2, send/3, receive_next_message/1]).

start_link() ->
    Server = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    io:format("Communicator server started~n"),
    Server.

stop() ->
    gen_server:cast(?SERVER, stop),
    io:format("Communicator server closed~n").

init([]) ->
    PidBot = spawn(fun chatbot_loop_init/0),
    register(bot, PidBot),
    {ok, {}}.

add_user(Username) ->
    case whereis(Username) of
        undefined ->
            Pid = spawn(fun clients_loop_init/0),
            register(Username, Pid),
            io:format("Hello ~p on the server ~n", [Username]),
            Pid;
        _Pid -> 
            "This user already exists"
    end.
    
clients_loop(#{inbox := Inbox} = State) ->
    receive
        {message, Sender, Message} ->
            NewInbox = Inbox ++ [{Sender, Message}],
            clients_loop(#{inbox => NewInbox});

        receive_next_message ->
            case Inbox of
                [] -> 
                    io:format("Inbox is empty ~n"),
                    clients_loop(State);
                _ ->
                    [{Sender, Message}|Rest] = Inbox,
                    io:format("From: ~p Message: ~p ~n", [Sender, Message]),
                    clients_loop(#{inbox => Rest})
            end
    end.

clients_loop_init() ->
    clients_loop(#{inbox=>[]}).

chatbot_loop_init() ->
    chatbot_loop([]).

chatbot_loop(State) ->
    receive
        {message, Sender, Message} ->
            Sign = string:right(Message,1),
            Response =  if
                    Message == "How are you?" -> "Sure.";
                    Sign == "?" -> "Oddalam to pytanie";
                    Message == [] -> "Aha aha";
                    true ->
                        "Whatever."
                end,
            PidSender = whereis(Sender),
            PidSender ! {message, bot, Response},
            chatbot_loop(State)
    end.

send(Sender, To, Message)->
    case whereis(Sender) of
        undefined ->
            "Sender doesn't exist.";
        _ ->
            case whereis(To) of
                undefined ->
                    "Recipient doesn't exist.";
                Pid ->
                    Pid ! {message, Sender, Message}
            end
    end.

send(Sender, Message)->
    case whereis(Sender) of
        undefined ->
            "Sender doesn't exist.";
        _ ->
            Pid = whereis(bot),
            Pid ! {message, Sender, Message}
    end.

receive_next_message(Username) ->
    case whereis(Username) of
        undefined ->
            "User doesn't exist.";
        Pid ->
            Pid ! receive_next_message
    end.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    io:format("Default cast.~n"),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


