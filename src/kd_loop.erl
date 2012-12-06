%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_loop).

% API
-export([
    loop/2
    ]). 

-define(TIMEOUT, 2000).

loop(Module, State) ->
    receive
		destroy ->
			exit(normal);
		{transform, {new_module, NewModule}, {new_state, NewState}} ->
            loop(NewModule, NewState);
        Message ->
            {ok, NewState} = Module:handle_message(Message, State),
            loop(Module, NewState)
    after ?TIMEOUT ->
        loop(Module, State)
    end.
