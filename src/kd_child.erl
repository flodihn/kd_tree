%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_child).

-include("kd_tree.hrl").

% API
-export([
	start_link/1,
	trace/1
	]).

% Main loop export
-export([
	handle_message/2
	]).

start_link(State) ->
	ChildPid = spawn_link(kd_loop, loop, [kd_child, State]),
	{ok, ChildPid}.

trace(ChildPid) ->
	ChildPid ! {trace, {reply_to_pid, self()}},
	receive
		{trace, TraceBack} ->
			{ok, TraceBack}
	after 
		?API_TIMEOUT -> {error, timeout}
	end.

% Main loop function
handle_message({trace, {reply_to_pid, ReplyToPid}}, State) ->
	Parent = State#kd_child.parent,
	Parent ! {trace, {traceback, []}, {reply_to_pid, ReplyToPid}},
	{ok, State};

handle_message(Message, State) ->
	error_logger:info_report({unknown_message, Message}),
	{ok, State}.
