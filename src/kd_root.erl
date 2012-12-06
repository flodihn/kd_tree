%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_root).

-include("kd_tree.hrl").

% Main loop export
-export([
	handle_message/2
	]).

% API
-export([
	unfold_dimensions/3,
	start_link/1
	]).

start_link(RootState) ->
	RootPid = spawn_link(kd_loop, loop, [kd_root, RootState]),
	{ok, RootPid}.

unfold_dimensions(NodePid, Dims, MaxDepth) ->
    NodePid ! {{unfold_dimensions, Dims}, {max_depth, MaxDepth}}.

% Main loop function
handle_message({trace, {traceback, TraceBack}, {reply_to_pid, ReplyToPid}},
		#kd_root{node_state=NodeState} = RootState) ->
	Median = NodeState#kd_node.median,
	NewTraceBack = {traceback, [{root, Median} | TraceBack]},
    ReplyToPid ! {trace, NewTraceBack},
    {ok, RootState};

handle_message({{unfold_dimensions, Dims}, {max_depth, MaxDepth}},
		#kd_root{node_state=NodeState} = RootState) ->
	[{NextDim, NextDimSize} | _Rest] = Dims,
	Median = kd_util:calculate_median(NextDimSize, root, 0),
	{NewLeft, NewRight} = kd_util:spawn_left_and_right_nodes(),
	kd_node:unfold_dimensions(NewRight, Dims, 1, MaxDepth, Median),
    kd_node:unfold_dimensions(NewLeft, Dims, 1, MaxDepth, Median),
	{ok, RootState#kd_root{node_state=NodeState#kd_node{
		my_dim=root, next_dim=NextDim, left=NewLeft, right=NewRight,
		median=Median, depth=0}}};

handle_message(Message, RootState) ->
	{ok, NewNodeState} = kd_node:handle_message(
		Message,
		RootState#kd_root.node_state),
	{ok, RootState#kd_root{node_state=NewNodeState}}.
