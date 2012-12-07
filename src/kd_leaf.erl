%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_leaf).

-include("kd_tree.hrl").

% Main loop export
-export([
	handle_message/2
	]).

% API
-export([
	start_link/1,
	get_state/1
	]).

start_link(LeafState) ->
	LeafPid = spawn_link(kd_loop, loop, [kd_leaf, LeafState]),
	{ok, LeafPid}.

get_state(LeafPid) ->
	kd_node:get_state(LeafPid).

% Main loop function
handle_message({insert_child, {child_state, ChildState},
		{reply_to_pid, ReplyToPid}}, State) ->
	{ok, ChildPid} = kd_child:start_link(
		ChildState#kd_child{parent=self()}),
	ReplyToPid ! {inserted_child_pid, ChildPid},
	NewChildren = lists:append(State#kd_leaf.children, [ChildPid]),
	{ok, State#kd_leaf{children=NewChildren}};

handle_message({trace, {traceback, TraceBack}, ReplyToTuple},
		#kd_leaf{node_state=NodeState} = State) ->
	Parent = NodeState#kd_node.parent_pid,
	DimName = NodeState#kd_node.my_dim,
	Depth = NodeState#kd_node.depth,
	Type = NodeState#kd_node.type,
    NewTraceBack = {traceback, [{leaf, Type, DimName,
		Depth} | TraceBack]},
    Parent ! {trace, NewTraceBack, ReplyToTuple},
    {ok, State};

handle_message(Message, State) ->
	{ok, NewNodeState} = kd_node:handle_message(
		Message,
		State#kd_leaf.node_state),
	{ok, State#kd_leaf{node_state=NewNodeState}}.
