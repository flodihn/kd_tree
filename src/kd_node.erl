%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_node).

-include("kd_tree.hrl").

% Main loop export
-export([
	handle_message/2
	]).

% API
-export([
	start_link/1,
	destroy/1,
	unfold_dimensions/5,
	insert_child/2,
	get_state/1
	]).

start_link(NodeState) ->
	NodePid = spawn_link(kd_loop, loop, [kd_node, NodeState]),
	{ok, NodePid}.

unfold_dimensions(NodePid, Dims, CurrDepth, MaxDepth, Median) ->
	NodePid ! {{unfold_dimensions, Dims}, {curr_depth, CurrDepth},
		{max_depth, MaxDepth}, {median, Median}}.

insert_child(NodePid, ChildState) ->
	NodePid ! {insert_child, {child_state, ChildState},
		{reply_to_pid, self()}},
	receive
		{inserted_child_pid, ChildPid} ->
			{ok, ChildPid}
	after
		?API_TIMEOUT -> {error, timeout}
	end.

get_state(NodePid) ->
	NodePid ! {get_state, {reply_to_pid, self()}},
	receive 
		{node_state, State} -> {ok, State}
	after
		?API_TIMEOUT -> {error, timeout}
	end.

destroy(NodePid) ->
	NodePid ! destroy.

% Internal functions

handle_message({{unfold_dimensions, Dims}, {curr_depth, CurrDepth}, 
		{max_depth, MaxDepth}, {median, Median}}, State) ->
	handle_unfold_dims(State, Dims, CurrDepth, MaxDepth, Median);

handle_message({insert_child, {child_state, ChildState},
		{reply_to_pid, ReplyToPid}}, State) ->
	handle_insert_child(State, ChildState, ReplyToPid);

handle_message({get_state, FromPid}, State) ->
	FromPid ! {node_state, State},
	{ok, State};

handle_message({trace, {traceback, TraceBack}, ReplyToTuple},
		#kd_node{type=Type, my_dim=MyDim, depth=Depth,
		median=Median} = State) ->
	NewTraceBack = {traceback, [{node, Type, MyDim, Depth,
		Median} | TraceBack]},
	State#kd_node.parent_pid ! {trace, NewTraceBack, ReplyToTuple},
	{ok, State};

handle_message(Message, State) ->
	error_logger:error_report({unknown_message, Message}),
	{ok, State}.

% Matches the last depth in the last dimension.
handle_unfold_dims(State, [{CurrDim, _DimSize} | []], CurrDepth, MaxDepth,
		Median) when CurrDepth == MaxDepth ->
	NewMedian = kd_util:calculate_median(Median, State#kd_node.type,
		CurrDepth),
	NewNodeState = State#kd_node{my_dim=CurrDim, depth=CurrDepth,
		median=NewMedian},
	transform_to_leaf(#kd_leaf{node_state=NewNodeState}),
	{ok, State};

% Matches the next depth in the last dimension.
handle_unfold_dims(State, [{CurrDim, DimSize} | []], CurrDepth, MaxDepth,
		Median)  ->
	{NewLeft, NewRight} = kd_util:spawn_left_and_right_nodes(),
	NewMedian = kd_util:calculate_median(Median, State#kd_node.type,
		CurrDepth),
	kd_node:unfold_dimensions(NewRight, [{CurrDim, DimSize}],
		CurrDepth + 1, MaxDepth, NewMedian),
	kd_node:unfold_dimensions(NewLeft, [{CurrDim, DimSize}],
		CurrDepth + 1, MaxDepth, NewMedian),
	{ok, State#kd_node{left=NewLeft, right=NewRight, my_dim=CurrDim,
		depth=CurrDepth, median=NewMedian}};

% Matches the last depth and there is a next dimension.
handle_unfold_dims(State, [{CurrDim, DimSize}, {NextDim, NextDimSize} | Dims],
		CurrDepth, MaxDepth, _Median) when CurrDepth == MaxDepth ->
	{NewLeft, NewRight} = kd_util:spawn_left_and_right_nodes(),
	kd_node:unfold_dimensions(NewRight, [{NextDim, NextDimSize} | Dims],
		1, MaxDepth, DimSize/2),
	kd_node:unfold_dimensions(NewLeft, [{NextDim, NextDimSize} | Dims],
		1, MaxDepth, DimSize/2),
	{ok, State#kd_node{left=NewLeft, right=NewRight, my_dim=CurrDim,
		next_dim=NextDim, depth=CurrDepth, max_depth=MaxDepth,
		median=DimSize/2}};

% Matches the next depth in the current dimension.
handle_unfold_dims(State, [{CurrDim, DimSize}, {NextDim, NextDimSize} | Dims],
		CurrDepth, MaxDepth, Median) ->
	{NewLeft, NewRight} = kd_util:spawn_left_and_right_nodes(),
	NewMedian = kd_util:calculate_median(Median, State#kd_node.type,
		CurrDepth),
	kd_node:unfold_dimensions(NewRight, [{CurrDim, DimSize},
		{NextDim, NextDimSize} | Dims], CurrDepth + 1, MaxDepth, NewMedian),
	kd_node:unfold_dimensions(NewLeft, [{CurrDim, DimSize},
		{NextDim, NextDimSize} | Dims], CurrDepth + 1, MaxDepth, NewMedian),
	{ok, State#kd_node{left=NewLeft, right=NewRight, my_dim=CurrDim,
		next_dim=NextDim, depth=CurrDepth, max_depth=MaxDepth,
		median=NewMedian}}.

handle_insert_child(#kd_node{my_dim=root, median=Median} = State,
		ChildState, ReplyToPid) ->
	insert_child_left_or_right(State#kd_node.next_dim, Median,
		State, ChildState, ReplyToPid),
	{ok, State};

handle_insert_child(#kd_node{depth=Depth, max_depth=MaxDepth,
		median=Median} = State, ChildState, ReplyToPid) 
		when Depth == MaxDepth ->
	insert_child_left_or_right(State#kd_node.next_dim, Median,
		State, ChildState, ReplyToPid),
	{ok, State};

handle_insert_child(#kd_node{median=Median} = State, ChildState,
		ReplyToPid) ->
	insert_child_left_or_right(State#kd_node.my_dim, Median,
		State, ChildState, ReplyToPid),
	{ok, State}.

insert_child_left_or_right(Dim, Median, State, ChildState, ReplyToPid) ->
	{_DimName, Pos} = lists:keyfind(Dim, 1,
		ChildState#kd_child.dimensions),
	InsertChildMessage = {insert_child, {child_state, ChildState},
		{reply_to_pid, ReplyToPid}},
	case Pos =< Median of
		true ->
			State#kd_node.left ! InsertChildMessage;
		false ->
			State#kd_node.right ! InsertChildMessage
	end.

transform_to_leaf(LeafState) ->
	self() ! {transform, {new_module, kd_leaf}, {new_state, LeafState}}.
