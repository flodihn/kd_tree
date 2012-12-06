%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

%% @doc
%% These are functional tests on a tree dimensional tree with 1 level of depth.
%% The traceback format is a bit tricky to understand:
%% Traceback format:
%%	{ok, {traceback, [
%%		{root, <median>}, 
%%		{node, <left|right>, <dimension>, <depth>, <median>},
%%		{leaf, <left|right>, <dimension>, <depth>}
%%	]}}
%%
%% Types:
%% A node can either be a root, node or leaf.
%%
%% Dimension:
%% The dimension is the current dimension that the node is position in.
%%
%% Depth:
%% This is the depth of the node.
%%
%% Median:
%% The median is the value that the node will send any children down to
%% in the next dimension.
%% Medians does not apply to leaves since they attched the children to
%% themselves instead of sending them left or right.
%% @end

-module(kd_three_dim_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_TREE_NAME, test_tree).

setup_three_dim_tree() ->
	{ok, RootPid} = kd_tree:create_tree(
		{tree_name, ?TEST_TREE_NAME},
		{dimensions, [{x, 32}, {y, 32},  {z, 32}]},
		{depth, 1}
	),
	RootPid.

teardown_tree(_RootPid) ->
	{ok, destroyed} = kd_tree:destroy_tree({tree_name, ?TEST_TREE_NAME}).

create_tree_test() ->
	{ok, RootPid} = kd_tree:create_tree(
		{tree_name, create_tree_test},
		{dimensions, [{x, 32}, {y, 32}, {z, 32}]},
		{depth, 1}
	),
	?assert(is_pid(RootPid)).

test_insert_child() ->
	{ok, ChildPid} = kd_tree:insert_child(
		{tree_name, ?TEST_TREE_NAME},
		{dimensions, [{x, 1}, {y, 1}, {z, 1}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	?assert(is_pid(ChildPid)).

test_trace_child1() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, ?TEST_TREE_NAME},
		{dimensions, [{x, 32}, {y, 32}, {z, 32}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0},
			{node, right, x, 1, 16.0},
			{node, right, y, 1, 16.0},
			{leaf, right, z, 1}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child2() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_tree},
		{dimensions, [{x, 0}, {y, 0}, {z, 0}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0},
			{node, left, x, 1, 16.0},
			{node, left, y, 1, 16.0},
			{leaf, left, z, 1}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child3() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_tree},
		{dimensions, [{x, 16.1}, {y, 15.0}, {z, 16.1}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0},
			{node, right, x, 1, 16.0},
			{node, left, y, 1, 16.0},
			{leaf, right, z, 1}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child4() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_tree},
		{dimensions, [{x, -1.0}, {y, -1.0}, {z, 17.0}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0}, 
			{node, left, x, 1, 16.0},
			{node, left, y, 1, 16.0},
			{leaf, right, z, 1}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child5() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_tree},
		{dimensions, [{x, 17.0}, {y, 17.0}, {z, 15.0}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0}, 
			{node, right, x, 1, 16.0},
			{node, right, y, 1, 16.0},
			{leaf, left, z, 1}
		]
	},
	?assertEqual(Expected, TraceBack).

three_dim_test_() ->
	{foreach, fun setup_three_dim_tree/0, fun teardown_tree/1,
		[
			{"Insert Child", fun test_insert_child/0},
			{"Trace Child 1", fun test_trace_child1/0},
			{"Trace Child 2", fun test_trace_child2/0},
			{"Trace Child 3", fun test_trace_child3/0},
			{"Trace Child 4", fun test_trace_child4/0},
			{"Trace Child 5", fun test_trace_child5/0}
		]
	}.
