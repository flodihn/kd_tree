%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_depth_test).

-include_lib("eunit/include/eunit.hrl").

setup_three_dim_tree_with_depth() ->
	{ok, RootPid} =  kd_tree:create_tree(
		{tree_name, test_depth_tree},
		{dimensions, [{x, 32}, {y, 32}, {z, 32}]},
		{depth, 2}
	),
	RootPid.

teardown_tree(_TreeName) ->
	{ok, destroyed} = kd_tree:destroy_tree({tree_name, test_depth_tree}).

test_insert_child() ->
	timer:sleep(100),
	{ok, ChildPid} = kd_tree:insert_child(
		{tree_name, test_depth_tree},
		{dimensions, [{x, 1}, {y, 1}, {z, 1}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	?assert(is_pid(ChildPid)).

test_trace_child1() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_depth_tree},
		{dimensions, [{x, 0.0}, {y, 32.0}, {z, 0.0}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0},
			{node, left, x, 1, 8.0},
			{node, left, x, 2, 16.0},
			{node, right, y, 1, 24.0},
			{node, right, y, 2, 16.0},
			{node, left, z, 1, 8.0},
			{leaf, left, z, 2}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child2() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_depth_tree},
		{dimensions, [{x, 0.0}, {y, 32.0}, {z, 0.0}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0}, 
			{node, left, x, 1, 8.0},
			{node, left, x, 2, 16.0},
			{node, right, y, 1, 24.0},
			{node, right, y, 2, 16.0},
			{node, left, z, 1, 8.0},
			{leaf, left, z, 2}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child3() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_depth_tree},
		{dimensions, [{x, 16.1}, {y, 7.0}, {z, 7.9}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0}, 
			{node, right, x, 1, 24.0},
			{node, left, x, 2, 16.0},
			{node, left, y, 1, 8.0},
			{node, left, y, 2, 16.0},
			{node, left, z, 1, 8.0},
			{leaf, left, z, 2}
		]
	},
	?assertEqual(Expected, TraceBack).

test_trace_child4() ->
	timer:sleep(100),
	{ok, _ChildPid, TraceBack} = kd_tree:insert_and_trace_child(
		{tree_name, test_depth_tree},
		{dimensions, [{x, 32.1}, {y, 8.1}, {z, 7.9}]},
		{user_data, <<"TestChild">>},
		{remote_pid, undefined}
	),
	Expected = {
		traceback, [
			{root, 16.0}, 
			{node, right, x, 1, 24.0},
			{node, right, x, 2, 16.0},
			{node, left, y, 1, 8.0},
			{node, right, y, 2, 16.0},
			{node, left, z, 1, 8.0},
			{leaf, left, z, 2}
		]
	},
	?assertEqual(Expected, TraceBack).


three_dim_test_() ->
	{foreach, fun setup_three_dim_tree_with_depth/0, fun teardown_tree/1,
		[
			{"Insert Child", fun test_insert_child/0},
			{"Trace Child 1", fun test_trace_child1/0},
			{"Trace Child 2", fun test_trace_child2/0},
			{"Trace Child 3", fun test_trace_child3/0},
			{"Trace Child 4", fun test_trace_child4/0}
		]
	}.
