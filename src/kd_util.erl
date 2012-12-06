%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-module(kd_util).

-include("kd_tree.hrl").

-export([
	spawn_left_and_right_nodes/0,
	calculate_median/3
	]).

spawn_left_and_right_nodes() ->
	{ok, NewLeft} = kd_node:start_link(#kd_node{
			parent_pid=self(),
			type=left}),
	{ok, NewRight} = kd_node:start_link(#kd_node{
			parent_pid=self(),
			type=right}),
	{NewLeft, NewRight}.


calculate_median(Median, root, _Depth) ->
	Median / 2;

calculate_median(Median, left, _Depth) ->
	Median - (Median / 2);

calculate_median(Median, right, _Depth) ->
	Median + (Median / 2).
