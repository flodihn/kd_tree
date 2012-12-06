%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn


-module(kd_tree).

-include("kd_tree.hrl").

-export([
	create_tree/3,
	destroy_tree/1,
	insert_child/4,
	insert_and_trace_child/4
	]).

%%----------------------------------------------------------------------------
%% @spec create_tree(TreeName::{tree_name, TreeName::atom()},
%%	{dimension, Dimensions::list()}, {depth, Depth::integer()}) -> 
%%	{ok, RootPid::pid()} | {error, name_taken}
%% @doc
%% Creates a new tree. A unique name, a list of dimensions and the 
%% depth of the tree must be supplied.
%%
%% The Dimensions is a list of tuples with the name and size of the
%% dimension, example: [{x, 32}, {y, 32}, {z, 32}].
%%
%% Note:
%% All dimensions does not need to have same size, but this behaviour is
%% not yet tested.
%%
%% A depth of 1 means that the dimension will only be split in one
%% right and left node, for each depth, each left and right node will
%% be split once more. This means that with a depth of 2, a dimension
%% will have 4 leaf nodes in the bottom. If the depth is 3, the
%% dimension will have 8 leaf nodes and so on.
%%
%% @end
%%----------------------------------------------------------------------------
create_tree({tree_name, TreeName}, {dimensions, Dimensions},
		{depth, MaxDepth}) when is_atom(TreeName) ->
	case whereis(TreeName) of
		undefined ->
			{ok, RootPid} = kd_root:start_link(
				#kd_root{
					dimensions=Dimensions,
					node_state=#kd_node{
						type=root}}),
			register(TreeName, RootPid),
			kd_root:unfold_dimensions(RootPid, Dimensions, MaxDepth),
			{ok, RootPid};
		_Pid ->
			{error, name_taken}
	end;

create_tree(_InvalidTreename, _Dimensions, _Depth) ->
	{error, tree_name_must_be_atom}.

%%----------------------------------------------------------------------------
%% @spec destroy_tree(TreeName::{tree_name, TreeName::atom()}) -> 
%%	{ok, destroyed} | {error, tree_not_found}
%% @doc
%% Destroyes a tree. This will exit the root node, since all nodes are
%% linked in a hierarchy, the whole tree should go down.
%% @end
%%----------------------------------------------------------------------------
destroy_tree({tree_name, TreeName}) when is_atom(TreeName) ->
	case get_root_pid(TreeName) of
		{ok, RootPid} ->
			% Since all processes are linked with their parents, killing 
			% the root node should kill the whole tree.
			kd_node:destroy(RootPid),
			unregister(TreeName),
			{ok, destroyed};
		undefined ->
			{error, tree_not_found}
	end;

destroy_tree(_InvalidTreename) ->
	{error, tree_name_must_be_atom}.

%%----------------------------------------------------------------------------
%% @spec insert_and_trace_child(TreeName::{tree_name, TreeName::atom()},
%%	{dimension, Dimensions::list()}, UserData::binary(), RemotePid::pid()) -> 
%%	{ok, ChildPid, TraceBack}
%% @doc
%% Inserts a child into the tree and traces its parents back to the root
%% node.
%% @end
%%----------------------------------------------------------------------------
insert_and_trace_child(TreeName, Dimensions, UserData, RemotePid) ->
	{ok, ChildPid} = insert_child(TreeName, Dimensions, UserData, RemotePid),
	{ok, TraceBack} = kd_child:trace(ChildPid),
	{ok, ChildPid, TraceBack}.

%%----------------------------------------------------------------------------
%% @spec insert_child(TreeName::{tree_name, TreeName::atom()},
%%	{dimension, Dimensions::list()}, UserData::binary(), RemotePid::pid()) -> 
%%	{ok, ChildPid}
%% @doc
%% Inserts a child into the tree without tracing back to the root pid.
%% @end
%%----------------------------------------------------------------------------
insert_child({tree_name, TreeName}, {dimensions, Dimensions},
		{user_data, UserData}, {remote_pid, RemotePid}) ->
	{ok, RootPid} = get_root_pid(TreeName),
	ChildState = #kd_child{
		dimensions=Dimensions,
		user_data=UserData,
		remote_pid=RemotePid
	},
	kd_node:insert_child(RootPid, ChildState).

%% @hidden
get_root_pid(TreeName) when is_atom(TreeName) ->
	case whereis(TreeName) of
		undefined ->
			{error, tree_not_found};
		RootPid ->
			{ok, RootPid}
	end;

get_root_pid(_TreeName) ->
	{error, tree_name_must_be_atom}.
