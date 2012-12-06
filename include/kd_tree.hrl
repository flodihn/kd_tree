%%----------------------------------------------------------------------------
%% @author christian@flodihn.se
%% @copyright Christian Flodihn

-record(kd_tree, {root, dimensions}).
-record(kd_root, {dimensions, node_state}).
-record(kd_node, {type, depth, max_depth, median, left, right, parent_pid,
	my_dim, next_dim}).
-record(kd_leaf, {children=[], node_state}).
-record(kd_child, {parent, remote_pid, dimensions, user_data}).

-define(API_TIMEOUT, 1000).
