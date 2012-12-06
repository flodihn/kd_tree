{application, kd_tree,
	[
		{description, "The K Dimensional Tree"},
		{vsn, "1"},
		{modules, [
			kd_tree,
			kd_root,
			kd_node,
			kd_leaf,
			kd_child,
			kd_loop,
			kd_util,
			kd_three_dim_test,
			kd_depth_test
		]},
		{registered, []},
		{applications, [kernel, stdlib]},
		{mod, {kd_tree, []}}
	 ]
}.
