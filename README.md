# This is a K-Dimensional binary tree implemented in Erlang #
Version 0.1 (prototype stage)

TODO:
* Add a queue to handle messages arriving to unfolded nodes (will allow removal of the sleep statements in the functional tests).
* Add a feature for children to send events up in the tree with a specified range.
* Add distribution.
* Add more functional test.
* Add unit tests.

To compile:
	make

To test:
	make test

Generate documentation:
	make doc
