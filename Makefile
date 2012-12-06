all:
	@erl -make

test:
	@erl -make
	@erl -noshell -pa ebin -eval 'eunit:test({application, kd_tree}, [verbose, {report, {eunit_surefire, [{dir, "."}]}}])' -s init stop

doc: docs
edoc: docs
docs:
	@erl -noshell -pa ebin -eval 'edoc:files(["src/kd_tree.erl"], [no_packages, {dir, "documentation"}])' -s init stop

clean:
	@rm -rf ebin/*.beam
