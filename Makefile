devel:
	rm -rf _build
	rebar3 release

console: devel
	_build/default/rel/fs_event/bin/fs_event console
