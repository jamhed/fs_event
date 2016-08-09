devel:
	rm -rf _rel
	rebar get-deps
	rebar compile
	relx -d

console: devel
	_rel/fs_event/bin/fs_event console
