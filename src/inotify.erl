-module(inotify).
-export([start_port/1, parse/1, check/0]).

check() ->
	case os:find_executable("inotifywait") of
		false -> erlang:error(no_inotifywait_binary_in_search_path);
		Bin -> Bin
	end.

start_port(Paths) ->
	Bin = check(),
	Args = ["-m", "-q", "-e", "close_write", "-e", "moved_to", "-e", "create", "-e", "delete", "-r", Paths],
	erlang:open_port(
		{spawn_executable, Bin},
		[stream, exit_status, {line, 16384}, {args, Args}]
	).

parse(Line) ->
	{match, [Dir, Flags1, DirEntry]} = re:run(Line, re(), [{capture, all_but_first, list}]),
	Flags = [event_map(F) || F <- string:tokens(Flags1, ",")],
	Path = Dir ++ DirEntry,
	{Path, Flags}.

event_map("CREATE") -> create;
event_map("DELETE") -> delete;
event_map("ISDIR") -> isdir;
event_map("CLOSE_WRITE") -> modify;
event_map("CLOSE") -> close;
event_map("MOVED_TO") -> rename;
event_map(_) -> undefined.

re() ->
	case get(inotifywait_re) of
		undefined ->
			{ok, R} = re:compile("^(.*/) ([A-Z_,]+) (.*)$", [unicode]),
			put(inotifywait_re, R),
			R;
		V -> V
	end.