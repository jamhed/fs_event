-module(naive).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("kernel/include/file.hrl").
-behaviour(gen_server).

-export([start/1, stop/1, parse/1, check/0]).

-record(state, {
	path,
	caller :: pid(),
	rescan_period,
	tree = gb_trees:empty() :: gb_trees:tree()
}).

start(Path) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [self(), Path, fs_event_cfg:rescan_period()], []),
	Pid.

stop(Pid) ->
	gen_server:stop(Pid).

rescan(Pid) ->
	gen_server:cast(Pid, rescan).

init([CallerPid, Path, Time]) -> 
	erlang:send_after(Time, self(), timer),
	{ok, #state{
		caller = CallerPid,
		path = Path,
		rescan_period = Time,
		tree = make_tree(fold(Path))
	}}.

handle_cast(rescan, S=#state{caller=Pid, path=Path, tree=Tree}) ->
	{noreply, S#state{tree=rescan(Pid, Tree, fold(Path))}};
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

handle_info(timer, S=#state{rescan_period=Time}) ->
	rescan(self()),
	erlang:send_after(Time, self(), timer),
	{noreply, S};
handle_info(_Info, S=#state{}) -> {noreply, S}.
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

%% impl

check() -> true.

parse(Event) -> Event.

rescan(_Pid, Tree, []) -> Tree;
rescan(Pid, Tree, [{File, #file_info{mtime=MTime} = Info} | FilesInfo]) ->
	NewTree = case gb_trees:lookup(File, Tree) of
		{value, #file_info{mtime=NewMTime}} when MTime =/= NewMTime->
			notify(Pid, modify, File),
			gb_trees:enter(File, Info, Tree);
		{value, _} -> Tree;
		_ ->
			notify(Pid, rename, File), % compatibility with inotify?
			gb_trees:enter(File, Info, Tree)
	end,
	rescan(Pid, NewTree, FilesInfo).

notify(Pid, Event, Path) ->
	Pid ! {self(), {data, {eol, {Path, [Event]}}}}.

info_tuple({ok, Info}, File) -> {File, Info};
info_tuple(_, File) -> File.

ok_list({ok, Files}) -> Files;
ok_list(_) -> [].

fold(Path) -> fold(Path, []).
fold(Path, Acc) ->
	FilesPath = [ filename:join(Path, File) || File <- ok_list(file:list_dir(Path)) ],
	FilesInfo = [ info_tuple(file:read_link_info(File, [raw]), File) || File <- FilesPath ],
	lists:flatten(Acc ++ [ fold_re(FileInfo) || FileInfo <- FilesInfo ]).

fold_re({File, #file_info{type = regular} = Info}) -> [{File, Info}];
fold_re({Path, #file_info{type = directory}}) -> fold(Path, []);
fold_re(_) -> [].

make_tree(FilesInfo) -> make_tree(gb_trees:empty(), FilesInfo).
make_tree(Tree, []) -> Tree;
make_tree(Tree, [{File, Info} | FilesInfo]) ->
	make_tree(gb_trees:enter(File, Info, Tree), FilesInfo).
