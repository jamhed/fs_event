-module(fs_event).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, add_handler/3, delete_handler/3, stop/1]).

-record(state, {event_manager, port}).

start_link(Path) -> gen_server:start_link(?MODULE, [Path], []).
add_handler(Pid, Handler, Args) -> gen_server:call(Pid, {add_handler, Handler, Args}).
delete_handler(Pid, Handler, Args) -> gen_server:call(Pid, {delete_handler, Handler, Args}).

stop(Pid) when is_pid(Pid) -> gen_server:call(Pid, {stop});
stop(#state{port=Port, event_manager=EventManager}) ->
	gen_event:stop(EventManager),
	erlang:port_close(Port).

%% API impl

init([Path]) ->
	{ok, EventManager} = gen_event:start_link(),
	Port = inotify:start_port(Path),
	{ok, #state{event_manager=EventManager, port=Port}}.
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

handle_info({_Port, {data, {eol, Line}}}, S=#state{event_manager=EventManager}) ->
	{File, Events} = inotify:parse(Line),
	_ = [ gen_event:notify(EventManager, {Event, File}) || Event <- Events ],
	{noreply, S};
handle_info(_Info, S=#state{}) ->
	{noreply, S}.

handle_call({add_handler, Handler, Args}, _From, S=#state{event_manager=EventManager}) ->
	gen_event:add_handler(EventManager, Handler, Args),
	{reply, ok, S};
handle_call({delete_handler, Handler, Args}, _From, S=#state{event_manager=EventManager}) ->
	gen_event:delete_handler(EventManager, Handler, Args),
	{reply, ok, S};
handle_call({stop}, _From, S=#state{}) ->
	{stop, normal, ok, S};
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.

terminate(_Reason, S) ->
	stop(S),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
