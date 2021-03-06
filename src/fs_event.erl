-module(fs_event).
-behaviour(gen_server).
-include_lib("fs_event/include/logger.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, add_handler/3, delete_handler/3, stop/1]).
-record(state, {event_manager, port, handler}).

start_link(Path) -> gen_server:start_link(?MODULE, [Path], []).
add_handler(Pid, Handler, Args) -> gen_server:call(Pid, {add_handler, Handler, Args}).
delete_handler(Pid, Handler, Args) -> gen_server:call(Pid, {delete_handler, Handler, Args}).

stop(Pid) when is_pid(Pid) -> gen_server:call(Pid, {stop});
stop(#state{port=Port, event_manager=EventManager, handler=HandlerModule}) ->
	gen_event:stop(EventManager),
	HandlerModule:stop(Port).

%% API impl

init([Path]) ->
	{ok, EventManager} = gen_event:start_link(),
	HandlerModule = choose_handler(fs_event_cfg:force_backend(), [inotify, naive]),
	?INFO("Selected backend: ~p", [HandlerModule]),
	Handler = HandlerModule:start(Path),
	{ok, #state{event_manager=EventManager, port=Handler, handler=HandlerModule}}.
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

handle_info({_Port, {data, {eol, Line}}}, S=#state{event_manager=EventManager, handler=HandlerModule}) ->
	{File, Events} = HandlerModule:parse(Line),
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

choose_handler(false, []) ->
	erlang:error(no_fs_module_available);
choose_handler(false, [Module|Modules]) ->
	case Module:check() of
		true -> Module;
		false -> choose_handler(false, Modules)
	end;
choose_handler(Handler, _) -> Handler.
