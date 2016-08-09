-module(fs_event_cfg).
-export([
	rescan_period/0,
	follow_symlinks/0,
	force_backend/0
]).
-define(APP, fs_event).

truefy({ok, Value}) -> Value;
truefy(_) -> false.

truefy({ok, Value}, _) -> Value;
truefy(_, Default) -> Default.

rescan_period() -> truefy(application:get_env(?APP, naive_rescan), 10000).
follow_symlinks() -> truefy(application:get_env(?APP, follow_symlinks)).
force_backend() -> truefy(application:get_env(?APP, force_backend)).