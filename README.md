Filesystem Events
==================

Recursivly watches specified path and procuces events if file was created or modified.
Have two backends, one based on inotifywait port, another is erlang-native. If inotifywait
is not available (not in search path), then native naive backend is used automatically.

Naive backend
-------------

Scans directory structure periodically (10 seconds default), and compares with stored one,
and produces events (create and modify) if there are differences.

Inotifywait backend
-------------------

Relies on inotifywait binary and on shell trick to kill spawned binary, as inotifywait doesn't
read STDIN and therefore doesn't die on SIGPIPE signal if spawned process terminates.

TODO
----

1. Add more backends (Mac, Win)