# Tail tracing

This is a demo for producing a tail-trace for an Lwt program. The Lwt program
emits tracing events as introduced by the ppx. A separate program monitors the
execution of the Lwt program process and, upon a crash, it gets the content of
the events ring-buffer and dumps it into a perfetto-compatible format.

The produced trace shows the trace of execution for the end of the program, the
part leading to its crash. This is called "tail tracing."
