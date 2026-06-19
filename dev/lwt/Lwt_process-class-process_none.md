
# Class `Lwt_process.process_none`

```ocaml
method pid : int
```
Pid of the sub-process

```ocaml
method state : state
```
Return the state of the process

```ocaml
method kill : int -> unit
```
`kill signum` sends `signum` to the process if it is still running.

```ocaml
method terminate : unit
```
Terminates the process. It is equivalent to `kill Sys.sigkill` on Unix but also works on Windows (unlike `Lwt_process.process_none.kill`).

```ocaml
method status : Unix.process_status Lwt.t
```
Threads which wait for the sub-process to exit then returns its exit status

```ocaml
method rusage : Lwt_unix.resource_usage Lwt.t
```
Threads which wait for the sub-process to exit then returns its resource usages

```ocaml
method close : Unix.process_status Lwt.t
```
Closes the process and returns its exit status. This closes all channels used to communicate with the process
