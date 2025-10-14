# Stall detector

Not really a test, more of a demo of live-monitoring runtime-events to detect
stalls (extended periods of time when the Lwt main loop is not entered).

Stalls happen when too many of the promises make too many blocking calls. In the
example here (`Stallerlib.stall`) it's one promise which does increasingly long
blocking calls to `Unix.sleepf`. But it's also possible to stall with many short
blocking calls (e.g., many promises logging to a file in a blocking way). Stalls
can also happen if promises execute long computations.

(Stalls can be lengthened by the GC. Execution of the GC is the same as a
blocking call as far as Lwt is concerned.)

## Detection

The detection relies on two mechanism. The first mechanism polls the runtime
event ring to record the latest occurrence of Lwt scheduler's events. The second
mechanism checks that this latest occurrence is recent enough.

## selfdetector

The `selfdetector.ml` file shows how to run the detector in-process using a
separate domain to run the detection in parallel.

## staller + detector

The `stall-detect.sh` runs separate processes for a program which stalls for
increasingly long times and a program which monitors the events of the first
process.
