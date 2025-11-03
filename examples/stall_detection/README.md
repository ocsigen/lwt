# Stall detector

In this directory is a demo for live-monitoring runtime-events and detecting
stalls (extended periods of time when the Lwt main loop is not entered).

Stalls happen when too many of the promises make too many blocking calls. In the
example here (`Stallerlib.stall`) it's one promise which does increasingly long
blocking calls to `Unix.sleepf`. Stalls can also happen with many short blocking
calls (e.g., many promises logging to a file in a blocking way). Stalls
can also happen if promises execute long computations.

## Detection

The detection relies on two mechanism. The first mechanism polls the runtime
event ring to record the latest occurrence of Lwt scheduler's events. The second
mechanism checks that this latest occurrence is recent enough.

## selfdetector

The `selfdetector.ml` file shows how to run the detector in the same process as
the Lwt code. It runs in a separate domain so it executes in parallel.

## staller + detector

The `stall-detect.sh` runs separate processes for the Lwt-based program which
stalls and a program which monitors the events of the first process.
