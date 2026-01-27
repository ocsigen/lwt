#!/bin/bash
set -euo pipefail

PROJECT_ROOT=./"$(git rev-parse --show-cdup)"
dune build "$PROJECT_ROOT/_build/default/examples/stall_detection/staller.exe"
dune build "$PROJECT_ROOT/_build/default/examples/stall_detection/detector.exe"

RING_DIR=$(mktemp -d -t staller-detector.XXXXXX)

OCAML_RUNTIME_EVENTS_DIR="$RING_DIR" "$PROJECT_ROOT"/_build/default/examples/stall_detection/staller.exe &
STALLER_PID=$!

echo "staller started"

sleep 0.1

"$PROJECT_ROOT/_build/default/examples/stall_detection/detector.exe" "$RING_DIR" "$STALLER_PID"

echo "detector started"

# Optional: wait for both processes to finish
wait

rm -f "$STALLER_PID.events"
