#!/bin/bash

make tests
code=$?
if [ $code -ne 0 ]; then
    exit $code
fi

cd _build/tests
for i in *; do
    if [ -d $i ]; then
        echo "================================================================================"
        if [ -f ./$i/main.native ]; then
            ./$i/main.native
        else
            ./$i/main.byte
        fi
        echo
    fi
done
