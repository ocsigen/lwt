#!/bin/bash

make tests
cd _build/tests
for i in *; do
    if [ -d $i ]; then
        if [ -f ./$i/main.native ]; then
            ./$i/main.native
        else
            ./$i/main.byte
        fi
    fi
done
