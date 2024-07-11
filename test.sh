#!/bin/sh
zig build test
for file in "tests/modules"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -- run $file
    fi
done
for file in "tests/modules"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -Doptimize=ReleaseSafe -- run $file
    fi
done