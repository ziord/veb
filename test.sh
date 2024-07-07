#!/bin/sh
zig test tests/test.zig --main-pkg-path .
for file in "tests/modules"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -- run $file
    fi
done