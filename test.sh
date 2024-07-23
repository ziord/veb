#!/bin/sh
#-------- behavioural --------#
zig build test

#----------- std -----------#
for file in "lib/std/test"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -- run $file
        echo
    fi
done
for file in "lib/std/test"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -Doptimize=ReleaseSafe -- run $file
        echo
    fi
done
for file in "lib/std/test"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -Doptimize=ReleaseSafe -- check $file
        echo
    fi
done

#----------- modules -----------#
for file in "tests/modules"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -- run $file
        echo
    fi
done
for file in "tests/modules"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -Doptimize=ReleaseSafe -- run $file
        echo
    fi
done
for file in "tests/modules"/*."veb"; do
    if [ -f "$file" ]; then
        echo $file
        zig build run -Doptimize=ReleaseSafe -- check $file
        echo
    fi
done
