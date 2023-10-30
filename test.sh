#!/bin/sh

folder="tests/"
skip="test.zig"
pkg="--main-pkg-path ."
for file in "$folder"/*; do
    if [ -f "$file" ]; then
        if [ "$file" != "$folder/$skip" ]; then
            zig test "$file" $pkg
        fi
    fi
done
