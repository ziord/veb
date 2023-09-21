#!/bin/sh

folder="src/tests/"
skip="test.zig"
pkg="--main-pkg-path src"
for file in "$folder"/*; do
    if [ -f "$file" ]; then
        if [ "$file" != "$folder/$skip" ]; then
            zig test "$file" $pkg
        fi
    fi
done
