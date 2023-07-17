#!/bin/sh
pkg="--main-pkg-path src"
zig test src/tests/runtime.zig $pkg && zig test src/tests/static.zig $pkg && zig test src/tests/error.zig $pkg