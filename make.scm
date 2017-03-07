#!/usr/bin/scheme --script

(system "rm -f foo.c")
(load "compiler.scm")
(compile-scheme-file "foo.scm" "foo.c")
(system "gcc -o foo foo.c")
(exit)