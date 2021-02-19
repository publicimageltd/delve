#!/usr/bin/bash
#
# Run emacs with a fake db in /tmp, and open delve.
# 

eldev emacs -L tests -L tests/utils -eval '(load "delve-with-fake-db")'
