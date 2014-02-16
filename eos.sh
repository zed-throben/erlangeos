#!/bin/bash

PREFIX=/usr/local
LIBDIR=lib
BINDIR=bin

erl -noshell -pa $PREFIX/$LIBDIR/erleos/ebin -run erleos_cmd main $1 $2 $3 $4 $5 $6 $7 $8 $9

