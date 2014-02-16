#!/bin/sh

./rebar compile
cd ebin
#erl -eval "b:t()"
erl
cd ..
