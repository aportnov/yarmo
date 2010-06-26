#!/bin/sh
cd `dirname $0`
exec erl -env ERL_MAX_PORTS 50000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s yarmo
