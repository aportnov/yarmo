#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/erlang_couchdb-0.2.3/ebin -boot start_sasl -s reloader -s erlymessage
