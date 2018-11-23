#!/bin/sh -e +x

epmd -daemon
rebar3 do eunit, ct, xref, dialyzer
