#!/bin/sh -e

epmd -daemon
rebar3 do eunit, ct, xref, dialyzer
