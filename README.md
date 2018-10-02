aten
=====

[![Build Status](https://travis-ci.org/rabbitmq/aten.svg?branch=master)](https://travis-ci.org/rabbitmq/aten)

Implementation of [A New Adaptive Accrual Failure Detector for Dependable Distributed Systems](https://dl.acm.org/citation.cfm?id=1244129)
for distributed erlang.

Status: Maturing. Essential component of [Ra](https://github.com/rabbitmq/ra)


Use
---

    % start the aten application if not part of a release
    {ok, _Apps} = aten:start(),
    % register interest in status changes for a node
    ok = aten:register(node1@host),
    % receive status change events
    receive
        {node_event, node1@host, down} -> ok;
        {node_event, node1@host, up} -> ok
    end

    % deregister interest
    ok = aten:deregister(node1@host),


Build
-----

    $ make

or

    $ rebar3 compile
