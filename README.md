aten
=====

Experimental implementation of [A New Adaptive Accrual Failure Detector for Dependable Distributed Systems](https://dl.acm.org/citation.cfm?id=1244129).

Status: Draft


Use
---

    % start the aten application if not part of a release
    ok = aten:start(),
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
