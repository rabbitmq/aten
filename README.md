# Aten

[![Build Status](https://travis-ci.org/rabbitmq/aten.svg?branch=master)](https://travis-ci.org/rabbitmq/aten)

This is an implementation of [A New Adaptive Accrual Failure Detector for Dependable Distributed Systems](https://dl.acm.org/citation.cfm?id=1244129)
for distributed Erlang.

Aten is an essential dependency of [Ra](https://github.com/rabbitmq/ra).

## Project Maturity

`aten` is a reasonably mature project that has been used in production
for a few years. Breaking public API changes are unlikely.

## Build Status

![Actions](https://github.com/rabbitmq/aten/actions/workflows/tests.yml/badge.svg)

## Use

``` erl
%% start the aten application if not part of a release
{ok, _Apps} = aten:start(),
%% register interest in status changes for a node
ok = aten:register(node1@host),
%% receive status change events
receive
    {node_event, node1@host, down} -> ok;
    {node_event, node1@host, up} -> ok
end

%% deregister interest
ok = aten:deregister(node1@host),
```

## Build

``` shell
gmake
```

Or with Rebar 3

``` shell

rebar3 compile
```

## License

`aten` is [dual-licensed](./LICENSE) under the Mozilla Public License 2.0
and the Apache Public License 2.0.

(c) 2017-2021 VMware, Inc or its affiliates.
