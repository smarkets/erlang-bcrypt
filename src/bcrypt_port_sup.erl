%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_port_sup).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(supervisor).

-export([start_link/0, start_child/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child() -> supervisor:start_child(?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 1},
          [{undefined,
            {bcrypt_port, start_link, []},
            transient, brutal_kill, worker, [bcrypt_port]}]}}.
