%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_sup).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PortChildren
        = [{bcrypt_port_sup, {bcrypt_port_sup, start_link, []}, permanent,
            16#ffffffff, supervisor, [bcrypt_port_sup]},
           {bcrypt_pool, {bcrypt_pool, start_link, []}, permanent,
            16#ffffffff, worker, [bcrypt_pool]}],
    {ok, {{one_for_all, 15, 60}, PortChildren}}.
