%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_sup).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, NifPoolArgs} = application:get_env(bcrypt, bcrypt_pool),
    NifChildren = poolboy:child_spec(bcrypt_pool,
                                     [{name, {local, bcrypt_pool}},
                                      {worker_module, bcrypt_nif_worker}] ++
                                         NifPoolArgs),
    {ok, {{one_for_one, 10, 10}, [NifChildren]}}.
