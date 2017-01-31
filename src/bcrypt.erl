%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt).
-author('Hunter Morris <hunter.morris@smarkets.com>').

%% API
-export([
         start/0,
         stop/0,
         get_rounds/0,
         gen_salt/0,
         gen_salt/1,
         hashpw/2
        ]).

-define(POOL, bcrypt_pool).

start() ->
    application:start(bcrypt).

stop() ->
    application:stop(bcrypt).

get_rounds() ->
    {ok, Default} = application:get_env(bcrypt, default_log_rounds),
    Default.

gen_salt() ->
    Rounds = get_rounds(),
    gen_salt(Rounds).
gen_salt(Rounds) ->
    poolboy:transaction(
      ?POOL,
      fun(Worker) ->
              gen_server:call(Worker, {gen_salt, Rounds})
      end).
hashpw(Password, Salt) ->
    poolboy:transaction(
      ?POOL,
      fun(Worker) ->
              gen_server:call(Worker, {hashpw, Password, Salt})
      end).
