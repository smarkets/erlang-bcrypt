%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt).
-author('Hunter Morris <hunter.morris@smarkets.com>').

%% API
-export([start/0, stop/0]).
-export([mechanism/0]).
-export([gen_salt/0, gen_salt/1, hashpw/2]).

start() -> application:start(bcrypt).
stop()  -> application:stop(bcrypt).

mechanism() ->
    {ok, M} = application:get_env(bcrypt, mechanism),
    M.

gen_salt() -> do_gen_salt(mechanism()).
gen_salt(Rounds) -> do_gen_salt(mechanism(), Rounds).
hashpw(Password, Salt) -> do_hashpw(mechanism(), Password, Salt).

do_gen_salt(nif)  -> bcrypt_nif_worker:gen_salt();
do_gen_salt(port) -> bcrypt_pool:gen_salt().

do_gen_salt(nif, Rounds)  -> bcrypt_nif_worker:gen_salt(Rounds);
do_gen_salt(port, Rounds) -> bcrypt_pool:gen_salt(Rounds).

do_hashpw(nif, Password, Salt)  -> bcrypt_nif_worker:hashpw(Password, Salt);
do_hashpw(port, Password, Salt) -> bcrypt_pool:hashpw(Password, Salt).
