%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt).
-author('Hunter Morris <hunter.morris@smarkets.com>').

%% API
-export([start/0, stop/0]).
-export([gen_salt/0, gen_salt/1, hashpw/2]).

start() -> application:start(bcrypt).
stop()  -> application:stop(bcrypt).

gen_salt() -> do_gen_salt().
gen_salt(Rounds) -> do_gen_salt(Rounds).
hashpw(Password, Salt) -> do_hashpw(Password, Salt).

do_gen_salt() -> bcrypt_pool:gen_salt().

do_gen_salt(Rounds) -> bcrypt_pool:gen_salt(Rounds).

do_hashpw(Password, Salt) -> bcrypt_pool:hashpw(Password, Salt).
