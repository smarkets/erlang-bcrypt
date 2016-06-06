%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_app).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-behaviour(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
    bcrypt_sup:start_link().

stop(_State) -> ok.
