%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_nif_worker).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

%% gen_server
-export([init/1, code_change/3, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
          default_log_rounds,
          context
         }).

start_link() ->
    start_link([]).

start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, Default} = application:get_env(bcrypt, default_log_rounds),
    Ctx = bcrypt_nif:create_ctx(),
    {ok, #state{default_log_rounds = Default, context = Ctx}}.

terminate(shutdown, _) -> ok.

handle_call(gen_salt, _From, #state{default_log_rounds = R} = State) ->
    {reply, {ok, bcrypt_nif:gen_salt(R)}, State};
handle_call({gen_salt, R}, _From, State) ->
    {reply, {ok, bcrypt_nif:gen_salt(R)}, State};
handle_call({hashpw, Password, Salt}, _From, #state{context=Ctx}=State) ->
    Ref = make_ref(),
    ok = bcrypt_nif:hashpw(Ctx, Ref, self(), Password, Salt),
    receive
        {ok, Ref, Result} ->
            {reply, {ok, Result}, State};
        {error, Ref, Result} ->
            {reply, {error, Result}, State}
    end;
handle_call(Msg, _, _) -> exit({unknown_call, Msg}).
handle_cast(Msg, _) -> exit({unknown_cast, Msg}).
handle_info(Msg, _) -> exit({unknown_info, Msg}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
