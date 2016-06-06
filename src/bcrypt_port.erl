%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_port).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([gen_salt/1, gen_salt/2]).
-export([hashpw/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {port :: port(), default_log_rounds :: pos_integer()}).

-define(CMD_SALT, 0).
-define(CMD_HASH, 1).

start_link() ->
    Dir = case code:priv_dir(bcrypt) of
              {error, bad_name} ->
                  case code:which(bcrypt) of
                      Filename when is_list(Filename) ->
                          filename:join(
                            [filename:dirname(Filename), "../priv"]);
                      _ ->
                          "../priv"
                  end;
              Priv -> Priv
          end,
    Port = filename:join(Dir, "erlang-bcrypt"),
    gen_server:start_link(?MODULE, [Port], []).

stop() -> gen_server:call(?MODULE, stop).

gen_salt(Pid) ->
    R = crypto:strong_rand_bytes(16),
    gen_server:call(Pid, {encode_salt, R}, infinity).

gen_salt(Pid, LogRounds) ->
    R = crypto:strong_rand_bytes(16),
    gen_server:call(Pid, {encode_salt, R, LogRounds}, infinity).

hashpw(Pid, Password, Salt) ->
    gen_server:call(Pid, {hashpw, Password, Salt}, infinity).

init([Filename]) ->
    case file:read_file_info(Filename) of
        {ok, _Info} ->
            Port = open_port(
                     {spawn, Filename}, [{packet, 2}, binary, exit_status]),
            ok = bcrypt_pool:available(self()),
            {ok, Rounds} = application:get_env(bcrypt, default_log_rounds),
            {ok, #state{port = Port, default_log_rounds = Rounds}};
        {error, Reason} ->
            error_logger:error_msg("Can't open file ~p: ~p", [Filename, Reason]),
            {stop, error_opening_bcrypt_file}
    end.

terminate(_Reason, #state{port=Port}) ->
    true = port_close(Port).

handle_call({encode_salt, R}, From, #state{default_log_rounds = LogRounds} = State) ->
    handle_call({encode_salt, R, LogRounds}, From, State);
handle_call({encode_salt, R, LogRounds}, From, State=#state{port=Port}) ->
    Data = term_to_binary({?CMD_SALT, From, {R, LogRounds}}),
    true = port_command(Port, Data),
    {noreply, State};
handle_call({hashpw, Password, Salt}, From, State=#state{port=Port}) ->
    Data = term_to_binary({?CMD_HASH, From, {Password, Salt}}),
    true = port_command(Port, Data),
    {noreply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("Unhandled cast: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({Port, {data, Data}}, State=#state{port=Port}) ->
    {Cmd, To, Reply0} = binary_to_term(Data),
    Reply =
        case {Cmd, Reply0} of
            {?CMD_SALT, "Invalid salt"} -> {error, invalid_salt};
            {?CMD_HASH, "Invalid salt"} -> {error, invalid_salt};
            {?CMD_SALT, "Invalid number of rounds"} -> {error, invalid_rounds};
            {?CMD_HASH, "Invalid salt length"} -> {error, invalid_salt_length};
            {_, _} when Cmd =:= ?CMD_SALT; Cmd =:= ?CMD_HASH -> {ok, Reply0}
        end,
    gen_server:reply(To, Reply),
    ok = bcrypt_pool:available(self()),
    {noreply, State};
handle_info({Port, {exit_status, Status}}, #state{port=Port}=State) ->
    %% Rely on whomever is supervising this process to restart.
    error_logger:warning_msg("Port died: ~p", [Status]),
    {stop, port_died, State}.
