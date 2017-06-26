%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2011 Hunter Morris
%%
%% @doc Wrapper around the OpenBSD Blowfish password hashing algorithm, as
%% described in "A Future-Adaptable Password Scheme" by Niels Provos and
%% David Mazieres: http://www.openbsd.org/papers/bcrypt-paper.ps
%% @end
%%
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.

%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(bcrypt_nif).
-author('Hunter Morris <hunter.morris@smarkets.com>').

%% API
-export([init/0]).
-export([gen_salt/1, hashpw/5, create_ctx/0]).

-on_load(init/0).

%%--------------------------------------------------------------------
%% @doc Load the bcrypt NIFs
%% @spec init() -> ok
%% @end
%%--------------------------------------------------------------------
init() ->
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
    erlang:load_nif(filename:join(Dir, "bcrypt_nif"), 0).

%%--------------------------------------------------------------------
%% @doc Generate a random text salt for use with hashpw/3. LogRounds
%% defines the complexity of the hashing, increasing the cost as
%% 2^log_rounds.
%% @spec gen_salt(integer()) -> string()
%% @end
%%--------------------------------------------------------------------
gen_salt(LogRounds)
  when is_integer(LogRounds), LogRounds < 32, LogRounds > 3 ->
    R = crypto:strong_rand_bytes(16),
    encode_salt(R, LogRounds).

encode_salt(_R, _LogRounds) ->
    nif_stub_error(?LINE).

%%--------------------------------------------------------------------
%% @doc Create a context which hashes passwords in a separate thread.
%% @spec create_ctx() -> term()
%% @end
%%--------------------------------------------------------------------
create_ctx() ->
    nif_stub_error(?LINE).

%%--------------------------------------------------------------------
%% @doc Hash the specified password and the salt using the OpenBSD
%% Blowfish password hashing algorithm. Returns the hashed password.
%% @spec hashpw(Ctx::term(),
%%              Ref::reference(),
%%              Pid::pid(),
%%              Password::binary(),
%%              Salt::binary()) -> string()
%% @end
%%--------------------------------------------------------------------
hashpw(_Ctx, _Ref, _Pid, _Password, _Salt) ->
    nif_stub_error(?LINE).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).
