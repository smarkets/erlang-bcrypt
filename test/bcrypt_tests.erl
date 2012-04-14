-module(bcrypt_tests).
-include_lib("eunit/include/eunit.hrl").

-define(
   PAIRS,
   % From bcrypt-ruby: https://github.com/codahale/bcrypt-ruby/
   [{<<"">>,
     <<"$2a$06$DCq7YPn5Rq63x1Lad4cll.">>,
     <<"$2a$06$DCq7YPn5Rq63x1Lad4cll.TV4S6ytwfsfvkgY8jIucDrjc8deX1s.">>},
    {<<"">>,
     <<"$2a$08$HqWuK6/Ng6sg9gQzbLrgb.">>,
     <<"$2a$08$HqWuK6/Ng6sg9gQzbLrgb.Tl.ZHfXLhvt/SgVyWhQqgqcZ7ZuUtye">>},
    {<<"">>,
     <<"$2a$10$k1wbIrmNyFAPwPVPSVa/ze">>,
     <<"$2a$10$k1wbIrmNyFAPwPVPSVa/zecw2BCEnBwVS2GbrmgzxFUOqW9dk4TCW">>},
    {<<"">>,
     <<"$2a$12$k42ZFHFWqBp3vWli.nIn8u">>,
     <<"$2a$12$k42ZFHFWqBp3vWli.nIn8uYyIkbvYRvodzbfbK18SSsY.CsIQPlxO">>},
    {<<"a">>,
     <<"$2a$06$m0CrhHm10qJ3lXRY.5zDGO">>,
     <<"$2a$06$m0CrhHm10qJ3lXRY.5zDGO3rS2KdeeWLuGmsfGlMfOxih58VYVfxe">>},
    {<<"a">>,
     <<"$2a$08$cfcvVd2aQ8CMvoMpP2EBfe">>,
     <<"$2a$08$cfcvVd2aQ8CMvoMpP2EBfeodLEkkFJ9umNEfPD18.hUF62qqlC/V.">>},
    {<<"a">>,
     <<"$2a$10$k87L/MF28Q673VKh8/cPi.">>,
     <<"$2a$10$k87L/MF28Q673VKh8/cPi.SUl7MU/rWuSiIDDFayrKk/1tBsSQu4u">>},
    {<<"a">>,
     <<"$2a$12$8NJH3LsPrANStV6XtBakCe">>,
     <<"$2a$12$8NJH3LsPrANStV6XtBakCez0cKHXVxmvxIlcz785vxAIZrihHZpeS">>},
    {<<"abc">>,
     <<"$2a$06$If6bvum7DFjUnE9p2uDeDu">>,
     <<"$2a$06$If6bvum7DFjUnE9p2uDeDu0YHzrHM6tf.iqN8.yx.jNN1ILEf7h0i">>},
    {<<"abc">>,
     <<"$2a$08$Ro0CUfOqk6cXEKf3dyaM7O">>,
     <<"$2a$08$Ro0CUfOqk6cXEKf3dyaM7OhSCvnwM9s4wIX9JeLapehKK5YdLxKcm">>},
    {<<"abc">>,
     <<"$2a$10$WvvTPHKwdBJ3uk0Z37EMR.">>,
     <<"$2a$10$WvvTPHKwdBJ3uk0Z37EMR.hLA2W6N9AEBhEgrAOljy2Ae5MtaSIUi">>},
    {<<"abc">>,
     <<"$2a$12$EXRkfkdmXn2gzds2SSitu.">>,
     <<"$2a$12$EXRkfkdmXn2gzds2SSitu.MW9.gAVqa9eLS1//RYtYCmB1eLHg.9q">>},
    {<<"abcdefghijklmnopqrstuvwxyz">>,
     <<"$2a$06$.rCVZVOThsIa97pEDOxvGu">>,
     <<"$2a$06$.rCVZVOThsIa97pEDOxvGuRRgzG64bvtJ0938xuqzv18d3ZpQhstC">>},
    {<<"abcdefghijklmnopqrstuvwxyz">>,
     <<"$2a$08$aTsUwsyowQuzRrDqFflhge">>,
     <<"$2a$08$aTsUwsyowQuzRrDqFflhgekJ8d9/7Z3GV3UcgvzQW3J5zMyrTvlz.">>},
    {<<"abcdefghijklmnopqrstuvwxyz">>,
     <<"$2a$10$fVH8e28OQRj9tqiDXs1e1u">>,
     <<"$2a$10$fVH8e28OQRj9tqiDXs1e1uxpsjN0c7II7YPKXua2NAKYvM6iQk7dq">>},
    {<<"abcdefghijklmnopqrstuvwxyz">>,
     <<"$2a$12$D4G5f18o7aMMfwasBL7Gpu">>,
     <<"$2a$12$D4G5f18o7aMMfwasBL7GpuQWuP3pkrZrOAnqP.bmezbMng.QwJ/pG">>},
    {<<"~!@#$%^&*()      ~!@#$%^&*()PNBFRD">>,
     <<"$2a$06$fPIsBO8qRqkjj273rfaOI.">>,
     <<"$2a$06$fPIsBO8qRqkjj273rfaOI.HtSV9jLDpTbZn782DC6/t7qT67P6FfO">>},
    {<<"~!@#$%^&*()      ~!@#$%^&*()PNBFRD">>,
     <<"$2a$08$Eq2r4G/76Wv39MzSX262hu">>,
     <<"$2a$08$Eq2r4G/76Wv39MzSX262huzPz612MZiYHVUJe/OcOql2jo4.9UxTW">>},
    {<<"~!@#$%^&*()      ~!@#$%^&*()PNBFRD">>,
     <<"$2a$10$LgfYWkbzEvQ4JakH7rOvHe">>,
     <<"$2a$10$LgfYWkbzEvQ4JakH7rOvHe0y8pHKF9OaFgwUZ2q7W2FFZmZzJYlfS">>},
    {<<"~!@#$%^&*()      ~!@#$%^&*()PNBFRD">>,
     <<"$2a$12$WApznUOJfkEGSmYRfnkrPO">>,
     <<"$2a$12$WApznUOJfkEGSmYRfnkrPOr466oFDCaj4b6HY3EXGvfxm43seyhgC">>},
    %% From the Openwall implementation: http://www.openwall.com/crypt/
    {<<"U*U">>,
     <<"$2a$05$CCCCCCCCCCCCCCCCCCCCC.">>,
     <<"$2a$05$CCCCCCCCCCCCCCCCCCCCC.E5YPO9kmyuRGyh0XouQYb4YMJKvyOeW">>},
    {<<"U*U*">>,
     <<"$2a$05$CCCCCCCCCCCCCCCCCCCCC.">>,
     <<"$2a$05$CCCCCCCCCCCCCCCCCCCCC.VGOzA784oUp/Z0DY336zx7pLYAy0lwK">>},
    {<<"U*U*U">>,
     <<"$2a$05$XXXXXXXXXXXXXXXXXXXXXO">>,
     <<"$2a$05$XXXXXXXXXXXXXXXXXXXXXOAcXxm9kjPGEMsLznoKqmqw7tc8WCx4a">>},
    {<<"">>,
     <<"$2a$05$CCCCCCCCCCCCCCCCCCCCC.">>,
     <<"$2a$05$CCCCCCCCCCCCCCCCCCCCC.7uG0VCzI2bS7j6ymqJi9CdcdxiRTWNy">>},
    {<<"0123456789abcdefghijklmnopqrstuvwxyz"
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>,
     <<"$2a$05$abcdefghijklmnopqrstuu">>,
     <<"$2a$05$abcdefghijklmnopqrstuu5s2v8.iXieOjg/.AySBTTZIIVFJeBui">>}]).

start_with(Mechanism) when Mechanism =:= nif; Mechanism =:= port ->
    application:start(crypto),
    case application:load(bcrypt) of
        {error, {already_loaded, bcrypt}} -> ok;
        ok -> ok
    end,
    ok = application:set_env(bcrypt, mechanism, Mechanism),
    case application:start(bcrypt) of
        {error, {already_started, bcrypt}} ->
            ok = application:stop(bcrypt),
            ok = application:start(bcrypt);
        ok -> ok
    end.

simple_nif_test_() ->
    {setup, fun() -> ok = start_with(nif) end,
     [{timeout, 1000,
       fun() ->
               {ok, Salt} = bcrypt:gen_salt(),
               {ok, Hash} = bcrypt:hashpw(<<"foo">>, Salt),
               ?assert({ok, Hash} =:= bcrypt:hashpw(<<"foo">>, Hash)),
               ?assertNot({ok, Hash} =:= bcrypt:hashpw(<<"bar">>, Hash))
       end}]}.

pair_nif_test_() ->
    {setup, fun() -> ok = start_with(nif) end,
     [{binary_to_list(Pass),
       ?_assertEqual({ok, Hash}, bcrypt:hashpw(Pass, Salt))} ||
         {Pass, Salt, Hash} <- ?PAIRS]}.

simple_port_test_() ->
    {setup, fun() -> ok = start_with(port) end,
     [{timeout, 1000,
       fun() ->
               {ok, Salt} = bcrypt:gen_salt(),
               {ok, Hash} = bcrypt:hashpw(<<"foo">>, Salt),
               ?assert({ok, Hash} =:= bcrypt:hashpw(<<"foo">>, Hash)),
               ?assertNot({ok, Hash} =:= bcrypt:hashpw(<<"bar">>, Hash))
       end}]}.

pair_port_test_() ->
    {setup, fun() -> ok = start_with(port) end,
     [{binary_to_list(Pass),
       ?_assertEqual({ok, Hash}, bcrypt:hashpw(Pass, Salt))} ||
         {Pass, Salt, Hash} <- ?PAIRS]}.
