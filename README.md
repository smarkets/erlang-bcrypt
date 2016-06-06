erlang-bcrypt
=============

[![Build Status][travis_ci_image]][travis_ci]
[![Coverage Status](https://coveralls.io/repos/github/puzza007/erlang-bcrypt/badge.svg?branch=master)](https://coveralls.io/github/puzza007/erlang-bcrypt?branch=master)

erlang-bcrypt is a wrapper around the OpenBSD Blowfish password hashing algorithm, as described in [“A Future-Adaptable Password Scheme”] by Niels Provos and David Mazieres.

Basic build instructions
------------------------

1.  Build it (project uses rebar3, but I’ve included a Makefile):

        make

2.  Run it (simple way, starting sasl, crypto and bcrypt):

        erl -pa ebin -boot start_sasl -s crypto -s bcrypt

Basic usage instructions
------------------------

1.  Hash a password using a salt with the default number of rounds:

```erlang
        1> {ok, Salt} = bcrypt:gen_salt().
        {ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK."}
        2> {ok, Hash} = bcrypt:hashpw("foo", Salt).
        {ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6"}
```

2.  Verify the password:

```erlang
        3> {ok, Hash} =:= bcrypt:hashpw("foo", Hash).
        true
        4> {ok, Hash} =:= bcrypt:hashpw("bar", Hash).
        false
```

Configuration
-------------

The bcrypt application is configured by changing values in the application’s environment:

`default_log_rounds`  
Sets the default number of rounds which define the complexity of the hash function. Defaults to `12`.

Authors
-------

-   [Hunter Morris]
-   [Mrinal Wadhwa]
-   [Paul Oliver]

  [“A Future-Adaptable Password Scheme”]: http://www.openbsd.org/papers/bcrypt-paper.ps
  [Hunter Morris]: http://github.com/skarab
  [Mrinal Wadhwa]: http://github.com/mrinalwadhwa
  [Paul Oliver]: http://github.com/puzza007

[travis_ci]: https://travis-ci.org/puzza007/erlang-bcrypt
[travis_ci_image]: https://travis-ci.org/puzza007/erlang-bcrypt.png

