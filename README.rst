erlang-bcrypt
=============

erlang-bcrypt is a wrapper around the OpenBSD Blowfish password hashing
algorithm, as described in `"A Future-Adaptable Password Scheme"`_ by Niels
Provos and David Mazieres.

.. _"A Future-Adaptable Password Scheme":
   http://www.openbsd.org/papers/bcrypt-paper.ps

Basic build instructions
------------------------

1. Build it (project uses rebar, but I've included a Makefile)::

        make

2. Run it (simple way, starting sasl, crypto and bcrypt)::

        erl -pa ebin -boot start_sasl -s crypto -s bcrypt

Basic usage instructions
------------------------

3. Hash a password using a salt with the default number of rounds::

        1> {ok, Salt} = bcrypt:gen_salt(),
        {ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK."}
        2> {ok, Hash} = bcrypt:hashpw("foo", Salt),
        {ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6"}

3. Verify the password::

        3> {ok, Hash} =:= bcrypt:hashpw("foo", Hash).
        true
        4> {ok, Hash} =:= bcrypt:hashpw("bar", Hash).
        false

Configuration
-------------

The bcrypt application is configured by changing values in the
application's environment:

``default_log_rounds``
  Sets the default number of rounds which define the complexity of the
  hash function. Defaults to ``12``.

Authors
-------

* `Hunter Morris`_
* `Mrinal Wadhwa`_

.. _Hunter Morris:
   http://github.com/skarab

.. _Mrinal Wadhwa:
   http://github.com/mrinalwadhwa
