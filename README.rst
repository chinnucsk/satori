#####################
satori
#####################

.. image:: https://secure.travis-ci.org/voluntas/satori.png?branch=develop

satori is type base binary converter.

binary type::

    > satori:convert({binary, {1, 2}}, <<"ab">>).
    <<"ab">>
    > satori:convert({binary, {1, 2}}, <<"abc">>).
    invalid_input

binary extend type::

    > satori:convert({binary, {1, 5}, {16#20, 16#7E}, [$,]}, <<"abc">>).
    <<"abc">>
    > satori:convert({binary, {1, 5}, {16#20, 16#7E}, [$,]}, <<"a,c">>).
    invalid_input
    > satori:convert({binary, {1, 5}, {16#20, 16#7E}, [$,]}, <<"\0\1\3">>).
    invalid_input

integer type::

    > satori:convert({integer, {1, 32}}, <<"12">>). 
    12
    > satori:convert({integer, {1, 32}}, <<"33">>).
    invalid_input
    > satori:convert({integer, {1, 32}}, <<"a">>). 
    invalid_input

atom type::

    > satori:convert({atom, [spam]}, <<"spam">>).
    spam
    > satori:convert({atom, [spam]}, <<"egg">>). 
    invalid_input
