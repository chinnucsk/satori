#####################
satori
#####################

satori is type base binary convertor.

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
