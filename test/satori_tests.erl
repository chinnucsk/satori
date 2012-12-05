-module(satori_tests).

-import(satori, [convert/2]).

-include_lib("eunit/include/eunit.hrl").

convert_error_test_() ->
    [
        {"error",
            ?_assertError({unknown_type, spam, <<"abc">>}, convert(spam, <<"abc">>))}
    ].

convert_binary_test_() ->
    [
        {"success",
            ?_assertEqual(<<"abc">>, convert({binary, {1, 3}}, <<"abc">>))},
        {"failure: chars out of range",
            ?_assertEqual(invalid_input, convert({binary, {3, 3}}, <<"12\0">>))},
        {"failure: binary size short",
            ?_assertEqual(invalid_input, convert({binary, {3, 3}}, <<"12">>))},
        {"failure: binary size long",
            ?_assertEqual(invalid_input, convert({binary, {3, 3}}, <<"1234">>))},

        {"sfailure: not begin '0b'",
            ?_assertEqual(invalid_input, convert({binary, 1}, <<"0x1">>))},

        {"success",
            ?_assertEqual(<<1:1>>, convert({binary, 1}, <<"0b1">>))},
        {"success",
            ?_assertEqual(<<9:4>>, convert({binary, 4}, <<"0b1001">>))},
        {"success",
            ?_assertEqual(<<9:5>>, convert({binary, 5}, <<"0b1001">>))},
        {"failure: size short",
            ?_assertEqual(invalid_input, convert({binary, 3}, <<"0b1001">>))},
        {"failure: not binary digits",
            ?_assertEqual(invalid_input, convert({binary, 3}, <<"0b100a">>))},


        {"success",
            ?_assertEqual(<<1,2>>, convert({binary, {1, 2}}, <<"0x0102">>))},
        {"success",
            ?_assertEqual(<<1,2>>, convert({binary, {2, 2}}, <<"0x0102">>))},
        {"failure: binary size short",
            ?_assertEqual(invalid_input, convert({binary, {3, 3}}, <<"0x0102">>))},
        {"failure: binary size long",
            ?_assertEqual(invalid_input, convert({binary, {3, 3}}, <<"0x01020304">>))},
        {"failure: chars out of range",
            ?_assertEqual(invalid_input, convert({binary, {1, 5}}, <<"0xFFGG">>))}
    ].

convert_binary_extend_test_() ->
    [
        {"success",
            ?_assertEqual(<<"abc">>, convert({binary, {1, 3}, {16#20, 16#7E}, []}, <<"abc">>))},
        {"failure:  binary size short",
            ?_assertEqual(invalid_input, convert({binary, {4, 4}, {16#20, 16#7E}, []}, <<"abc">>))},
        {"failure: include 'c' ",
            ?_assertEqual(invalid_input, convert({binary, {1, 3}, {16#20, 16#7E}, [$c]}, <<"abc">>))}
    ].

convert_integer_test_() ->
    [
        {"success",
            ?_assertEqual(1, convert({integer, {1, 1}}, <<"1">>))},
        {"failure: chars",
            ?_assertEqual(invalid_input, convert({integer, {1, 1}}, <<"a">>))},
        {"failure: large integer",
            ?_assertEqual(invalid_input, convert({integer, {256, 512}}, <<"513">>))},
        {"failure: small integer",
            ?_assertEqual(invalid_input, convert({integer, {0, 255}}, <<"-1">>))}
    ].

convert_atom_test_() ->
    [
        {"success",
            ?_assertEqual(spam, convert({atom, [spam, eggs]}, <<"spam">>))},
        {"failure: chars",
            ?_assertEqual(invalid_input, convert({atom, [spam]},<<"eggs">>))},
        {"failure: chars",
            ?_assertEqual(invalid_input, convert({atom, [spam]},<<"bacon">>))}
    ].

convert_ipaddr_test_() ->
    [
        {"success ipv4",
            ?_assertEqual({192,0,2,0}, convert(ipaddr, <<"192.0.2.0">>))},
        {"success ipv6",
            ?_assertEqual({8193,3512,0,0,0,0,0,0}, convert(ipaddr, <<"2001:DB8::">>))},
        {"failure: comma",
            ?_assertEqual(invalid_input, convert(ipaddr,<<"192,0,2,0">>))},
        {"failure: integer",
            ?_assertEqual(invalid_input, convert(ipaddr,<<"192020">>))}
    ].



