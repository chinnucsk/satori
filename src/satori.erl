-module(satori).

-export([convert/2]).

-type regexp_type() :: {regexp, iodata()}.
-type binary_type() :: {binary, {non_neg_integer(), non_neg_integer()}}.
-type binary_extend_type() :: {binary, {non_neg_integer(), non_neg_integer()}, {byte(), byte()}, [byte()]}.
-type integer_type() :: {integer, {non_neg_integer(), non_neg_integer()}}.
-type atom_type() :: {atom, [atom()]}.

-type type() :: regexp_type() | binary_type() | binary_extend_type() | integer_type() | atom_type().

-spec convert(type(), binary()) -> invalid_input | any().
%% convert({regexp, RE}, Binary) ->
convert({binary, {Min, Max}}, <<"0x", Rest/binary>>) when is_integer(Min), is_integer(Max) ->
    try
        case satori_mochihex:to_bin(binary_to_list(Rest)) of
            Bin when Min =< byte_size(Bin) andalso byte_size(Bin) =< Max ->
                Bin;
            _Bin ->
                invalid_input
        end
    catch
        _:_ ->
            invalid_input
    end;
convert({binary, {Min, Max}}, Binary) when is_integer(Min), is_integer(Max), is_binary(Binary) ->
    case Binary of
        Binary when Min =< byte_size(Binary) andalso byte_size(Binary) =< Max ->
            case is_printable_binary(Binary, {16#20, 16#7E}, []) of
                true ->
                    Binary;
                false ->
                    invalid_input
            end;
        _Binary ->
            invalid_input
    end;
convert({binary, {Min, Max}, {Start, End}, ListOfChar}, Binary) ->
    case Binary of
        Binary when Min =< byte_size(Binary) andalso byte_size(Binary) =< Max ->
            case is_printable_binary(Binary, {Start, End}, ListOfChar) of
                true ->
                    Binary;
                false ->
                    invalid_input
            end;
        _Binary ->
            invalid_input
    end;
convert({integer, {Min, Max}}, Binary) when is_integer(Min), is_integer(Max) ->
    case string:to_integer(binary_to_list(Binary)) of
        {error, _Reason} ->
            invalid_input;
        {Int, []} when Min =< Int andalso Int =< Max ->
            Int;
        {_Int, _Rest} ->
            invalid_input
    end;
convert({atom, ListOfAtom}, Binary) ->
    try
        Atom = binary_to_existing_atom(Binary, latin1),
        case lists:member(Atom, ListOfAtom) of
            true ->
                Atom;
            false ->
                invalid_input
        end
    catch
        _:_ ->
            invalid_input
    end;
convert(Type, Binary) ->
    error({unknown_type, Type, Binary}).

-spec is_printable_binary(list(), {byte(), byte()}, [char()]) -> boolean().
is_printable_binary(Binary, {Start, End}, ListOfChar) when is_binary(Binary) ->
    is_printable_binary(binary_to_list(Binary), {Start, End}, ListOfChar);
is_printable_binary([], {_Start, _End}, _ListOfChar) ->
    true;
is_printable_binary([C|Rest], {Start, End}, ListOfChar)  ->
    case lists:member(C, ListOfChar) of
        false when Start =< C andalso C =< End ->
            is_printable_binary(Rest, {Start, End}, ListOfChar);
        _ ->
            false
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_printable_binary_test_() ->
    [
        {"",
            ?_assertEqual(true, is_printable_binary(<<"abc">>, {16#20, 16#7E}, []))},
        {"",
            ?_assertEqual(false, is_printable_binary(<<"\0">>, {16#20, 16#7E}, []))},
        {"",
            ?_assertEqual(false, is_printable_binary(<<"abc">>, {16#20, 16#7E}, [$a]))}
    ].

-endif.
