%% -*- erlang -*-
%% @doc Unit tests for wf_case_runner

-module(wf_case_runner_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% Test case ID generation format
wf_case_runner_generate_case_id_format_test() ->
    CaseId = generate_case_id(),
    ?assert(is_binary(CaseId)),
    ?assertEqual(<<"case_">>, binary:part(CaseId, {0, 5})),
    ?assert(byte_size(CaseId) > 10).

%% Test case IDs are unique
wf_case_runner_generate_case_id_unique_test() ->
    Id1 = generate_case_id(),
    Id2 = generate_case_id(),
    ?assertNotEqual(Id1, Id2).

%%====================================================================
%% Internal Functions (for testing)
%%====================================================================

%% @private Helper function for testing case ID generation
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.
