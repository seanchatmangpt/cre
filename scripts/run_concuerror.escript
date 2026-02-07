#!/usr/bin/env escript
%%! -pa _build/concuerror/lib/cre/ebin _build/concuerror/lib/concuerror/ebin _build/concuerror/lib/getopt/ebin _build/concuerror+test/lib/cre/test -noshell
main(_) ->
    code:add_patha("_build/concuerror/lib/cre/ebin"),
    code:add_patha("_build/concuerror/lib/concuerror/ebin"),
    code:add_patha("_build/concuerror/lib/getopt/ebin"),
    code:add_patha("_build/concuerror+test/lib/cre/test"),
    R = concuerror:run([
        {module, nato_deterrence_concuerror_tests},
        {test, scenario_consensus_publish}
    ]),
    io:format("Concuerror result: ~p~n", [R]),
    halt(case R of ok -> 0; _ -> 1 end).
