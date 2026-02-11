%%% @doc RDF Ontology Loader and Validator
-module(ggen_rdf).
-export([load/1, validate/1, query/3, export_nquads/2]).
-include_lib("eunit/include/eunit.hrl").

%% Load RDF ontology from directory
%% Input: directory with *.rdf files
%% Output: {ok, Graph} | {error, Reason}
load(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            RdfFiles = [filename:join(Dir, F) || F <- Files, filename:extension(F) == ".rdf"],
            load_files(RdfFiles, []);
        {error, Reason} ->
            {error, {invalid_dir, Reason}}
    end.

load_files([], Acc) ->
    {ok, Acc};
load_files([File | Rest], Acc) ->
    case read_rdf_file(File) of
        {ok, Triples} ->
            load_files(Rest, Acc ++ Triples);
        {error, Reason} ->
            {error, {parse_error, File, Reason}}
    end.

%% Read RDF file (minimal XML parser)
read_rdf_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            {ok, parse_rdf_xml(Content)};
        {error, Reason} ->
            {error, Reason}
    end.

parse_rdf_xml(Content) when is_binary(Content) ->
    %% Simplified: extract rdf:Description elements
    %% For MVP, use regex-based extraction
    ContentStr = binary_to_list(Content),
    extract_descriptions(ContentStr, []).

extract_descriptions(Content, Acc) ->
    %% Extract triples from rdf:Description / rdf:Property elements
    %% Pattern: <rdf:Description rdf:about="..."><ns:property>value</ns:property>...</rdf:Description>
    case extract_next_description(Content) of
        {ok, {Subject, Predicates, Rest}} ->
            Triples = [{Subject, P, V} || {P, V} <- Predicates],
            extract_descriptions(Rest, Acc ++ Triples);
        eof ->
            Acc
    end.

extract_next_description(Content) ->
    case re:run(Content, "<rdf:Description rdf:about=\"([^\"]+)\"", [{capture, [1], list}]) of
        {match, [Subject]} ->
            %% Extract properties from this description
            case re:run(Content, "<rdf:Description.*?</rdf:Description>", [{capture, [0], list}]) of
                {match, [Block]} ->
                    Predicates = extract_properties(Block, []),
                    Rest = string:sub_string(Content, string:str(Content, Block) + length(Block)),
                    {ok, {Subject, Predicates, Rest}};
                nomatch ->
                    eof
            end;
        nomatch ->
            eof
    end.

extract_properties(Block, Acc) ->
    case re:run(Block, "<([^>]+:)?([^>]+)>([^<]+)</", [{capture, [1, 2, 3], list}]) of
        {match, [_NS, Pred, Val]} ->
            extract_properties(Block, Acc ++ [{Pred, Val}]);
        nomatch ->
            Acc
    end.

%% Validate RDF structure
validate(Graph) ->
    %% Check: all subjects have rdf:type
    %% Check: all properties have valid ranges
    case check_typed(Graph, []) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

check_typed([], _Seen) ->
    ok;
check_typed([{S, 'rdf:type', _} | Rest], Seen) ->
    check_typed(Rest, [S | Seen]);
check_typed([{S, _, _} | Rest], Seen) ->
    case lists:member(S, Seen) of
        true -> check_typed(Rest, Seen);
        false -> {error, {untyped_subject, S}}
    end.

%% Query RDF graph (simplified SPARQL-like)
query(Graph, QueryType, Pattern) ->
    case QueryType of
        select -> query_select(Graph, Pattern);
        ask -> query_ask(Graph, Pattern);
        construct -> query_construct(Graph, Pattern)
    end.

query_select(Graph, {Var, Pred, Val}) ->
    Results = [S || {S, P, V} <- Graph, match_pattern(P, Pred), match_pattern(V, Val)],
    {ok, Results}.

query_ask(Graph, {Subj, Pred, Val}) ->
    case lists:any(fun({S, P, V}) ->
        match_pattern(S, Subj), match_pattern(P, Pred), match_pattern(V, Val)
    end, Graph) of
        true -> {ok, true};
        false -> {ok, false}
    end.

query_construct(_Graph, _Pattern) ->
    {ok, []}.  % TODO: implement construct

match_pattern('_', _) -> true;
match_pattern(X, X) -> true;
match_pattern(_, _) -> false.

%% Export as N-Quads (for validation)
export_nquads(Graph, Output) ->
    Lines = [format_nquad(S, P, V) || {S, P, V} <- Graph],
    case file:write_file(Output, string:join(Lines, "\n")) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

format_nquad(S, P, V) ->
    io_lib:format("<~s> <~s> \"~s\" .~n", [S, P, V]).

