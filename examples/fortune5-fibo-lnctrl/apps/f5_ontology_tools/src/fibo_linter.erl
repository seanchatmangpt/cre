%% FIBO Ontology Linter - Validates financial terms against FIBO vocabulary
-module(fibo_linter).
-export([
    lint_file/1,
    lint_file/2,
    check_term/1,
    generate_report/2,
    suggest_fibo_term/1
]).

-include("fibo_linter.hrl").

%% Known FIBO namespaces and their structure
-define(FIBO_NAMESPACES, #{
    <<"fibo-fnd">> => <<"https://spec.edmcouncil.org/fibo/ontology/FND/">>,
    <<"fibo-be">> => <<"https://spec.edmcouncil.org/fibo/ontology/BE/">>,
    <<"fibo-loan">> => <<"https://spec.edmcouncil.org/fibo/ontology/LOAN/">>,
    <<"fibo-sec">> => <<"https://spec.edmcouncil.org/fibo/ontology/SEC/">>,
    <<"fibo-der">> => <<"https://spec.edmcouncil.org/fibo/ontology/DER/">>,
    <<"fibo-ind">> => <<"https://spec.edmcouncil.org/fibo/ontology/IND/">>,
    <<"fibo-fbc">> => <<"https://spec.edmcouncil.org/fibo/ontology/FBC/">>
}).

%% FIBO term mappings for common financial concepts
-define(FIBO_TERM_MAP, #{
    %% Loan domain
    <<"LoanApplication">> => {<<"fibo-loan">>, <<"LoanContract">>},
    <<"Borrower">> => {<<"fibo-loan">>, <<"Borrower">>},
    <<"Loan">> => {<<"fibo-loan">>, <<"Loan">>},
    <<"LoanContract">> => {<<"fibo-loan">>, <<"LoanContract">>},
    <<"CreditAgreement">> => {<<"fibo-loan">>, <<"LoanContract">>},
    <<"MortgageLoan">> => {<<"fibo-loan">>, <<"MortgageLoan">>},
    <<"ConsumerLoan">> => {<<"fibo-loan">>, <<"ConsumerLoan">>},
    <<"CommercialLoan">> => {<<"fibo-loan">>, <<"CommercialLoan">>},

    %% Foundations domain
    <<"Party">> => {<<"fibo-fnd">>, <<"Party">>},
    <<"Agreement">> => {<<"fibo-fnd">>, <<"Agreement">>},
    <<"Account">> => {<<"fibo-fnd">>, <<"Account">>},
    <<"Person">> => {<<"fibo-fnd">>, <<"Person">>},
    <<"Organization">> => {<<"fibo-fnd">>, <<"Organization">>},
    <<"Contract">> => {<<"fibo-fnd">>, <<"Contract">>},
    <<"Document">> => {<<"fibo-fnd">>, <<"Document">>},

    %% Business Entities domain
    <<"LegalEntity">> => {<<"fibo-be">>, <<"LegalEntity">>},
    <<"FunctionalEntity">> => {<<"fibo-be">>, <<"FunctionalEntity">>},
    <<"Corporation">> => {<<"fibo-be">>, <<"Corporation">>},
    <<"Partnership">> => {<<"fibo-be">>, <<"Partnership">>},

    %% Financial Business and Commerce
    <<"FinancialInstitution">> => {<<"fibo-fbc">>, <<"FinancialInstitution">>},
    <<"Bank">> => {<<"fibo-fbc">>, <<"Bank">>},
    <<"CreditUnion">> => {<<"fibo-fbc">>, <<"CreditUnion">>},
    <<"Lender">> => {<<"fibo-fbc">>, <<"Lender">>}
}).

%% @doc Lint a Turtle ontology file with default options
-spec lint_file(string()) -> {ok, #lint_result{}} | {error, term()}.
lint_file(FilePath) ->
    lint_file(FilePath, #{}).

%% @doc Lint a Turtle ontology file with options
-spec lint_file(string(), map()) -> {ok, #lint_result{}} | {error, term()}.
lint_file(FilePath, _Options) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            Result = analyze_lines(Lines, FilePath),
            {ok, Result};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% @doc Check if a term is FIBO-aligned
-spec check_term(string()) -> {ok, {binary(), binary()}} | {error, not_fibo}.
check_term(Term) when is_list(Term) ->
    check_term(list_to_binary(Term));
check_term(Term) when is_binary(Term) ->
    case maps:get(Term, ?FIBO_TERM_MAP, undefined) of
        undefined -> {error, not_fibo};
        {Namespace, FiboTerm} -> {ok, {Namespace, FiboTerm}}
    end.

%% @doc Suggest FIBO term for custom term
-spec suggest_fibo_term(string()) -> {ok, string()} | {error, no_suggestion}.
suggest_fibo_term(Term) when is_list(Term) ->
    suggest_fibo_term(list_to_binary(Term));
suggest_fibo_term(Term) when is_binary(Term) ->
    %% Extract term name without namespace prefix
    TermName = case binary:split(Term, <<":">>) of
        [_Prefix, Name] -> Name;
        [Name] -> Name
    end,

    case maps:get(TermName, ?FIBO_TERM_MAP, undefined) of
        undefined ->
            %% Try fuzzy matching
            case fuzzy_match(TermName) of
                {ok, Suggestion} -> {ok, Suggestion};
                error -> {error, no_suggestion}
            end;
        {Namespace, FiboTerm} ->
            Suggestion = <<Namespace/binary, ":", FiboTerm/binary>>,
            {ok, binary_to_list(Suggestion)}
    end.

%% @doc Generate FIBO alignment report
-spec generate_report(#lint_result{}, string()) -> ok | {error, term()}.
generate_report(Result, OutputPath) ->
    Report = format_report(Result),
    file:write_file(OutputPath, Report).

%% Internal functions

analyze_lines(Lines, FilePath) ->
    {Terms, Undefined, Warnings} = lists:foldl(
        fun(Line, {TermsAcc, UndefAcc, WarnAcc}) ->
            LineNum = length(TermsAcc) + 1,
            analyze_line(Line, LineNum, TermsAcc, UndefAcc, WarnAcc)
        end,
        {[], [], []},
        Lines
    ),

    #lint_result{
        file = FilePath,
        total_terms = length(Terms),
        fibo_aligned = length(Terms) - length(Undefined),
        undefined_terms = lists:reverse(Undefined),
        warnings = lists:reverse(Warnings),
        timestamp = calendar:universal_time()
    }.

analyze_line(Line, LineNum, Terms, Undefined, Warnings) ->
    %% Match patterns like: f5:SomeTerm a ln:Type
    %% or fibo-loan:Borrower
    case extract_terms(Line) of
        [] -> {Terms, Undefined, Warnings};
        ExtractedTerms ->
            lists:foldl(
                fun(Term, {TAcc, UAcc, WAcc}) ->
                    case classify_term(Term, LineNum) of
                        {fibo_aligned, _} ->
                            {[Term | TAcc], UAcc, WAcc};
                        {custom, Info} ->
                            {[Term | TAcc], [Info | UAcc], WAcc};
                        {warning, Msg} ->
                            {TAcc, UAcc, [Msg | WAcc]}
                    end
                end,
                {Terms, Undefined, Warnings},
                ExtractedTerms
            )
    end.

extract_terms(Line) ->
    %% Extract namespace:term patterns
    case re:run(Line, <<"([a-z0-9_-]+):([A-Z][a-zA-Z0-9_]*)">>,
                [global, {capture, all_but_first, binary}]) of
        {match, Matches} ->
            [{Namespace, Term} || [Namespace, Term] <- Matches];
        nomatch ->
            []
    end.

classify_term({Namespace, Term}, LineNum) ->
    %% Check if namespace is FIBO
    case maps:is_key(Namespace, ?FIBO_NAMESPACES) of
        true ->
            {fibo_aligned, {Namespace, Term}};
        false ->
            %% Custom term - try to suggest FIBO equivalent
            FullTerm = <<Namespace/binary, ":", Term/binary>>,
            Suggestion = case suggest_fibo_term(Term) of
                {ok, Sugg} -> Sugg;
                {error, no_suggestion} -> undefined
            end,

            Info = #term_info{
                term = binary_to_list(FullTerm),
                line = LineNum,
                namespace = binary_to_list(Namespace),
                suggestion = Suggestion
            },
            {custom, Info}
    end.

fuzzy_match(Term) ->
    %% Simple fuzzy matching - check if term contains known keywords
    TermLower = string:lowercase(binary_to_list(Term)),

    Matches = [
        {["loan", "credit", "mortgage"], <<"fibo-loan:Loan">>},
        {["borrow"], <<"fibo-loan:Borrower">>},
        {["party", "person"], <<"fibo-fnd:Party">>},
        {["account"], <<"fibo-fnd:Account">>},
        {["agreement", "contract"], <<"fibo-fnd:Agreement">>},
        {["entity", "legal"], <<"fibo-be:LegalEntity">>},
        {["bank", "institution"], <<"fibo-fbc:FinancialInstitution">>}
    ],

    case find_fuzzy_match(TermLower, Matches) of
        {ok, Match} -> {ok, binary_to_list(Match)};
        error -> error
    end.

find_fuzzy_match(_Term, []) ->
    error;
find_fuzzy_match(Term, [{Keywords, FiboTerm} | Rest]) ->
    case lists:any(fun(Kw) -> string:find(Term, Kw) =/= nomatch end, Keywords) of
        true -> {ok, FiboTerm};
        false -> find_fuzzy_match(Term, Rest)
    end.

format_report(#lint_result{} = Result) ->
    #lint_result{
        file = File,
        total_terms = Total,
        fibo_aligned = Aligned,
        undefined_terms = Undefined,
        warnings = Warnings,
        timestamp = Timestamp
    } = Result,

    {{Y, M, D}, {H, Min, S}} = Timestamp,
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                            [Y, M, D, H, Min, S]),

    Header = io_lib:format(
        "# FIBO Alignment Report\n\n"
        "**File:** ~s\n"
        "**Generated:** ~s\n\n"
        "## Summary\n\n"
        "- **Total Terms:** ~B\n"
        "- **FIBO-Aligned:** ~B (~.1f%)\n"
        "- **Undefined Terms:** ~B\n"
        "- **Warnings:** ~B\n\n",
        [File, DateStr, Total, Aligned,
         (Aligned / max(Total, 1)) * 100,
         length(Undefined), length(Warnings)]
    ),

    UndefinedSection = case Undefined of
        [] ->
            "## Undefined Terms\n\n"
            "All terms are FIBO-aligned! ✓\n\n";
        _ ->
            UndefinedList = lists:map(
                fun(#term_info{term = Term, line = Line, namespace = NS, suggestion = Sugg}) ->
                    SuggStr = case Sugg of
                        undefined -> "No FIBO suggestion available";
                        _ -> io_lib:format("Suggested: `~s`", [Sugg])
                    end,
                    io_lib:format(
                        "### `~s` (Line ~B)\n"
                        "- **Namespace:** `~s`\n"
                        "- **Status:** ⚠️  Needs FIBO alignment\n"
                        "- **Recommendation:** ~s\n\n",
                        [Term, Line, NS, SuggStr]
                    )
                end,
                Undefined
            ),
            ["## Undefined Terms\n\n", UndefinedList]
    end,

    WarningsSection = case Warnings of
        [] -> "";
        _ ->
            WarnList = lists:map(
                fun(Warn) -> io_lib:format("- ~s\n", [Warn]) end,
                Warnings
            ),
            ["## Warnings\n\n", WarnList, "\n"]
    end,

    FiboInfo =
        "## FIBO Namespaces\n\n"
        "The following FIBO namespaces are recognized:\n\n"
        "- `fibo-fnd`: Foundations (Party, Agreement, Account, etc.)\n"
        "- `fibo-be`: Business Entities (LegalEntity, Corporation, etc.)\n"
        "- `fibo-loan`: Loans (Loan, Borrower, LoanContract, etc.)\n"
        "- `fibo-fbc`: Financial Business & Commerce (Bank, Lender, etc.)\n"
        "- `fibo-sec`: Securities\n"
        "- `fibo-der`: Derivatives\n"
        "- `fibo-ind`: Indices & Indicators\n\n"
        "## Recommendations\n\n"
        "1. Update custom terms to use FIBO vocabulary\n"
        "2. Create `ontology/fibo_alignment.ttl` with explicit mappings\n"
        "3. Import FIBO ontologies in your main ontology file\n"
        "4. Use `owl:equivalentClass` for custom term mappings\n\n"
        "## References\n\n"
        "- FIBO Specification: https://spec.edmcouncil.org/fibo/\n"
        "- FIBO on GitHub: https://github.com/edmcouncil/fibo\n"
        "- EDM Council: https://edmcouncil.org/frameworks/industry-models/fibo/\n",

    unicode:characters_to_binary([Header, UndefinedSection, WarningsSection, FiboInfo]).
