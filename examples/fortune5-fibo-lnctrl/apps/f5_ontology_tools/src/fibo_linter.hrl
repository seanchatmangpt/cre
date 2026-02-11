%% Shared header file for FIBO linter

-record(term_info, {
    term :: string(),
    line :: non_neg_integer(),
    namespace :: string(),
    suggestion :: string() | undefined
}).

-record(lint_result, {
    file :: string(),
    total_terms :: non_neg_integer(),
    fibo_aligned :: non_neg_integer(),
    undefined_terms :: [#term_info{}],
    warnings :: [string()],
    timestamp :: calendar:datetime()
}).
