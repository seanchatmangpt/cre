%%====================================================================
%% cre_ssl_opts.erl
%%
%% Centralized hardened TLS options for all httpc HTTPS calls in CRE.
%%
%% Default httpc TLS behavior is `verify_none' (no certificate chain
%% validation, no hostname check). Every HTTPS call must pass an
%% explicit ssl option list from this module so that:
%%   - the server certificate chain is validated against the OTP CA
%%     bundle (`public_key:cacerts_get()', OTP 25+);
%%   - the certificate hostname is verified per RFC 6125 (https).
%%
%% SSL options are inert for plain-HTTP requests, so callers may apply
%% the profile unconditionally.
%%====================================================================
-module(cre_ssl_opts).

-export([httpc_options/0, httpc_options/1, ssl_options/0]).

%%--------------------------------------------------------------------
%% @doc Default httpc HTTPOptions carrying the hardened SSL profile.
%% Prepend these to any other httpc options a call site needs.
%% @end
%%--------------------------------------------------------------------
-spec httpc_options() -> list().
httpc_options() ->
    [{ssl, ssl_options()}].

%%--------------------------------------------------------------------
%% @doc httpc HTTPOptions with the hardened SSL profile plus Extra
%% httpc options (e.g. timeouts), hardened profile first.
%% @end
%%--------------------------------------------------------------------
-spec httpc_options(list()) -> list().
httpc_options(Extra) when is_list(Extra) ->
    [{ssl, ssl_options()} | Extra].

%%--------------------------------------------------------------------
%% @doc The hardened SSL option list (OTP 25+):
%% verify_peer + OTP CA bundle + RFC 6125 https hostname check.
%% @end
%%--------------------------------------------------------------------
-spec ssl_options() -> list().
ssl_options() ->
    [{verify, verify_peer},
     {cacerts, public_key:cacerts_get()},
     {customize_hostname_check,
      [{match_https, public_key:pkix_verify_hostname_match_fun(https)}]}].
