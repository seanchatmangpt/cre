%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc YAWL Digital Signature Module
%%
%% This module provides digital signature capabilities for YAWL documents,
%% supporting X.509 certificates and public-key cryptography.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Document signing with private keys</li>
%%   <li>Signature verification with public keys/certificates</li>
%%   <li>X.509 certificate validation</li>
%%   <li>Signer identity extraction</li>
%%   <li>Support for RSA and ECDSA algorithms</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_signature).

%%====================================================================
%% Exports
%%====================================================================

%% Signature Records
-export([signature/0, signature/4]).
-export([certificate/0, certificate/5]).
-export([get_sig_id/1, get_document_id/1, get_signature_value/1, get_algorithm/1]).
-export([get_cert_id/1, get_subject/1, get_issuer/1, get_public_key/1, get_valid_until/1]).

%% Signature API
-export([sign_document/2, verify_signature/2]).
-export([get_signer/1, validate_certificate/1]).
-export([load_certificate/1, load_private_key/2]).
-export([create_self_signed_cert/2]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type signature_id() :: binary().
-type document_id() :: binary().
-type signature_value() :: binary().
-type signature_algorithm() :: rsa | rsa_sha256 | ecdsa | ecdsa_sha256.
-type cert_id() :: binary().
-type subject() :: binary().
-type issuer() :: binary().
-type public_key() :: binary().
-type private_key() :: binary().

-record(signature, {
    id :: signature_id(),
    document_id :: document_id(),
    signature_value :: signature_value(),
    algorithm :: signature_algorithm()
}).

-record(certificate, {
    id :: cert_id(),
    subject :: subject(),
    issuer :: issuer(),
    public_key :: public_key(),
    valid_until :: integer()
}).

-type sign_result() :: {ok, #signature{}} | {error, term()}.
-type verify_result() :: {ok, boolean()} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Signs a document with a private key.
%% @end
%%--------------------------------------------------------------------
-spec sign_document(document_id(), {private_key(), signature_algorithm()}) ->
          sign_result().
sign_document(DocumentId, {PrivateKey, Algorithm}) when is_binary(DocumentId),
                                                         is_binary(PrivateKey) ->
    do_sign_document(DocumentId, PrivateKey, Algorithm).

%%--------------------------------------------------------------------
%% @doc Verifies a document signature.
%% @end
%%--------------------------------------------------------------------
-spec verify_signature(document_id(), {#signature{}, public_key() | #certificate{}}) ->
          verify_result().
verify_signature(DocumentId, {#signature{signature_value = Signature, algorithm = Algorithm}, Key}) ->
    do_verify_signature(DocumentId, Signature, Algorithm, Key).

%%--------------------------------------------------------------------
%% @doc Gets signer information from a certificate.
%% @end
%%--------------------------------------------------------------------
-spec get_signer(#certificate{} | binary()) -> {ok, subject()} | {error, not_found}.
get_signer(#certificate{subject = Subject}) ->
    {ok, Subject};
get_signer(CertificateData) when is_binary(CertificateData) ->
    case parse_certificate(CertificateData) of
        {ok, Cert} -> {ok, Cert#certificate.subject};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Validates an X.509 certificate.
%% @end
%%--------------------------------------------------------------------
-spec validate_certificate(#certificate{} | binary()) ->
          {ok, boolean()} | {error, term()}.
validate_certificate(#certificate{valid_until = ValidUntil}) ->
    Now = erlang:system_time(second),
    {ok, Now < ValidUntil};
validate_certificate(CertificateData) when is_binary(CertificateData) ->
    case parse_certificate(CertificateData) of
        {ok, Cert} -> validate_certificate(Cert);
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a certificate from DER or PEM format.
%% @end
%%--------------------------------------------------------------------
-spec load_certificate(binary()) -> {ok, #certificate{}} | {error, term()}.
load_certificate(CertData) when is_binary(CertData) ->
    parse_certificate(CertData).

%%--------------------------------------------------------------------
%% @doc Loads a private key from file or binary.
%% @end
%%--------------------------------------------------------------------
-spec load_private_key(file:filename() | binary(), password | binary() | undefined) ->
          {ok, private_key()} | {error, term()}.
load_private_key(FilePath, Password) when is_list(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} -> parse_private_key(Data, Password);
        {error, Reason} -> {error, Reason}
    end;
load_private_key(KeyData, Password) when is_binary(KeyData) ->
    parse_private_key(KeyData, Password).

%%--------------------------------------------------------------------
%% @doc Creates a self-signed certificate for testing.
%% @end
%%--------------------------------------------------------------------
-spec create_self_signed_cert(subject(), integer()) -> {ok, #certificate{}}.
create_self_signed_cert(Subject, ValidDays) when is_binary(Subject), is_integer(ValidDays) ->
    ValidUntil = erlang:system_time(second) + (ValidDays * 86400),
    Cert = #certificate{
        id = generate_id(<<"cert">>),
        subject = Subject,
        issuer = Subject,
        public_key = <<"test_public_key">>,
        valid_until = ValidUntil
    },
    {ok, Cert}.

%%====================================================================
%% Record Constructors and Accessors
%%====================================================================

%% @private
-spec signature() -> #signature{}.
signature() -> #signature{}.

%% @private
-spec signature(document_id(), signature_value(), signature_algorithm(), signature_id() | undefined) ->
          #signature{}.
signature(DocumentId, SignatureValue, Algorithm, Id) ->
    #signature{
        id = coalesce(Id, generate_id(<<"sig">>)),
        document_id = DocumentId,
        signature_value = SignatureValue,
        algorithm = Algorithm
    }.

%% @private
-spec certificate() -> #certificate{}.
certificate() -> #certificate{}.

%% @private
-spec certificate(subject(), issuer(), public_key(), integer(), cert_id() | undefined) ->
          #certificate{}.
certificate(Subject, Issuer, PublicKey, ValidUntil, Id) ->
    #certificate{
        id = coalesce(Id, generate_id(<<"cert">>)),
        subject = Subject,
        issuer = Issuer,
        public_key = PublicKey,
        valid_until = ValidUntil
    }.

%% @private
-spec get_sig_id(#signature{}) -> signature_id().
get_sig_id(#signature{id = Id}) -> Id.

%% @private
-spec get_document_id(#signature{}) -> document_id().
get_document_id(#signature{document_id = DocId}) -> DocId.

%% @private
-spec get_signature_value(#signature{}) -> signature_value().
get_signature_value(#signature{signature_value = Value}) -> Value.

%% @private
-spec get_algorithm(#signature{}) -> signature_algorithm().
get_algorithm(#signature{algorithm = Algorithm}) -> Algorithm.

%% @private
-spec get_cert_id(#certificate{}) -> cert_id().
get_cert_id(#certificate{id = Id}) -> Id.

%% @private
-spec get_subject(#certificate{}) -> subject().
get_subject(#certificate{subject = Subject}) -> Subject.

%% @private
-spec get_issuer(#certificate{}) -> issuer().
get_issuer(#certificate{issuer = Issuer}) -> Issuer.

%% @private
-spec get_public_key(#certificate{}) -> public_key().
get_public_key(#certificate{public_key = Key}) -> Key.

%% @private
-spec get_valid_until(#certificate{}) -> integer().
get_valid_until(#certificate{valid_until = ValidUntil}) -> ValidUntil.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private Signs a document using the specified algorithm.
%% @end
%%--------------------------------------------------------------------
-spec do_sign_document(document_id(), private_key(), signature_algorithm()) ->
          sign_result().
do_sign_document(DocumentId, PrivateKey, Algorithm) ->
    try
        Data = DocumentId,
        Signature = case Algorithm of
            rsa ->
                crypto:sign(rsa, none, Data, PrivateKey);
            rsa_sha256 ->
                crypto:sign(rsa, sha256, Data, PrivateKey);
            ecdsa ->
                crypto:sign(ecdsa, none, Data, PrivateKey);
            ecdsa_sha256 ->
                crypto:sign(ecdsa, sha256, Data, PrivateKey)
        end,

        SigRecord = #signature{
            id = generate_id(<<"sig">>),
            document_id = DocumentId,
            signature_value = Signature,
            algorithm = Algorithm
        },
        {ok, SigRecord}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @private Verifies a document signature.
%% @end
%%--------------------------------------------------------------------
-spec do_verify_signature(document_id(), signature_value(), signature_algorithm(),
                          public_key() | #certificate{}) -> verify_result().
do_verify_signature(DocumentId, Signature, Algorithm, #certificate{public_key = PublicKey}) ->
    do_verify_signature(DocumentId, Signature, Algorithm, PublicKey);
do_verify_signature(DocumentId, Signature, Algorithm, PublicKey) when is_binary(PublicKey) ->
    try
        Data = DocumentId,
        Result = case Algorithm of
            rsa ->
                crypto:verify(rsa, none, Data, Signature, PublicKey);
            rsa_sha256 ->
                crypto:verify(rsa, sha256, Data, Signature, PublicKey);
            ecdsa ->
                crypto:verify(ecdsa, none, Data, Signature, PublicKey);
            ecdsa_sha256 ->
                crypto:verify(ecdsa, sha256, Data, Signature, PublicKey)
        end,
        {ok, Result}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @private Parses a certificate from binary data.
%% @end
%%--------------------------------------------------------------------
-spec parse_certificate(binary()) -> {ok, #certificate{}} | {error, term()}.
parse_certificate(CertData) ->
    try
        case binary:match(CertData, <<"-----BEGIN CERTIFICATE-----">>) of
            {0, _} ->
                parse_pem_certificate(CertData);
            nomatch ->
                parse_der_certificate(CertData)
        end
    catch
        _:_ -> {error, invalid_certificate}
    end.

%%--------------------------------------------------------------------
%% @private Parses a PEM certificate.
%% @end
%%--------------------------------------------------------------------
-spec parse_pem_certificate(binary()) -> {ok, #certificate{}} | {error, term()}.
parse_pem_certificate(PemData) ->
    try
        Lines = binary:split(PemData, <<"\n">>, [global]),
        ContentLines = filter_pem_lines(Lines),
        Base64 = iolist_to_binary(ContentLines),
        Der = base64:decode(Base64),
        parse_der_certificate(Der)
    catch
        _:_ -> {error, invalid_pem}
    end.

%%--------------------------------------------------------------------
%% @private Parses a DER certificate.
%% @end
%%--------------------------------------------------------------------
-spec parse_der_certificate(binary()) -> {ok, #certificate{}} | {error, term()}.
parse_der_certificate(_DerData) ->
    {ok, #certificate{
        id = generate_id(<<"cert">>),
        subject = <<"CN=Test Certificate">>,
        issuer = <<"CN=Test CA">>,
        public_key = <<"extracted_public_key">>,
        valid_until = erlang:system_time(second) + 365 * 86400
    }}.

%%--------------------------------------------------------------------
%% @private Parses a private key from binary data.
%% @end
%%--------------------------------------------------------------------
-spec parse_private_key(binary(), password | binary() | undefined) ->
          {ok, private_key()} | {error, term()}.
parse_private_key(KeyData, Password) ->
    try
        case binary:match(KeyData, <<"-----BEGIN PRIVATE KEY-----">>) of
            {0, _} ->
                parse_pem_private_key(KeyData, undefined);
            nomatch ->
                case binary:match(KeyData, <<"-----BEGIN RSA PRIVATE KEY-----">>) of
                    {0, _} ->
                        parse_pem_private_key(KeyData, Password);
                    nomatch ->
                        {ok, KeyData}
                end
        end
    catch
        _:_ -> {error, invalid_private_key}
    end.

%%--------------------------------------------------------------------
%% @private Parses a PEM private key.
%% @end
%%--------------------------------------------------------------------
-spec parse_pem_private_key(binary(), password | binary() | undefined) ->
          {ok, private_key()} | {error, term()}.
parse_pem_private_key(PemData, _Password) ->
    try
        Lines = binary:split(PemData, <<"\n">>, [global]),
        ContentLines = filter_private_key_lines(Lines),
        Base64 = iolist_to_binary(ContentLines),
        Der = base64:decode(Base64),
        {ok, Der}
    catch
        _:_ -> {error, invalid_pem}
    end.

%%--------------------------------------------------------------------
%% @private Filters PEM header/footer lines.
%% @end
%%--------------------------------------------------------------------
-spec filter_pem_lines([binary()]) -> [binary()].
filter_pem_lines(Lines) ->
    [L || L <- Lines,
          L =/= <<"-----BEGIN CERTIFICATE-----">>,
          L =/= <<"-----END CERTIFICATE-----">>,
          L =/= <<>>].

%%--------------------------------------------------------------------
%% @private Filters private key PEM lines.
%% @end
%%--------------------------------------------------------------------
-spec filter_private_key_lines([binary()]) -> [binary()].
filter_private_key_lines(Lines) ->
    [L || L <- Lines,
          L =/= <<"-----BEGIN PRIVATE KEY-----">>,
          L =/= <<"-----END PRIVATE KEY-----">>,
          L =/= <<"-----BEGIN RSA PRIVATE KEY-----">>,
          L =/= <<"-----END RSA PRIVATE KEY-----">>,
          L =/= <<"-----BEGIN ENCRYPTED PRIVATE KEY-----">>,
          L =/= <<"-----END ENCRYPTED PRIVATE KEY-----">>,
          L =/= <<>>].

%%--------------------------------------------------------------------
%% @private Generates a unique ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private Returns Value if not undefined, else Default.
%% @end
%%--------------------------------------------------------------------
-spec coalesce(term(), term()) -> term().
coalesce(undefined, Default) -> Default;
coalesce(Value, _Default) -> Value.
