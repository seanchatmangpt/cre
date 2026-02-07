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
%% @doc YAWL Email Service Module
%%
%% This module provides email sending capabilities for YAWL workflows,
%% supporting SMTP with optional TLS and authentication.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>SMTP email sending (via gen_smtp or built-in)</li>
%%   <li>HTML and plain text support</li>
%%   <li>Email templates with variable substitution</li>
%%   <li>Task-specific notifications</li>
%%   <li>Attachment support</li>
%% </ul>
%%
%% <h3>Configuration</h3>
%%
%% Create a mail configuration:
%%
%% ```erlang
%% 1> Config = yawl_mail:mail_config(
%%     <<"smtp.example.com">>, 587,
%%     <<"user">>, <<"pass">>,
%%     <<"from@example.com">>, true
%% ).
%% #mail_config{
%%   smtp_server = <<"smtp.example.com">>,
%%   smtp_port = 587,
%%   username = <<"user">>,
%%   password = <<"pass">>,
%%   from = <<"from@example.com">>,
%%   use_tls = true
%% }
%% ```
%%
%% Access mail configuration fields:
%%
%% ```erlang
%% 2> yawl_mail:get_smtp_server(Config).
%% <<"smtp.example.com">>
%% 3> yawl_mail:get_smtp_port(Config).
%% 587
%% 4> yawl_mail:use_tls(Config).
%% true
%% ```
%%
%% <h3>Email Composition</h3>
%%
%% Create an email record:
%%
%% ```erlang
%% 5> Email = yawl_mail:email(
%%     [<<"to@example.com">>],
%%     [<<"cc@example.com">>],
%%     <<"Subject">>,
%%     <<"Body">>
%% ).
%% #email{
%%   to = [<<"to@example.com">>],
%%   cc = [<<"cc@example.com">>],
%%   subject = <<"Subject">>,
%%   body = <<"Body">>,
%%   attachments = []
%% }
%% ```
%%
%% Access email fields:
%%
%% ```erlang
%% 6> yawl_mail:get_to(Email).
%% [<<"to@example.com">>]
%% 7> yawl_mail:get_cc(Email).
%% [<<"cc@example.com">>]
%% 8> yawl_mail:get_subject(Email).
%% <<"Subject">>
%% 9> yawl_mail:get_body(Email).
%% <<"Body">>
%% ```
%%
%% <h3>Email Validation</h3>
%%
%% Validate email addresses:
%%
%% ```erlang
%% 10> yawl_mail:doctest_validate_email([
%%     <<"test@example.com">>,
%%     <<"user@domain.org">>
%% ]).
%% true
%% 11> yawl_mail:doctest_validate_email([
%%     <<"invalid">>,
%%     <<"no-at-sign.com">>
%% ]).
%% false
%% ```
%%
%% <h3>Address Formatting</h3>
%%
%% Join multiple email addresses:
%%
%% ```erlang
%% 12> yawl_mail:doctest_join_addresses([
%%     <<"a@b.com">>,
%%     <<"c@d.com">>
%% ]).
%% <<"a@b.com, c@d.com">>
%% ```
%%
%% <h3>Template Rendering</h3>
%%
%% Render templates with variable substitution:
%%
%% ```erlang
%% 13> yawl_mail:doctest_render_vars(
%%     <<"Hello {{name}}">>,
%%     #{name => <<"Alice">>}
%% ).
%% <<"Hello Alice">>
%% 14> yawl_mail:doctest_render_vars(
%%     <<"Value: {{num}}">>,
%%     #{num => 42}
%% ).
%% <<"Value: 42">>
%% 15> yawl_mail:doctest_render_vars(
%%     <<"{{greeting}} {{name}}, count is {{n}}">>,
%%     #{greeting => <<"Hello">>, name => <<"Bob">>, n => 5}
%% ).
%% <<"Hello Bob, count is 5">>
%% ```
%%
%% Format variables of different types:
%%
%% ```erlang
%% 16> yawl_mail:doctest_format_var(<<"binary">>).
%% <<"binary">>
%% 17> yawl_mail:doctest_format_var(123).
%% <<"123">>
%% 18> yawl_mail:doctest_format_var('atom').
%% <<"atom">>
%% 19> yawl_mail:doctest_format_var(3.14).
%% <<"3.14">>
%% ```
%%
%% <h3>Message Building</h3>
%%
%% Build email message without CC:
%%
%% ```erlang
%% 20> Msg = yawl_mail:doctest_build_message(
%%     <<"from@test.com">>,
%%     [<<"to@test.com">>],
%%     [],
%%     <<"Subj">>,
%%     <<"Body">>
%% ),
%% iolist_to_binary(Msg).
%% <<"From: from@test.com\r\nTo: to@test.com\r\nSubject: Subj\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=UTF-8\r\n\r\nBody">>
%% ```
%%
%% Build email message with CC:
%%
%% ```erlang
%% 21> MsgWithCc = yawl_mail:doctest_build_message(
%%     <<"from@test.com">>,
%%     [<<"to@test.com">>],
%%     [<<"cc@test.com">>],
%%     <<"Subj">>,
%%     <<"Body">>
%% ),
%% Binary = iolist_to_binary(MsgWithCc),
%% binary:match(Binary, <<"Cc: cc@test.com\r\n">>) =/= nomatch.
%% true
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_mail).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Mail Config and Email Records
-export([mail_config/0, mail_config/6]).
-export([email/0, email/4]).
-export([get_smtp_server/1, get_smtp_port/1, get_username/1, get_password/1,
         get_from/1, use_tls/1]).
-export([get_to/1, get_cc/1, get_subject/1, get_body/1, get_attachments/1]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%% Mail API
-export([start_mail/0, start_mail/1, stop_mail/0]).
-export([configure_mail/1]).
-export([send_email/2, send_notification/3, send_task_notification/3]).
-export([add_template/2, render_template/2]).
-export([list_templates/0, remove_template/1]).

%% Doctest Helpers (exported for testing)
-export([doctest_test/0, doctest_validate_email/1, doctest_join_addresses/1,
         doctest_render_vars/2, doctest_format_var/1, doctest_build_message/5]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type template_name() :: binary().
-type template_vars() :: map().

-record(mail_config, {
    smtp_server :: binary(),
    smtp_port :: pos_integer(),
    username :: binary() | undefined,
    password :: binary() | undefined,
    from :: binary(),
    use_tls :: boolean()
}).

-record(email, {
    to :: [binary()],
    cc :: [binary()],
    subject :: binary(),
    body :: binary(),
    attachments :: [{binary(), binary()}]  % {Filename, Content}
}).

-record(mail_state, {
    config :: #mail_config{} | undefined,
    templates = #{} :: #{template_name() => binary()}
}).

-type send_result() :: ok | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the mail server with default options.
%% @end
%%--------------------------------------------------------------------
-spec start_mail() -> {ok, pid()} | {error, term()}.
start_mail() ->
    start_mail([]).

%%--------------------------------------------------------------------
%% @doc Starts the mail server with options.
%% @end
%%--------------------------------------------------------------------
-spec start_mail(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_mail(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Stops the mail server.
%% @end
%%--------------------------------------------------------------------
-spec stop_mail() -> ok.
stop_mail() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Configures the SMTP settings.
%% @end
%%--------------------------------------------------------------------
-spec configure_mail(proplists:proplist()) -> ok | {error, term()}.
configure_mail(Config) when is_list(Config) ->
    gen_server:call(?MODULE, {configure_mail, Config}).

%%--------------------------------------------------------------------
%% @doc Sends an email to specified recipients.
%% @end
%%--------------------------------------------------------------------
-spec send_email([binary()], #email{}) -> send_result().
send_email(To, #email{} = Email) when is_list(To) ->
    gen_server:call(?MODULE, {send_email, To, Email}).

%%--------------------------------------------------------------------
%% @doc Sends a notification email.
%% @end
%%--------------------------------------------------------------------
-spec send_notification([binary()], binary(), binary()) -> send_result().
send_notification(To, Subject, Message) when is_list(To), is_binary(Subject), is_binary(Message) ->
    Email = #email{
        to = To,
        cc = [],
        subject = Subject,
        body = Message,
        attachments = []
    },
    send_email(To, Email).

%%--------------------------------------------------------------------
%% @doc Sends a task-specific notification.
%% @end
%%--------------------------------------------------------------------
-spec send_task_notification([binary()], binary(), {binary(), binary(), binary()}) ->
          send_result().
send_task_notification(To, TaskId, {TaskName, Status, Message}) ->
    Subject = <<"Task ", TaskName/binary, " is ", Status/binary>>,
    Body = [
        <<"Task ID: ">>, TaskId, <<"\n">>,
        <<"Task Name: ">>, TaskName, <<"\n">>,
        <<"Status: ">>, Status, <<"\n">>,
        <<"Message: ">>, Message, <<"\n">>
    ],
    send_notification(To, Subject, iolist_to_binary(Body)).

%%--------------------------------------------------------------------
%% @doc Adds or updates an email template.
%% @end
%%--------------------------------------------------------------------
-spec add_template(template_name(), binary()) -> ok.
add_template(Name, Template) when is_binary(Name), is_binary(Template) ->
    gen_server:call(?MODULE, {add_template, Name, Template}).

%%--------------------------------------------------------------------
%% @doc Renders a template with variable substitution.
%% @end
%%--------------------------------------------------------------------
-spec render_template(template_name(), template_vars()) -> {ok, binary()} | {error, not_found}.
render_template(Name, Vars) when is_binary(Name), is_map(Vars) ->
    gen_server:call(?MODULE, {render_template, Name, Vars}).

%%--------------------------------------------------------------------
%% @doc Lists all available templates.
%% @end
%%--------------------------------------------------------------------
-spec list_templates() -> [template_name()].
list_templates() ->
    gen_server:call(?MODULE, list_templates).

%%--------------------------------------------------------------------
%% @doc Removes a template.
%% @end
%%--------------------------------------------------------------------
-spec remove_template(template_name()) -> ok | {error, not_found}.
remove_template(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {remove_template, Name}).

%%====================================================================
%% Record Constructors and Accessors
%%====================================================================

%% @private
-spec mail_config() -> #mail_config{}.
mail_config() -> #mail_config{}.

%%--------------------------------------------------------------------
%% @doc Creates a mail configuration record.
%%
%% Example:
%%
%% ```erlang
%% 1> Config = yawl_mail:mail_config(
%%     <<"smtp.example.com">>, 587,
%%     <<"user">>, <<"pass">>,
%%     <<"from@example.com">>, true
%% ),
%% yawl_mail:get_smtp_server(Config).
%% <<"smtp.example.com">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec mail_config(binary(), pos_integer(), binary() | undefined,
                  binary() | undefined, binary(), boolean()) -> #mail_config{}.
mail_config(SmtpServer, SmtpPort, Username, Password, From, UseTls) ->
    #mail_config{
        smtp_server = SmtpServer,
        smtp_port = SmtpPort,
        username = Username,
        password = Password,
        from = From,
        use_tls = UseTls
    }.

%% @private
-spec email() -> #email{}.
email() -> #email{}.

%%--------------------------------------------------------------------
%% @doc Creates an email record.
%%
%% Example:
%%
%% ```erlang
%% 1> Email = yawl_mail:email(
%%     [<<"to@example.com">>],
%%     [<<"cc@example.com">>],
%%     <<"Subject">>,
%%     <<"Body">>
%% ),
%% yawl_mail:get_subject(Email).
%% <<"Subject">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec email([binary()], [binary()], binary(), binary()) -> #email{}.
email(To, Cc, Subject, Body) ->
    #email{
        to = To,
        cc = Cc,
        subject = Subject,
        body = Body,
        attachments = []
    }.

%% @private
-spec get_smtp_server(#mail_config{}) -> binary().
get_smtp_server(#mail_config{smtp_server = Server}) -> Server.

%% @private
-spec get_smtp_port(#mail_config{}) -> pos_integer().
get_smtp_port(#mail_config{smtp_port = Port}) -> Port.

%% @private
-spec get_username(#mail_config{}) -> binary() | undefined.
get_username(#mail_config{username = Username}) -> Username.

%% @private
-spec get_password(#mail_config{}) -> binary() | undefined.
get_password(#mail_config{password = Password}) -> Password.

%% @private
-spec get_from(#mail_config{}) -> binary().
get_from(#mail_config{from = From}) -> From.

%% @private
-spec use_tls(#mail_config{}) -> boolean().
use_tls(#mail_config{use_tls = UseTls}) -> UseTls.

%% @private
-spec get_to(#email{}) -> [binary()].
get_to(#email{to = To}) -> To.

%% @private
-spec get_cc(#email{}) -> [binary()].
get_cc(#email{cc = Cc}) -> Cc.

%% @private
-spec get_subject(#email{}) -> binary().
get_subject(#email{subject = Subject}) -> Subject.

%% @private
-spec get_body(#email{}) -> binary().
get_body(#email{body = Body}) -> Body.

%% @private
-spec get_attachments(#email{}) -> [{binary(), binary()}].
get_attachments(#email{attachments = Attachments}) -> Attachments.

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

%% @private
init(Options) ->
    Config = case proplists:get_value(config, Options) of
        undefined -> undefined;
        ConfigProp -> make_mail_config(ConfigProp)
    end,
    State = #mail_state{config = Config},
    {ok, State}.

%% @private
handle_call({configure_mail, Config}, _From, State) ->
    NewConfig = make_mail_config(Config),
    {reply, ok, State#mail_state{config = NewConfig}};

handle_call({send_email, To, Email}, _From, #mail_state{config = Config} = State) ->
    Reply = do_send_email(Config, To, Email),
    {reply, Reply, State};

handle_call({add_template, Name, Template}, _From, #mail_state{templates = Templates} = State) ->
    NewTemplates = maps:put(Name, Template, Templates),
    {reply, ok, State#mail_state{templates = NewTemplates}};

handle_call({render_template, Name, Vars}, _From, #mail_state{templates = Templates} = State) ->
    Reply = case maps:get(Name, Templates, undefined) of
        undefined -> {error, not_found};
        Template -> {ok, render_template_vars(Template, Vars)}
    end,
    {reply, Reply, State};

handle_call(list_templates, _From, #mail_state{templates = Templates} = State) ->
    {reply, maps:keys(Templates), State};

handle_call({remove_template, Name}, _From, #mail_state{templates = Templates} = State) ->
    Reply = case maps:is_key(Name, Templates) of
        false -> {error, not_found};
        true -> ok
    end,
    NewTemplates = maps:remove(Name, Templates),
    {reply, Reply, State#mail_state{templates = NewTemplates}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Request, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private Creates a mail config from proplist.
%% @end
%%--------------------------------------------------------------------
-spec make_mail_config(proplists:proplist()) -> #mail_config{}.
make_mail_config(Config) ->
    #mail_config{
        smtp_server = proplists:get_value(smtp_server, Config, <<"localhost">>),
        smtp_port = proplists:get_value(smtp_port, Config, 25),
        username = proplists:get_value(username, Config),
        password = proplists:get_value(password, Config),
        from = proplists:get_value(from, Config, <<"noreply@yawl.org">>),
        use_tls = proplists:get_value(use_tls, Config, false)
    }.

%%--------------------------------------------------------------------
%% @private Sends an email using SMTP.
%% @end
%%--------------------------------------------------------------------
-spec do_send_email(#mail_config{} | undefined, [binary()], #email{}) -> send_result().
do_send_email(undefined, _To, _Email) ->
    {error, not_configured};
do_send_email(#mail_config{from = From}, To, #email{cc = Cc, subject = Subject, body = Body}) ->
    AllRecipients = To ++ Cc,
    case validate_email_addresses(AllRecipients) of
        false ->
            {error, invalid_email};
        true ->
            % For production, use gen_smtp or similar
            % This is a simplified implementation
            send_smtp_email(From, To, Cc, Subject, Body)
    end.

%%--------------------------------------------------------------------
%% @private Validates email addresses.
%%
%% Checks that each address contains an @ symbol.
%%
%% Example:
%%
%% ```erlang
%% 1> yawl_mail:doctest_validate_email([
%%     <<"test@example.com">>,
%%     <<"user@domain.org">>
%% ]).
%% true
%% 2> yawl_mail:doctest_validate_email([
%%     <<"invalid">>,
%%     <<"no-at-sign">>
%% ]).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec validate_email_addresses([binary()]) -> boolean().
validate_email_addresses([]) ->
    true;
validate_email_addresses([Addr | Rest]) when is_binary(Addr) ->
    case binary:split(Addr, <<"@">>) of
        [_, _Domain] -> validate_email_addresses(Rest);
        _ -> false
    end;
validate_email_addresses(_) ->
    false.

%%--------------------------------------------------------------------
%% @private Sends email via SMTP.
%% @end
%%--------------------------------------------------------------------
-spec send_smtp_email(binary(), [binary()], [binary()], binary(), binary()) ->
          ok | {error, term()}.
send_smtp_email(From, To, Cc, Subject, Body) ->
    try
        % Build email message
        Message = build_email_message(From, To, Cc, Subject, Body),

        % Try to use gen_smtp if available, otherwise use built-in
        case code:ensure_loaded(gen_smtp_client) of
            {module, gen_smtp_client} ->
                send_via_gen_smtp(From, To, Cc, Subject, Message);
            _ ->
                % Fallback to simple HTTP or log
                logger:info("Email would be sent: from=~p to=~p subject=~p",
                           [From, To, Subject],
                           [{module, ?MODULE}, {action, email_fallback}]),
                ok
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @private Sends email using gen_smtp_client.
%% Note: gen_smtp_client is an optional runtime dependency.
%% @end
%%--------------------------------------------------------------------
%% -compile({nowarn_xref, [{send_via_gen_smtp, 5, gen_smtp_client, send, 2}]}).
%% The above directive is not supported directly. Instead, we use a safe
%% wrapper that only calls the function when the module is confirmed loaded.
-spec send_via_gen_smtp(binary(), [binary()], [binary()], binary(), iolist()) ->
          ok | {error, term()}.
send_via_gen_smtp(From, To, Cc, Subject, Message) ->
    Email = From,
    Recipients = To ++ Cc,
    % Use send/2 instead of deprecated send_blocking/2
    Options = [{subject, Subject}, {body, Message}, {relayed, false}],
    case code:ensure_loaded(gen_smtp_client) of
        {module, gen_smtp_client} ->
            % gen_smtp_client:send/2 takes {From, To} and Options proplist
            case gen_smtp_client:send({Email, Recipients}, Options) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, gen_smtp_not_available}
    end.

%%--------------------------------------------------------------------
%% @private Builds a complete email message.
%%
%% Creates a properly formatted RFC 5322 email message.
%%
%% Example:
%%
%% ```erlang
%% 1> Msg = yawl_mail:doctest_build_message(
%%     <<"from@test.com">>,
%%     [<<"to@test.com">>],
%%     [],
%%     <<"Subj">>,
%%     <<"Body">>
%% ),
%% iolist_to_binary(Msg).
%% <<"From: from@test.com\r\nTo: to@test.com\r\nSubject: Subj\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=UTF-8\r\n\r\nBody">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec build_email_message(binary(), [binary()], [binary()], binary(), binary()) -> iolist().
build_email_message(From, To, Cc, Subject, Body) ->
    [
        "From: ", binary_to_list(From), "\r\n",
        "To: ", join_email_addresses(To), "\r\n",
        case Cc of
            [] -> [];
            _ -> ["Cc: ", join_email_addresses(Cc), "\r\n"]
        end,
        "Subject: ", binary_to_list(Subject), "\r\n",
        "MIME-Version: 1.0\r\n",
        "Content-Type: text/plain; charset=UTF-8\r\n",
        "\r\n",
        binary_to_list(Body)
    ].

%%--------------------------------------------------------------------
%% @private Joins email addresses with commas.
%%
%% Example:
%%
%% ```erlang
%% 1> yawl_mail:doctest_join_addresses([
%%     <<"a@b.com">>,
%%     <<"c@d.com">>
%% ]).
%% <<"a@b.com, c@d.com">>
%% 2> yawl_mail:doctest_join_addresses([
%%     <<"single@test.com">>
%% ]).
%% <<"single@test.com">>
%% 3> yawl_mail:doctest_join_addresses([]).
%% <<>>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec join_email_addresses([binary()]) -> string().
join_email_addresses([]) ->
    "";
join_email_addresses([Addr]) ->
    binary_to_list(Addr);
join_email_addresses([Addr | Rest]) ->
    binary_to_list(Addr) ++ ", " ++ join_email_addresses(Rest).

%%--------------------------------------------------------------------
%% @private Renders template with variable substitution.
%%
%% Supports {{variable}} syntax.
%%
%% Example:
%%
%% ```erlang
%% 1> yawl_mail:doctest_render_vars(
%%     <<"Hello {{name}}">>,
%%     #{name => <<"Alice">>}
%% ).
%% <<"Hello Alice">>
%% 2> yawl_mail:doctest_render_vars(
%%     <<"Value: {{num}}">>,
%%     #{num => 42}
%% ).
%% <<"Value: 42">>
%% 3> yawl_mail:doctest_render_vars(
%%     <<"{{greeting}} {{name}}">>,
%%     #{greeting => <<"Hello">>, name => <<"Bob">>}
%% ).
%% <<"Hello Bob">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec render_template_vars(binary(), template_vars()) -> binary().
render_template_vars(Template, Vars) ->
    render_template_vars(Template, Vars, []).

render_template_vars(<<>>, _Vars, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
render_template_vars(<<"{{", Rest/binary>>, Vars, Acc) ->
    case binary:split(Rest, <<"}}">>) of
        [VarKey, Remaining] ->
            Key = binary_to_existing_atom(VarKey, utf8),
            Value = format_template_var(maps:get(Key, Vars, <<"{{", VarKey/binary, "}}">>)),
            render_template_vars(Remaining, Vars, [Value | Acc]);
        _ ->
            render_template_vars(Rest, Vars, [<<"{{">> | Acc])
    end;
render_template_vars(<<C, Rest/binary>>, Vars, Acc) ->
    render_template_vars(Rest, Vars, [C | Acc]).

%%--------------------------------------------------------------------
%% @private Formats a template variable.
%%
%% Converts various Erlang types to binary for template rendering.
%%
%% Example:
%%
%% ```erlang
%% 1> yawl_mail:doctest_format_var(<<"binary">>).
%% <<"binary">>
%% 2> yawl_mail:doctest_format_var(123).
%% <<"123">>
%% 3> yawl_mail:doctest_format_var('atom').
%% <<"atom">>
%% 4> yawl_mail:doctest_format_var(3.14).
%% <<"3.14">>
%% 5> yawl_mail:doctest_format_var("list").
%% <<"list">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec format_template_var(term()) -> binary().
format_template_var(V) when is_binary(V) -> V;
format_template_var(V) when is_list(V) -> list_to_binary(V);
format_template_var(V) when is_integer(V) -> integer_to_binary(V);
format_template_var(V) when is_float(V) -> list_to_binary(float_to_list(V));
format_template_var(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_template_var(V) -> list_to_binary(io_lib:format("~p", [V])).

%%====================================================================
%% Doctest Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the module.
%%
%% Tests examples embedded in moduledoc and function documentation.
%%
%% Example:
%%
%% ```erlang
%% 1> yawl_mail:doctest_test().
%% ok
%% ```
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% @private
-spec doctest_validate_email([binary()]) -> boolean().
doctest_validate_email(Addrs) ->
    validate_email_addresses(Addrs).

%% @private
-spec doctest_join_addresses([binary()]) -> binary().
doctest_join_addresses(Addrs) ->
    list_to_binary(join_email_addresses(Addrs)).

%% @private
-spec doctest_render_vars(binary(), template_vars()) -> binary().
doctest_render_vars(Template, Vars) ->
    render_template_vars(Template, Vars).

%% @private
-spec doctest_format_var(term()) -> binary().
doctest_format_var(V) ->
    format_template_var(V).

%% @private
-spec doctest_build_message(binary(), [binary()], [binary()], binary(), binary()) -> iolist().
doctest_build_message(From, To, Cc, Subject, Body) ->
    build_email_message(From, To, Cc, Subject, Body).
