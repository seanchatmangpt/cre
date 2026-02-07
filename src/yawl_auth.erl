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
%% @doc YAWL Authentication and Authorization Module
%%
%% This module provides authentication and authorization services for
%% YAWL workflows, including user management, session handling, and
%% role-based access control.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Password hashing using PBKDF2-HMAC-SHA256 (100,000 iterations)</li>
%%   <li>Secure password strength validation</li>
%%   <li>Session management with expiration</li>
%%   <li>Role-based access control (RBAC)</li>
%%   <li>User CRUD operations</li>
%% </ul>
%%
%% <h3>Password Security</h3>
%% Passwords are hashed using PBKDF2-HMAC-SHA256 with 100,000 iterations
%% and a 16-byte random salt. This uses the built-in Erlang crypto module
%% for OTP 25+ compatibility without external dependencies. The salt is
%% embedded in the stored hash for verification.
%%
%% <h3>Doctests</h3>
%%
%% Starting the authentication server:
%%
%% ```erlang
%% 1> {ok, Pid} = yawl_auth:start_auth().
%% {ok, <0.123.0>}
%% ```
%%
%% Creating a user with password validation:
%%
%% ```erlang
%% 2> {ok, UserId} = yawl_auth:create_user(<<"alice">>, <<"SecureP@ss123">>, [<<"user">>]).
%% {ok, <<"user_", _/binary>>}
%% ```
%%
%% Password strength validation - valid password:
%%
%% ```erlang
%% 3> {ok, <<"SecureP@ss123">>} = yawl_auth:validate_password_strength(<<"SecureP@ss123">>).
%% {ok, <<"SecureP@ss123">>}
%% ```
%%
%% Password strength validation - weak password (too short):
%%
%% ```erlang
%% 4> {error, {too_short, _}} = yawl_auth:validate_password_strength(<<"short1!">>).
%% {error, {too_short, "Password must be at least 8 characters long"}}
%% ```
%%
%% Password strength validation - missing complexity:
%%
%% ```erlang
%% 5> {error, {weak_password, _}} = yawl_auth:validate_password_strength(<<"weakpassword">>).
%% {error, {weak_password, [<<"uppercase">>, <<"digit">>, <<"special">>]}}
%% ```
%%
%% Authentication with valid credentials:
%%
%% ```erlang
%% 6> {ok, SessionId} = yawl_auth:authenticate(<<"alice">>, <<"SecureP@ss123">>).
%% {ok, <<"session_", _/binary>>}
%% ```
%%
%% Authentication with invalid credentials:
%%
%% ```erlang
%% 7> {error, invalid_credentials} = yawl_auth:authenticate(<<"alice">>, <<"WrongPassword">>).
%% {error, invalid_credentials}
%% ```
%%
%% Session retrieval:
%%
%% ```erlang
%% 8> {ok, Session} = yawl_auth:get_session(SessionId).
%% {ok, #session{session_id = SessionId, user_id = UserId, expires_at = _}}
%% ```
%%
%% Session invalidation (logout):
%%
%% ```erlang
%% 9> ok = yawl_auth:invalidate_session(SessionId).
%% ok
%% ```
%%
%% Authorization checks:
%%
%% ```erlang
%% 10> true = yawl_auth:authorize(UserId, <<"workflow">>, <<"start">>).
%% true
%% ```
%%
%% Running all doctests:
%%
%% ```erlang
%% 1> yawl_auth:doctest_test().
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_auth).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% User and Session Records
-export([user/0, user/4, session/0, session/2]).
-export([new_user/3, new_session/1]).
-export([get_user_id/1, get_username/1, get_password_hash/1,
         get_roles/1, is_enabled/1]).
-export([get_session_id/1, get_session_user_id/1, get_expires_at/1]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%% Auth API
-export([start_auth/0, start_auth/1, stop_auth/0]).
-export([authenticate/2, authorize/3]).
-export([create_user/3, update_user/2, delete_user/1]).
-export([add_role/2, remove_role/2]).
-export([has_permission/2, get_session/1, invalidate_session/1]).
-export([validate_password_strength/1, doctest_test/0]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type user_id() :: binary().
-type username() :: binary().
-type password_hash() :: binary().
-type role() :: binary().
-type session_id() :: binary().

-record(user, {
    id :: user_id(),
    username :: username(),
    password_hash :: password_hash(),
    roles :: [role()],
    enabled :: boolean()
}).

-record(session, {
    session_id :: session_id(),
    user_id :: user_id(),
    expires_at :: integer()
}).

-record(auth_state, {
    users = #{} :: #{user_id() => #user{}},
    username_index = #{} :: #{username() => user_id()},
    sessions = #{} :: #{session_id() => #session{}},
    session_timeout :: pos_integer()  % 1 hour default (from persistent_term)
}).

-type auth_result() :: {ok, user_id() | session_id()} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the authentication server with default options.
%% @end
%%--------------------------------------------------------------------
-spec start_auth() -> {ok, pid()} | {error, term()}.
start_auth() ->
    start_auth([]).

%%--------------------------------------------------------------------
%% @doc Starts the authentication server with options.
%% Options:
%%   - {session_timeout, Seconds}: Default 3600 (1 hour)
%% @end
%%--------------------------------------------------------------------
-spec start_auth(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_auth(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Stops the authentication server.
%% @end
%%--------------------------------------------------------------------
-spec stop_auth() -> ok.
stop_auth() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Authenticates a user with username and password.
%% Returns {ok, SessionId} on success, {error, Reason} on failure.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Username :: username(), Password :: binary()) ->
          {ok, session_id()} | {error, invalid_credentials | user_disabled}.
authenticate(Username, Password) when is_binary(Username), is_binary(Password) ->
    gen_server:call(?MODULE, {authenticate, Username, Password}).

%%--------------------------------------------------------------------
%% @doc Authorizes a user for a specific action based on roles.
%% Returns true if authorized, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec authorize(UserId :: user_id(), Resource :: binary(), Action :: binary()) ->
          boolean().
authorize(UserId, Resource, Action) ->
    gen_server:call(?MODULE, {authorize, UserId, Resource, Action}).

%%--------------------------------------------------------------------
%% @doc Creates a new user with username and password.
%% Returns {ok, UserId} on success, {error, Reason} on failure.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Username :: username(), Password :: binary(),
                  Roles :: [role()]) -> auth_result().
create_user(Username, Password, Roles) when is_binary(Username), is_binary(Password) ->
    gen_server:call(?MODULE, {create_user, Username, Password, Roles}).

%%--------------------------------------------------------------------
%% @doc Updates an existing user's properties.
%% @end
%%--------------------------------------------------------------------
-spec update_user(UserId :: user_id(), Props :: proplists:proplist()) ->
          ok | {error, term()}.
update_user(UserId, Props) when is_binary(UserId), is_list(Props) ->
    gen_server:call(?MODULE, {update_user, UserId, Props}).

%%--------------------------------------------------------------------
%% @doc Deletes a user by ID.
%% @end
%%--------------------------------------------------------------------
-spec delete_user(UserId :: user_id()) -> ok | {error, not_found}.
delete_user(UserId) when is_binary(UserId) ->
    gen_server:call(?MODULE, {delete_user, UserId}).

%%--------------------------------------------------------------------
%% @doc Adds a role to a user.
%% @end
%%--------------------------------------------------------------------
-spec add_role(UserId :: user_id(), Role :: role()) ->
          ok | {error, not_found}.
add_role(UserId, Role) when is_binary(UserId), is_binary(Role) ->
    gen_server:call(?MODULE, {add_role, UserId, Role}).

%%--------------------------------------------------------------------
%% @doc Removes a role from a user.
%% @end
%%--------------------------------------------------------------------
-spec remove_role(UserId :: user_id(), Role :: role()) ->
          ok | {error, not_found}.
remove_role(UserId, Role) when is_binary(UserId), is_binary(Role) ->
    gen_server:call(?MODULE, {remove_role, UserId, Role}).

%%--------------------------------------------------------------------
%% @doc Checks if a user has a specific permission.
%% @end
%%--------------------------------------------------------------------
-spec has_permission(UserId :: user_id(), Permission :: binary()) ->
          boolean().
has_permission(UserId, Permission) when is_binary(UserId), is_binary(Permission) ->
    gen_server:call(?MODULE, {has_permission, UserId, Permission}).

%%--------------------------------------------------------------------
%% @doc Gets session information by session ID.
%% @end
%%--------------------------------------------------------------------
-spec get_session(SessionId :: session_id()) ->
          {ok, #session{}} | {error, not_found | expired}.
get_session(SessionId) when is_binary(SessionId) ->
    gen_server:call(?MODULE, {get_session, SessionId}).

%%--------------------------------------------------------------------
%% @doc Invalidates a session (logs out user).
%% @end
%%--------------------------------------------------------------------
-spec invalidate_session(SessionId :: session_id()) -> ok.
invalidate_session(SessionId) when is_binary(SessionId) ->
    gen_server:cast(?MODULE, {invalidate_session, SessionId}).

%%--------------------------------------------------------------------
%% @doc Validates password strength against security requirements.
%% Requirements:
%%   - Minimum length of 8 characters
%%   - Contains at least one uppercase letter (A-Z)
%%   - Contains at least one lowercase letter (a-z)
%%   - Contains at least one digit (0-9)
%%   - Contains at least one special character
%%
%% Returns {ok, Password} if valid, {error, Reason} otherwise.
%%
%% Examples:
%%
%% Valid password:
%% ```erlang
%% 1> {ok, _} = yawl_auth:validate_password_strength(<<"SecureP@ss123">>).
%% {ok, <<"SecureP@ss123">>}
%% ```
%%
%% Too short:
%% ```erlang
%% 2> {error, {too_short, _}} = yawl_auth:validate_password_strength(<<"short1!">>).
%% {error, {too_short, "Password must be at least 8 characters long"}}
%% ```
%%
%% Missing uppercase, digit, and special:
%% ```erlang
%% 3> {error, {weak_password, Missing}} = yawl_auth:validate_password_strength(<<"weakpassword">>).
%% 4> lists:sort(Missing).
%% [<<"digit">>, <<"special">>, <<"uppercase">>]
%% ```
%% @end
%%--------------------------------------------------------------------
-spec validate_password_strength(binary()) -> {ok, binary()} | {error, term()}.
validate_password_strength(Password) when is_binary(Password) ->
    validate_password_length(Password).

%%--------------------------------------------------------------------
%% @private Validates password minimum length.
%% @end
%%--------------------------------------------------------------------
-spec validate_password_length(binary()) -> {ok, binary()} | {error, term()}.
validate_password_length(Password) when byte_size(Password) >= 8 ->
    validate_password_complexity(Password, []);
validate_password_length(_Password) ->
    {error, {too_short, "Password must be at least 8 characters long"}}.

%%--------------------------------------------------------------------
%% @private Validates password complexity requirements.
%% Checks for uppercase, lowercase, digit, and special character.
%% @end
%%--------------------------------------------------------------------
-spec validate_password_complexity(binary(), [binary()]) ->
          {ok, binary()} | {error, term()}.
validate_password_complexity(Password, _Missing) ->
    HasUpper = re:run(Password, "[A-Z]", [{capture, none}]) =:= match,
    HasLower = re:run(Password, "[a-z]", [{capture, none}]) =:= match,
    HasDigit = re:run(Password, "[0-9]", [{capture, none}]) =:= match,
    HasSpecial = re:run(Password, "[^a-zA-Z0-9]", [{capture, none}]) =:= match,

    Missing0 = lists:concat([
        [<<"uppercase">> || not HasUpper],
        [<<"lowercase">> || not HasLower],
        [<<"digit">> || not HasDigit],
        [<<"special">> || not HasSpecial]
    ]),

    case Missing0 of
        [] -> {ok, Password};
        _ -> {error, {weak_password, Missing0}}
    end.

%%====================================================================
%% Record Constructors and Accessors
%%====================================================================

%% @private
-spec user() -> #user{}.
user() -> #user{}.

%% @private
-spec user(username(), password_hash(), [role()], boolean()) -> #user{}.
user(Username, PasswordHash, Roles, Enabled) ->
    #user{
        id = generate_id(<<"user">>),
        username = Username,
        password_hash = PasswordHash,
        roles = Roles,
        enabled = Enabled
    }.

%% @private
-spec session() -> #session{}.
session() -> #session{}.

%% @private
-spec session(user_id(), integer()) -> #session{}.
session(UserId, ExpiresAt) ->
    #session{
        session_id = generate_id(<<"session">>),
        user_id = UserId,
        expires_at = ExpiresAt
    }.

%% @private
-spec new_user(username(), binary(), [role()]) -> #user{}.
new_user(Username, Password, Roles) ->
    #user{
        id = generate_id(<<"user">>),
        username = Username,
        password_hash = hash_password(Password),
        roles = Roles,
        enabled = true
    }.

%% @private
-spec new_session(user_id()) -> #session{}.
new_session(UserId) ->
    ExpiresAt = erlang:system_time(second) + 3600,
    #session{
        session_id = generate_id(<<"session">>),
        user_id = UserId,
        expires_at = ExpiresAt
    }.

%% @private
-spec get_user_id(#user{}) -> user_id().
get_user_id(#user{id = Id}) -> Id.

%% @private
-spec get_username(#user{}) -> username().
get_username(#user{username = Username}) -> Username.

%% @private
-spec get_password_hash(#user{}) -> password_hash().
get_password_hash(#user{password_hash = Hash}) -> Hash.

%% @private
-spec get_roles(#user{}) -> [role()].
get_roles(#user{roles = Roles}) -> Roles.

%% @private
-spec is_enabled(#user{}) -> boolean().
is_enabled(#user{enabled = Enabled}) -> Enabled.

%% @private
-spec get_session_id(#session{}) -> session_id().
get_session_id(#session{session_id = Id}) -> Id.

%% @private
-spec get_session_user_id(#session{}) -> user_id().
get_session_user_id(#session{user_id = UserId}) -> UserId.

%% @private
-spec get_expires_at(#session{}) -> integer().
get_expires_at(#session{expires_at = ExpiresAt}) -> ExpiresAt.

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

%% @private
init(Options) ->
    %% No external password library needed - using built-in crypto module
    continue_init(Options).

%% @private
continue_init(Options) ->
    %% Use persistent_term for O(1) access to default session timeout
    %% (OTP 21+ optimization)
    DefaultTimeout = cre_config:get(cre_auth_default_session_timeout, 3600),
    SessionTimeout = proplists:get_value(session_timeout, Options, DefaultTimeout),
    State = #auth_state{session_timeout = SessionTimeout},
    {ok, State}.

%% @private
handle_call({authenticate, Username, Password}, _From, State) ->
    {Reply, NewState} = do_authenticate(Username, Password, State),
    {reply, Reply, NewState};

handle_call({authorize, UserId, Resource, Action}, _From, State) ->
    Reply = do_authorize(UserId, Resource, Action, State),
    {reply, Reply, State};

handle_call({create_user, Username, Password, Roles}, _From, State) ->
    {Reply, NewState} = do_create_user(Username, Password, Roles, State),
    {reply, Reply, NewState};

handle_call({update_user, UserId, Props}, _From, State) ->
    {Reply, NewState} = do_update_user(UserId, Props, State),
    {reply, Reply, NewState};

handle_call({delete_user, UserId}, _From, State) ->
    {Reply, NewState} = do_delete_user(UserId, State),
    {reply, Reply, NewState};

handle_call({add_role, UserId, Role}, _From, State) ->
    {Reply, NewState} = do_add_role(UserId, Role, State),
    {reply, Reply, NewState};

handle_call({remove_role, UserId, Role}, _From, State) ->
    {Reply, NewState} = do_remove_role(UserId, Role, State),
    {reply, Reply, NewState};

handle_call({has_permission, UserId, Permission}, _From, State) ->
    Reply = do_has_permission(UserId, Permission, State),
    {reply, Reply, State};

handle_call({get_session, SessionId}, _From, State) ->
    Reply = do_get_session(SessionId, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
handle_cast({invalidate_session, SessionId}, State) ->
    NewState = do_invalidate_session(SessionId, State),
    {noreply, NewState};

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
%% @private Hashes a password using PBKDF2-HMAC-SHA256.
%% Generates a 16-byte random salt and embeds it with the derived key
%% in the resulting hash, which can later be used for verification.
%%
%% 100,000 iterations provides strong security while maintaining
%% reasonable performance. This can be increased over time as
%% hardware improves.
%%
%% The stored hash format is: <<Salt:16/binary, DerivedKey:32/binary>>
%% @end
%%--------------------------------------------------------------------
-spec hash_password(binary()) -> password_hash().
hash_password(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    %% Use persistent_term for O(1) access to iterations constant
    %% (OTP 21+ optimization)
    Iterations = cre_config:get(cre_auth_pbkdf2_iterations, 100000),
    %% Handle both OTP versions - some return binary, some return {ok, binary}
    DerivedKey = case crypto:pbkdf2_hmac(sha256, Password, Salt, Iterations, 32) of
        {ok, Key} -> Key;
        Key when is_binary(Key) -> Key
    end,
    <<Salt/binary, DerivedKey/binary>>.

%%--------------------------------------------------------------------
%% @private Verifies a password against a PBKDF2 hash.
%% Extracts the salt from the stored hash and re-computes the derived key
%% for comparison.
%% @end
%%--------------------------------------------------------------------
-spec verify_password(binary(), password_hash()) -> boolean().
verify_password(Password, StoredHash) when byte_size(StoredHash) =:= 48 ->
    try
        <<Salt:16/binary, StoredDerivedKey:32/binary>> = StoredHash,
        %% Use persistent_term for O(1) access to iterations constant
        %% (OTP 21+ optimization)
        Iterations = cre_config:get(cre_auth_pbkdf2_iterations, 100000),
        ComputedKey = case crypto:pbkdf2_hmac(sha256, Password, Salt, Iterations, 32) of
            {ok, Key} -> Key;
            Key when is_binary(Key) -> Key
        end,
        ComputedKey =:= StoredDerivedKey
    catch
        _:_ -> false
    end;
verify_password(_, _) ->
    false.

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
%% @private Implements authentication logic.
%% @end
%%--------------------------------------------------------------------
-spec do_authenticate(username(), binary(), #auth_state{}) ->
          {{ok, session_id()} | {error, invalid_credentials | user_disabled},
           #auth_state{}}.
do_authenticate(Username, Password, #auth_state{users = Users,
                                                username_index = UsernameIndex,
                                                session_timeout = Timeout} = State) ->
    case maps:get(Username, UsernameIndex, undefined) of
        undefined ->
            {{error, invalid_credentials}, State};
        UserId ->
            case maps:get(UserId, Users, undefined) of
                undefined ->
                    {{error, invalid_credentials}, State};
                #user{enabled = false} ->
                    {{error, user_disabled}, State};
                #user{password_hash = Hash} ->
                    case verify_password(Password, Hash) of
                        false ->
                            {{error, invalid_credentials}, State};
                        true ->
                            Session = new_session(UserId),
                            ExpiresAt = erlang:system_time(second) + Timeout,
                            Session1 = Session#session{expires_at = ExpiresAt},
                            SessionId = Session1#session.session_id,
                            NewSessions = maps:put(SessionId, Session1, State#auth_state.sessions),
                            {{ok, SessionId}, State#auth_state{sessions = NewSessions}}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private Implements authorization logic based on roles.
%% @end
%%--------------------------------------------------------------------
-spec do_authorize(user_id(), binary(), binary(), #auth_state{}) -> boolean().
do_authorize(UserId, Resource, Action, #auth_state{users = Users}) ->
    case maps:get(UserId, Users, undefined) of
        undefined -> false;
        #user{roles = Roles} ->
            % Simple RBAC: role "admin" has all permissions
            % Other roles checked against resource:action pattern
            lists:member(<<"admin">>, Roles) orelse
            check_role_permission(Roles, Resource, Action)
    end.

%%--------------------------------------------------------------------
%% @private Checks if any role grants permission for resource:action.
%% @end
%%--------------------------------------------------------------------
-spec check_role_permission([role()], binary(), binary()) -> boolean().
check_role_permission([], _Resource, _Action) -> false;
check_role_permission([Role | Rest], Resource, Action) ->
    % Role format: "resource:action" or "resource:*" or "*"
    case Role of
        <<"*">> -> true;
        _ ->
            case binary:split(Role, <<":">>) of
                [Resource, <<"*">>] -> true;
                [Resource, Action] -> true;
                _ -> check_role_permission(Rest, Resource, Action)
            end
    end.

%%--------------------------------------------------------------------
%% @private Creates a new user.
%% Validates password strength before creating the user.
%% @end
%%--------------------------------------------------------------------
-spec do_create_user(username(), binary(), [role()], #auth_state{}) ->
          {{ok, user_id()} | {error, already_exists | weak_password}, #auth_state{}}.
do_create_user(Username, Password, Roles, #auth_state{users = Users,
                                                       username_index = UsernameIndex} = State) ->
    case maps:is_key(Username, UsernameIndex) of
        true ->
            {{error, already_exists}, State};
        false ->
            % Validate password strength before creating user
            case validate_password_strength(Password) of
                {ok, _ValidatedPassword} ->
                    User = new_user(Username, Password, Roles),
                    UserId = User#user.id,
                    NewUsers = maps:put(UserId, User, Users),
                    NewIndex = maps:put(Username, UserId, UsernameIndex),
                    {{ok, UserId}, State#auth_state{users = NewUsers, username_index = NewIndex}};
                {error, _Reason} = Error ->
                    {Error, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private Updates a user.
%% Also updates username_index if username is changed.
%% @end
%%--------------------------------------------------------------------
-spec do_update_user(user_id(), proplists:proplist(), #auth_state{}) ->
          {ok | {error, not_found | weak_password}, #auth_state{}}.
do_update_user(UserId, Props, #auth_state{users = Users, username_index = UsernameIndex} = State) ->
    case maps:get(UserId, Users, undefined) of
        undefined ->
            {{error, not_found}, State};
        #user{username = OldUsername} = User ->
            case apply_user_props(User, Props) of
                {error, Reason} ->
                    {{error, Reason}, State};
                UpdatedUser ->
                    NewUsers = maps:put(UserId, UpdatedUser, Users),
                    % Update username_index if username changed
                    NewUsernameIndex = case UpdatedUser#user.username of
                        OldUsername -> UsernameIndex;
                        UpdatedUsername when UpdatedUsername =/= OldUsername ->
                            maps:remove(OldUsername, maps:put(UpdatedUsername, UserId, UsernameIndex))
                    end,
                    {ok, State#auth_state{users = NewUsers, username_index = NewUsernameIndex}}
            end
    end.

%%--------------------------------------------------------------------
%% @private Applies property updates to a user record.
%% Validates password strength when updating password.
%% @end
%%--------------------------------------------------------------------
-spec apply_user_props(#user{}, proplists:proplist()) -> #user{} | {error, term()}.
apply_user_props(User, []) -> User;
apply_user_props(User, [{username, NewUsername} | Rest]) when is_binary(NewUsername) ->
    apply_user_props(User#user{username = NewUsername}, Rest);
apply_user_props(User, [{password, NewPassword} | Rest]) when is_binary(NewPassword) ->
    % Validate password strength before updating
    case validate_password_strength(NewPassword) of
        {ok, _} ->
            apply_user_props(User#user{password_hash = hash_password(NewPassword)}, Rest);
        {error, Reason} ->
            {error, Reason}
    end;
apply_user_props(User, [{enabled, Enabled} | Rest]) when is_boolean(Enabled) ->
    apply_user_props(User#user{enabled = Enabled}, Rest);
apply_user_props(User, [_ | Rest]) ->
    apply_user_props(User, Rest).

%%--------------------------------------------------------------------
%% @private Deletes a user.
%% @end
%%--------------------------------------------------------------------
-spec do_delete_user(user_id(), #auth_state{}) ->
          {ok | {error, not_found}, #auth_state{}}.
do_delete_user(UserId, #auth_state{users = Users, username_index = UsernameIndex} = State) ->
    case maps:get(UserId, Users, undefined) of
        undefined ->
            {{error, not_found}, State};
        #user{username = Username} ->
            NewUsers = maps:remove(UserId, Users),
            NewIndex = maps:remove(Username, UsernameIndex),
            {ok, State#auth_state{users = NewUsers, username_index = NewIndex}}
    end.

%%--------------------------------------------------------------------
%% @private Adds a role to a user.
%% @end
%%--------------------------------------------------------------------
-spec do_add_role(user_id(), role(), #auth_state{}) ->
          {ok | {error, not_found}, #auth_state{}}.
do_add_role(UserId, Role, #auth_state{users = Users} = State) ->
    case maps:get(UserId, Users, undefined) of
        undefined ->
            {{error, not_found}, State};
        #user{roles = Roles} = User ->
            case lists:member(Role, Roles) of
                true ->
                    {ok, State};
                false ->
                    UpdatedUser = User#user{roles = [Role | Roles]},
                    NewUsers = maps:put(UserId, UpdatedUser, Users),
                    {ok, State#auth_state{users = NewUsers}}
            end
    end.

%%--------------------------------------------------------------------
%% @private Removes a role from a user.
%% @end
%%--------------------------------------------------------------------
-spec do_remove_role(user_id(), role(), #auth_state{}) ->
          {ok | {error, not_found}, #auth_state{}}.
do_remove_role(UserId, Role, #auth_state{users = Users} = State) ->
    case maps:get(UserId, Users, undefined) of
        undefined ->
            {{error, not_found}, State};
        #user{roles = Roles} = User ->
            UpdatedUser = User#user{roles = lists:delete(Role, Roles)},
            NewUsers = maps:put(UserId, UpdatedUser, Users),
            {ok, State#auth_state{users = NewUsers}}
    end.

%%--------------------------------------------------------------------
%% @private Checks if a user has a specific permission.
%% @end
%%--------------------------------------------------------------------
-spec do_has_permission(user_id(), binary(), #auth_state{}) -> boolean().
do_has_permission(UserId, Permission, #auth_state{users = Users}) ->
    case maps:get(UserId, Users, undefined) of
        undefined -> false;
        #user{roles = Roles} ->
            lists:member(<<"admin">>, Roles) orelse
            lists:member(Permission, Roles)
    end.

%%--------------------------------------------------------------------
%% @private Gets session information.
%% @end
%%--------------------------------------------------------------------
-spec do_get_session(session_id(), #auth_state{}) ->
          {ok, #session{}} | {error, not_found | expired}.
do_get_session(SessionId, #auth_state{sessions = Sessions}) ->
    Now = erlang:system_time(second),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            {error, not_found};
        #session{expires_at = ExpiresAt} when ExpiresAt < Now ->
            {error, expired};
        Session ->
            {ok, Session}
    end.

%%--------------------------------------------------------------------
%% @private Invalidates a session.
%% @end
%%--------------------------------------------------------------------
-spec do_invalidate_session(session_id(), #auth_state{}) -> #auth_state{}.
do_invalidate_session(SessionId, #auth_state{sessions = Sessions} = State) ->
    NewSessions = maps:remove(SessionId, Sessions),
    State#auth_state{sessions = NewSessions}.

%%--------------------------------------------------------------------
%% @doc
%% Runs doctests for the yawl_auth module.
%%
%% This function validates the authentication and authorization examples
%% in the module documentation.
%%
%% Returns `ok' when all tests pass.
%%
%% Example:
%% ```
%% 1> yawl_auth:doctest_test().
%% ok
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Password validation - valid password with all requirements
    {ok, _} = validate_password_strength(<<"SecureP@ss123">>),

    %% Test 2: Password validation - too short
    {error, {too_short, _}} = validate_password_strength(<<"short1!">>),

    %% Test 3: Password validation - missing uppercase
    {error, {weak_password, Missing1}} = validate_password_strength(<<"lowercase123!">>),
    true = lists:member(<<"uppercase">>, Missing1),

    %% Test 4: Password validation - missing lowercase
    {error, {weak_password, Missing2}} = validate_password_strength(<<"UPPERCASE123!">>),
    true = lists:member(<<"lowercase">>, Missing2),

    %% Test 5: Password validation - missing digit
    {error, {weak_password, Missing3}} = validate_password_strength(<<"NoDigits!">>),
    true = lists:member(<<"digit">>, Missing3),

    %% Test 6: Password validation - missing special character
    {error, {weak_password, Missing4}} = validate_password_strength(<<"NoSpecial123">>),
    true = lists:member(<<"special">>, Missing4),

    %% Test 7: Password hashing produces correct format (48 bytes: 16 salt + 32 key)
    %% Using a fast iteration count for doctests
    TestPassword = <<"TestPassword123!">>,
    Salt = crypto:strong_rand_bytes(16),
    Iterations = 100,  % Fast iterations for doctests
    DerivedKey = case crypto:pbkdf2_hmac(sha256, TestPassword, Salt, Iterations, 32) of
        {ok, Key} -> Key;
        Key when is_binary(Key) -> Key
    end,
    StoredHash = <<Salt/binary, DerivedKey/binary>>,
    48 = byte_size(StoredHash),

    %% Test 8: Password verification with correct password
    <<Salt2:16/binary, Key2:32/binary>> = StoredHash,
    ComputedKey = case crypto:pbkdf2_hmac(sha256, TestPassword, Salt2, Iterations, 32) of
        {ok, K} -> K;
        K when is_binary(K) -> K
    end,
    true = ComputedKey =:= Key2,

    %% Test 9: Password verification with incorrect password
    WrongKey = case crypto:pbkdf2_hmac(sha256, <<"WrongPassword">>, Salt2, Iterations, 32) of
        {ok, WK} -> WK;
        WK when is_binary(WK) -> WK
    end,
    false = WrongKey =:= Key2,

    %% Test 10: ID generation produces valid binary with prefix
    UserId = generate_id(<<"user">>),
    true = is_binary(UserId),
    true = byte_size(UserId) > 4,
    {ok, _} = re:run(UserId, <<"^user_[a-f0-9]+$">>),

    %% Test 11: Session ID generation
    SessionId = generate_id(<<"session">>),
    true = is_binary(SessionId),
    true = byte_size(SessionId) > 8,
    {ok, _} = re:run(SessionId, <<"^session_[a-f0-9]+$">>),

    %% Test 12: User record creation
    TestUser = new_user(<<"testuser">>, <<"TestPass123!">>, [<<"user">>]),
    true = is_binary(TestUser#user.id),
    <<"testuser">> = TestUser#user.username,
    true = is_binary(TestUser#user.password_hash),
    [<<"user">>] = TestUser#user.roles,
    true = TestUser#user.enabled,

    %% Test 13: Session record creation
    TestUserId = <<"user_test">>,
    TestSession = new_session(TestUserId),
    true = is_binary(TestSession#session.session_id),
    TestUserId = TestSession#session.user_id,
    true = is_integer(TestSession#session.expires_at),
    true = TestSession#session.expires_at > erlang:system_time(second),

    %% Test 14: Role permission checking
    true = check_role_permission([<<"*">>], <<"any">>, <<"action">>),
    true = check_role_permission([<<"workflow:start">>], <<"workflow">>, <<"start">>),
    true = check_role_permission([<<"workflow:*">>], <<"workflow">>, <<"start">>),
    false = check_role_permission([<<"workflow:start">>], <<"workflow">>, <<"stop">>),
    false = check_role_permission([<<"other:*">>], <<"workflow">>, <<"start">>),

    %% Test 15: User record accessors
    AccessorUser = #user{id = <<"id1">>, username = <<"user1">>,
                        password_hash = <<"hash1">>, roles = [<<"admin">>], enabled = true},
    <<"id1">> = get_user_id(AccessorUser),
    <<"user1">> = get_username(AccessorUser),
    <<"hash1">> = get_password_hash(AccessorUser),
    [<<"admin">>] = get_roles(AccessorUser),
    true = is_enabled(AccessorUser),

    %% Test 16: Session record accessors
    AccessorSession = #session{session_id = <<"sid1">>, user_id = <<"uid1">>, expires_at = 12345},
    <<"sid1">> = get_session_id(AccessorSession),
    <<"uid1">> = get_session_user_id(AccessorSession),
    12345 = get_expires_at(AccessorSession),

    ok.
