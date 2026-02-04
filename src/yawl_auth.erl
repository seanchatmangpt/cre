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
%%   <li>Password hashing using bcrypt (work factor 12)</li>
%%   <li>Secure password strength validation</li>
%%   <li>Session management with expiration</li>
%%   <li>Role-based access control (RBAC)</li>
%%   <li>User CRUD operations</li>
%% </ul>
%%
%% <h3>Password Security</h3>
%% Passwords are hashed using bcrypt with a work factor of 12, which
%% provides approximately 2^12 iterations. The bcrypt library automatically
%% handles salt generation and inclusion in the hash output.
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
-export([validate_password_strength/1]).

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
    session_timeout = 3600 :: pos_integer()  % 1 hour default
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
    %% Initialize bcrypt library (handle if already started)
    case bcrypt:start() of
        ok ->
            continue_init(Options);
        {error, {already_started, bcrypt}} ->
            continue_init(Options);
        Error ->
            {stop, {bcrypt_init_failed, Error}}
    end.

%% @private
continue_init(Options) ->
    SessionTimeout = proplists:get_value(session_timeout, Options, 3600),
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
%% @private Hashes a password using bcrypt with work factor 12.
%% The bcrypt library automatically generates a salt and embeds it
%% in the resulting hash, which can later be used for verification.
%%
%% Work factor 12 provides approximately 2^12 = 4096 iterations,
%% which is considered secure as of 2024. This can be increased
%% over time as hardware improves.
%% @end
%%--------------------------------------------------------------------
-spec hash_password(binary()) -> password_hash().
hash_password(Password) ->
    {ok, Salt} = bcrypt:gen_salt(12),
    case bcrypt:hashpw(Password, Salt) of
        {ok, Hash} -> Hash;
        {error, Reason} -> error({bcrypt_hash_failed, Reason})
    end.

%%--------------------------------------------------------------------
%% @private Verifies a password against a bcrypt hash.
%% Uses bcrypt:hashpw/2 which safely re-hashes the password with
%% the salt embedded in the stored hash.
%% @end
%%--------------------------------------------------------------------
-spec verify_password(binary(), password_hash()) -> boolean().
verify_password(Password, StoredHash) ->
    try
        case bcrypt:hashpw(Password, StoredHash) of
            {ok, ComputedHash} -> ComputedHash =:= StoredHash;
            {error, _Reason} -> false
        end
    catch
        _:_ -> false
    end.

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
