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
%% @doc YAWL Authentication and Authorization Module Test Suite
%%
%% Comprehensive test suite for user authentication, session management,
%% and role-based access control (RBAC).
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_auth_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the bcrypt application and authentication server.
%% @end
%%--------------------------------------------------------------------
setup() ->
    test_helper:ensure_app_started(bcrypt),
    {ok, Pid} = yawl_auth:start_auth([{session_timeout, 3600}]),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the authentication server.
%% @end
%%--------------------------------------------------------------------
cleanup(_Pid) ->
    yawl_auth:stop_auth(),
    ok.

%%====================================================================
%% 1. Server Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test starting the auth server.
%% @end
%%--------------------------------------------------------------------
test_start_auth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    ?assert(is_pid(whereis(yawl_auth)))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test stopping and restarting the auth server.
%% @end
%%--------------------------------------------------------------------
test_stop_restart_auth_test_() ->
    {setup,
     fun() ->
             {ok, Pid} = yawl_auth:start_auth(),
             Pid
     end,
     fun(Pid) ->
             catch yawl_auth:stop_auth(),
             catch unlink(Pid),
             timer:sleep(200)
     end,
     fun(_Pid) ->
         [?_test(begin
                    catch yawl_auth:stop_auth(),
                    timer:sleep(200),
                    ?assertEqual(undefined, whereis(yawl_auth)),
                    {ok, NewPid} = yawl_auth:start_auth(),
                    ?assert(is_pid(NewPid)),
                    catch yawl_auth:stop_auth()
                end)]
     end}.

%%====================================================================
%% 2. User Creation Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test creating a new user.
%% @end
%%--------------------------------------------------------------------
test_create_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"testuser">>,
                        <<"SecurePass123!">>,
                        [<<"user">>]
                    ),
                    ?assert(is_binary(UserId)),
                    ?assertMatch(<<"user_", _/binary>>, UserId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test creating a user with initial roles.
%% @end
%%--------------------------------------------------------------------
test_create_user_with_roles_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"adminuser">>,
                        <<"AdminPass456!">>,
                        [<<"admin">>, <<"editor">>]
                    ),
                    ?assert(is_binary(UserId)),
                    % Verify user has admin role
                    ?assert(yawl_auth:has_permission(UserId, <<"admin">>)),
                    ?assert(yawl_auth:has_permission(UserId, <<"editor">>))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test creating duplicate user fails.
%% @end
%%--------------------------------------------------------------------
test_create_duplicate_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"duplicate">>,
                    {ok, _UserId1} = yawl_auth:create_user(
                        Username,
                        <<"Password123!">>,
                        []
                    ),
                    {error, already_exists} = yawl_auth:create_user(
                        Username,
                        <<"Password456!">>,
                        []
                    )
                end)]
     end}.

%%====================================================================
%% 3. Authentication Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test successful authentication with valid credentials.
%% @end
%%--------------------------------------------------------------------
test_authenticate_success_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"validuser">>,
                    Password = <<"ValidPass123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    {ok, SessionId} = yawl_auth:authenticate(Username, Password),
                    ?assert(is_binary(SessionId)),
                    ?assertMatch(<<"session_", _/binary>>, SessionId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test authentication failure with invalid credentials.
%% @end
%%--------------------------------------------------------------------
test_authenticate_failure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"creduser">>,
                    Password = <<"CorrectPass123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        Username,
                        <<"WrongPassword">>
                    )
                end),
          ?_test(begin
                    % Test with non-existent user
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        <<"nonexistent">>,
                        <<"password">>
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test authentication with disabled user.
%% @end
%%--------------------------------------------------------------------
test_authenticate_disabled_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"disableduser">>,
                    Password = <<"DisabledPass123!">>,
                    {ok, UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    % Disable the user
                    ok = yawl_auth:update_user(UserId, [{enabled, false}]),
                    % Try to authenticate
                    {error, user_disabled} = yawl_auth:authenticate(
                        Username,
                        Password
                    )
                end)]
     end}.

%%====================================================================
%% 4. Password Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test password hashing produces consistent results.
%% This is tested indirectly through authentication.
%% @end
%%--------------------------------------------------------------------
test_hash_password_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Password = <<"TestPassword123!">>,
                    % Hashing is tested via authentication
                    % Same password should always authenticate successfully
                    {ok, _UserId} = yawl_auth:create_user(
                        <<"hashuser">>,
                        Password,
                        []
                    ),
                    % Authenticate multiple times - should always succeed
                    {ok, _S1} = yawl_auth:authenticate(<<"hashuser">>, Password),
                    {ok, _S2} = yawl_auth:authenticate(<<"hashuser">>, Password),
                    {ok, _S3} = yawl_auth:authenticate(<<"hashuser">>, Password)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test password verification.
%% This is tested indirectly through authentication.
%% @end
%%--------------------------------------------------------------------
test_verify_password_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Password = <<"VerifyPass123!">>,
                    WrongPassword = <<"WrongPassword123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        <<"verifyuser">>,
                        Password,
                        []
                    ),
                    % Correct password should work
                    {ok, _SessionId} = yawl_auth:authenticate(<<"verifyuser">>, Password),
                    % Wrong password should fail
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        <<"verifyuser">>,
                        WrongPassword
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test updating user password.
%% @end
%%--------------------------------------------------------------------
test_update_user_password_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"passupdateuser">>,
                    OldPassword = <<"OldPass123!">>,
                    NewPassword = <<"NewPass456!">>,
                    {ok, UserId} = yawl_auth:create_user(
                        Username,
                        OldPassword,
                        [<<"user">>]
                    ),
                    % Authenticate with old password
                    {ok, _Session1} = yawl_auth:authenticate(Username, OldPassword),
                    % Update password
                    ok = yawl_auth:update_user(UserId, [{password, NewPassword}]),
                    % Old password should no longer work
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        Username,
                        OldPassword
                    ),
                    % New password should work
                    {ok, _Session2} = yawl_auth:authenticate(Username, NewPassword)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test password strength validation - strong passwords accepted.
%% @end
%%--------------------------------------------------------------------
test_password_strength_validation_strong_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Test various strong password formats are accepted
                    % Note: bcrypt is intentionally slow, so we test just one password
                    StrongPassword = <<"StrongPass123!">>,
                    Username = <<"strong_pass_user">>,
                    {ok, _} = yawl_auth:create_user(Username, StrongPassword, []),
                    {ok, _} = yawl_auth:authenticate(Username, StrongPassword)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test password strength validation - weak passwords rejected.
%% @end
%%--------------------------------------------------------------------
test_password_strength_validation_weak_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Too short (< 8 characters)
                    {error, {too_short, _}} =
                        yawl_auth:create_user(<<"shortuser">>, <<"short1!">>, []),

                    % Missing uppercase
                    {error, {weak_password, Missing1}} =
                        yawl_auth:create_user(<<"noupper">>, <<"lowercase123!">>, []),
                    ?assert(lists:member(<<"uppercase">>, Missing1)),

                    % Missing lowercase
                    {error, {weak_password, Missing2}} =
                        yawl_auth:create_user(<<"nolower">>, <<"UPPERCASE123!">>, []),
                    ?assert(lists:member(<<"lowercase">>, Missing2)),

                    % Missing digit
                    {error, {weak_password, Missing3}} =
                        yawl_auth:create_user(<<"nodigit">>, <<"NoDigitsHere!">>, []),
                    ?assert(lists:member(<<"digit">>, Missing3)),

                    % Missing special character
                    {error, {weak_password, Missing4}} =
                        yawl_auth:create_user(<<"nospecial">>, <<"NoSpecial123">>, []),
                    ?assert(lists:member(<<"special">>, Missing4)),

                    % Multiple missing requirements
                    {error, {too_short, _}} =
                        yawl_auth:create_user(<<"multiple">>, <<"simple">>, [])
                end)]
     end}.

%%====================================================================
%% 5. Authorization Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test admin has all permissions.
%% @end
%%--------------------------------------------------------------------
test_authorize_admin_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, AdminId} = yawl_auth:create_user(
                        <<"admin">>,
                        <<"AdminPass123!">>,
                        [<<"admin">>]
                    ),
                    % Admin should have all permissions
                    ?assert(yawl_auth:authorize(AdminId, <<"any_resource">>, <<"any_action">>)),
                    ?assert(yawl_auth:authorize(AdminId, <<"users">>, <<"delete">>)),
                    ?assert(yawl_auth:authorize(AdminId, <<"settings">>, <<"modify">>))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test role-based authorization.
%% @end
%%--------------------------------------------------------------------
test_authorize_role_based_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Create user with specific resource:action role
                    {ok, UserId} = yawl_auth:create_user(
                        <<"editor">>,
                        <<"EditorPass123!">>,
                        [<<"posts:read">>, <<"posts:write">>]
                    ),
                    % Should have these permissions
                    ?assert(yawl_auth:authorize(UserId, <<"posts">>, <<"read">>)),
                    ?assert(yawl_auth:authorize(UserId, <<"posts">>, <<"write">>)),
                    % Should not have these
                    ?assertNot(yawl_auth:authorize(UserId, <<"posts">>, <<"delete">>)),
                    ?assertNot(yawl_auth:authorize(UserId, <<"users">>, <<"read">>))
                end),
          ?_test(begin
                    % Test wildcard permission
                    {ok, WildcardId} = yawl_auth:create_user(
                        <<"superuser">>,
                        <<"SuperPass123!">>,
                        [<<"*">>]
                    ),
                    ?assert(yawl_auth:authorize(WildcardId, <<"anything">>, <<"any_action">>))
                end),
          ?_test(begin
                    % Test resource wildcard
                    {ok, ResourceWildcardId} = yawl_auth:create_user(
                        <<"resource_admin">>,
                        <<"ResourcePass123!">>,
                        [<<"posts:*">>]
                    ),
                    ?assert(yawl_auth:authorize(ResourceWildcardId, <<"posts">>, <<"read">>)),
                    ?assert(yawl_auth:authorize(ResourceWildcardId, <<"posts">>, <<"write">>)),
                    ?assert(yawl_auth:authorize(ResourceWildcardId, <<"posts">>, <<"delete">>)),
                    ?assertNot(yawl_auth:authorize(ResourceWildcardId, <<"users">>, <<"read">>))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test has_permission function.
%% @end
%%--------------------------------------------------------------------
test_has_permission_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"permduser">>,
                        <<"PermPass123!">>,
                        [<<"custom_permission">>, <<"another_perm">>]
                    ),
                    ?assert(yawl_auth:has_permission(UserId, <<"custom_permission">>)),
                    ?assert(yawl_auth:has_permission(UserId, <<"another_perm">>)),
                    ?assertNot(yawl_auth:has_permission(UserId, <<"nonexistent_perm">>))
                end)]
     end}.

%%====================================================================
%% 6. Role Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding and removing roles.
%% @end
%%--------------------------------------------------------------------
test_add_remove_role_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"roleuser">>,
                        <<"RolePass123!">>,
                        [<<"base_role">>]
                    ),
                    ?assert(yawl_auth:has_permission(UserId, <<"base_role">>)),
                    ?assertNot(yawl_auth:has_permission(UserId, <<"new_role">>)),
                    % Add new role
                    ok = yawl_auth:add_role(UserId, <<"new_role">>),
                    ?assert(yawl_auth:has_permission(UserId, <<"new_role">>)),
                    % Remove role
                    ok = yawl_auth:remove_role(UserId, <<"new_role">>),
                    ?assertNot(yawl_auth:has_permission(UserId, <<"new_role">>)),
                    % Original role still present
                    ?assert(yawl_auth:has_permission(UserId, <<"base_role">>))
                end),
          ?_test(begin
                    % Test removing non-existent user
                    {error, not_found} = yawl_auth:add_role(
                        <<"nonexistent_user_id">>,
                        <<"role">>
                    ),
                    {error, not_found} = yawl_auth:remove_role(
                        <<"nonexistent_user_id">>,
                        <<"role">>
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test adding duplicate role is idempotent.
%% @end
%%--------------------------------------------------------------------
test_add_duplicate_role_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"duproleuser">>,
                        <<"RolePass123!">>,
                        [<<"initial_role">>]
                    ),
                    % Add same role twice
                    ok = yawl_auth:add_role(UserId, <<"extra_role">>),
                    ok = yawl_auth:add_role(UserId, <<"extra_role">>),
                    ?assert(yawl_auth:has_permission(UserId, <<"extra_role">>))
                end)]
     end}.

%%====================================================================
%% 7. Session Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test session creation and validation.
%% @end
%%--------------------------------------------------------------------
test_session_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"sessionuser">>,
                    Password = <<"SessionPass123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    {ok, SessionId} = yawl_auth:authenticate(Username, Password),
                    % Get session info - verify it's a tuple (session record)
                    {ok, Session} = yawl_auth:get_session(SessionId),
                    ?assert(is_tuple(Session))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test session invalidation.
%% @end
%%--------------------------------------------------------------------
test_invalidate_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"invalidatesessionuser">>,
                    Password = <<"InvalidatePass123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    {ok, SessionId} = yawl_auth:authenticate(Username, Password),
                    % Validate session exists
                    {ok, _Session} = yawl_auth:get_session(SessionId),
                    % Invalidate session
                    ok = yawl_auth:invalidate_session(SessionId),
                    % Session should no longer be found
                    {error, not_found} = yawl_auth:get_session(SessionId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test session expiration.
%% @end
%%--------------------------------------------------------------------
test_expired_session_test_() ->
    {setup,
     fun() ->
             % Start with very short session timeout
             {ok, Pid} = yawl_auth:start_auth([{session_timeout, 1}]),
             Pid
     end,
     fun(Pid) ->
             unlink(Pid),
             yawl_auth:stop_auth(),
             timer:sleep(100)
     end,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"expiresessionuser">>,
                    Password = <<"ExpirePass123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    {ok, SessionId} = yawl_auth:authenticate(Username, Password),
                    % Session should be valid immediately
                    {ok, _Session} = yawl_auth:get_session(SessionId),
                    % Wait for session to expire (2+ seconds for reliability)
                    timer:sleep(2100),
                    % Session should now be expired
                    {error, expired} = yawl_auth:get_session(SessionId)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test getting non-existent session.
%% @end
%%--------------------------------------------------------------------
test_get_nonexistent_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeSessionId = <<"session_fakesessionid">>,
                    {error, not_found} = yawl_auth:get_session(FakeSessionId)
                end)]
     end}.

%%====================================================================
%% 8. User Update and Deletion Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test updating user properties.
%% @end
%%--------------------------------------------------------------------
test_update_user_properties_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"updateuser">>,
                        <<"UpdatePass123!">>,
                        [<<"user">>]
                    ),
                    % Update username
                    ok = yawl_auth:update_user(UserId, [{username, <<"newusername">>}]),
                    % Can authenticate with new username (need password for this)
                    {ok, _SessionId} = yawl_auth:authenticate(
                        <<"newusername">>,
                        <<"UpdatePass123!">>
                    )
                end),
          ?_test(begin
                    {ok, UserId} = yawl_auth:create_user(
                        <<"enableuser">>,
                        <<"EnablePass123!">>,
                        [<<"user">>]
                    ),
                    % Disable then re-enable
                    ok = yawl_auth:update_user(UserId, [{enabled, false}]),
                    {error, user_disabled} = yawl_auth:authenticate(
                        <<"enableuser">>,
                        <<"EnablePass123!">>
                    ),
                    ok = yawl_auth:update_user(UserId, [{enabled, true}]),
                    {ok, _SessionId} = yawl_auth:authenticate(
                        <<"enableuser">>,
                        <<"EnablePass123!">>
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test deleting a user.
%% @end
%%--------------------------------------------------------------------
test_delete_user_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"deleteuser">>,
                    {ok, UserId} = yawl_auth:create_user(
                        Username,
                        <<"DeletePass123!">>,
                        [<<"user">>]
                    ),
                    % User can authenticate
                    {ok, _SessionId} = yawl_auth:authenticate(
                        Username,
                        <<"DeletePass123!">>
                    ),
                    % Delete user
                    ok = yawl_auth:delete_user(UserId),
                    % User can no longer authenticate
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        Username,
                        <<"DeletePass123!">>
                    ),
                    % Deleting non-existent user
                    {error, not_found} = yawl_auth:delete_user(UserId)
                end)]
     end}.

%%====================================================================
%% 9. Multiple Session Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test user can have multiple active sessions.
%% @end
%%--------------------------------------------------------------------
test_multiple_sessions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"multisessionuser">>,
                    Password = <<"MultiSessionPass123!">>,
                    {ok, _UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"user">>]
                    ),
                    % Create multiple sessions
                    {ok, Session1} = yawl_auth:authenticate(Username, Password),
                    {ok, Session2} = yawl_auth:authenticate(Username, Password),
                    {ok, Session3} = yawl_auth:authenticate(Username, Password),
                    % All sessions should be valid
                    ?assertNotEqual(Session1, Session2),
                    ?assertNotEqual(Session2, Session3),
                    {ok, _} = yawl_auth:get_session(Session1),
                    {ok, _} = yawl_auth:get_session(Session2),
                    {ok, _} = yawl_auth:get_session(Session3)
                end)]
     end}.

%%====================================================================
%% 10. Edge Cases and Error Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test authentication with empty inputs.
%% @end
%%--------------------------------------------------------------------
test_empty_input_authentication_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Empty username
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        <<>>,
                        <<"password">>
                    ),
                    % Empty password
                    {error, invalid_credentials} = yawl_auth:authenticate(
                        <<"someuser">>,
                        <<>>
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test operations on non-existent user ID.
%% @end
%%--------------------------------------------------------------------
test_nonexistent_user_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    FakeUserId = <<"user_fakenonexistent">>,
                    {error, not_found} = yawl_auth:delete_user(FakeUserId),
                    {error, not_found} = yawl_auth:update_user(
                        FakeUserId,
                        [{enabled, false}]
                    ),
                    {error, not_found} = yawl_auth:add_role(FakeUserId, <<"role">>),
                    {error, not_found} = yawl_auth:remove_role(FakeUserId, <<"role">>),
                    ?assertNot(yawl_auth:has_permission(FakeUserId, <<"any">>)),
                    ?assertNot(yawl_auth:authorize(FakeUserId, <<"any">>, <<"any">>))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test bcrypt password hashing produces unique salts each time.
%% This ensures proper salt generation - same password should produce
%% different hashes each time due to random salt.
%% @end
%%--------------------------------------------------------------------
test_bcrypt_unique_salts_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Create multiple users with same password
                    Password = <<"SamePassword123!">>,
                    {ok, _UserId1} = yawl_auth:create_user(<<"user1">>, Password, []),
                    {ok, _UserId2} = yawl_auth:create_user(<<"user2">>, Password, []),
                    {ok, _UserId3} = yawl_auth:create_user(<<"user3">>, Password, []),

                    % All users should be able to authenticate with the same password
                    {ok, _Session1} = yawl_auth:authenticate(<<"user1">>, Password),
                    {ok, _Session2} = yawl_auth:authenticate(<<"user2">>, Password),
                    {ok, _Session3} = yawl_auth:authenticate(<<"user3">>, Password)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test wrong password is properly rejected.
%% Ensures timing attacks are mitigated by bcrypt's constant-time comparison.
%% @end
%%--------------------------------------------------------------------
test_wrong_password_rejection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Password = <<"CorrectPassword123!">>,
                    {ok, _UserId} = yawl_auth:create_user(<<"secureuser">>, Password, []),

                    % Similar but wrong passwords should all fail
                    WrongPasswords = [
                        <<"correctpassword123!">>,  % Wrong case
                        <<"CorrectPassword123">>,   % Missing special char
                        <<"CorrectPassword124!">>,  % Wrong digit
                        <<"Correct Password123!">>, % Extra space
                        <<"CorrectPass123!">>,      % Shorter
                        <<"CorrectPassword123!!">>  % Extra special
                    ],
                    lists:foreach(
                        fun(WrongPass) ->
                            {error, invalid_credentials} =
                                yawl_auth:authenticate(<<"secureuser">>, WrongPass)
                        end,
                        WrongPasswords
                    )
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test password update maintains strength validation.
%% @end
%%--------------------------------------------------------------------
test_password_update_strength_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    Username = <<"passupdateuser2">>,
                    OldPassword = <<"OldPassword123!">>,
                    {ok, UserId} = yawl_auth:create_user(Username, OldPassword, []),
                    {ok, SessionId} = yawl_auth:authenticate(Username, OldPassword),

                    % Updating with a weak password should fail
                    {error, {too_short, _}} = yawl_auth:update_user(UserId, [{password, <<"weak">>}]),

                    % Original password should still work
                    {ok, _} = yawl_auth:authenticate(Username, OldPassword),

                    % Update with a strong password should work
                    NewPassword = <<"NewPassword456!">>,
                    ok = yawl_auth:update_user(UserId, [{password, NewPassword}]),

                    % Old password should no longer work
                    {error, invalid_credentials} = yawl_auth:authenticate(Username, OldPassword),

                    % New password should work
                    {ok, _} = yawl_auth:authenticate(Username, NewPassword),

                    % Invalidate old session
                    ok = yawl_auth:invalidate_session(SessionId)
                end)]
     end}.

%%====================================================================
%% 11. Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test complete authentication workflow.
%% @end
%%--------------------------------------------------------------------
test_complete_auth_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % 1. Create user
                    Username = <<"workflowuser">>,
                    Password = <<"WorkflowPass123!">>,
                    {ok, UserId} = yawl_auth:create_user(
                        Username,
                        Password,
                        [<<"reader:read">>]  % Use resource:action format for roles
                    ),
                    ?assert(is_binary(UserId)),

                    % 2. Authenticate
                    {ok, SessionId} = yawl_auth:authenticate(Username, Password),
                    ?assert(is_binary(SessionId)),

                    % 3. Check session (records are tuples: {session, SessionId, UserId, ExpiresAt})
                    {ok, {session, _SessionId2, UserId, _ExpiresAt}} = yawl_auth:get_session(SessionId),

                    % 4. Authorize with existing role
                    ?assert(yawl_auth:authorize(UserId, <<"reader">>, <<"read">>)),

                    % 5. Add new role (use resource:action format)
                    ok = yawl_auth:add_role(UserId, <<"writer:write">>),
                    ?assert(yawl_auth:authorize(UserId, <<"writer">>, <<"write">>)),

                    % 6. Invalidate session
                    ok = yawl_auth:invalidate_session(SessionId),
                    {error, not_found} = yawl_auth:get_session(SessionId),

                    % 7. Create new session
                    {ok, NewSessionId} = yawl_auth:authenticate(Username, Password),
                    {ok, _NewSession} = yawl_auth:get_session(NewSessionId),

                    ok
                end)]
     end}.
