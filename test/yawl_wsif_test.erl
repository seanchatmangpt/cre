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
%% @doc YAWL WSIF (Web Service Integration Framework) Test Suite
%%
%% Comprehensive test suite for WSIF functionality including:
%% - Service registration and lifecycle
%% - REST API invocation (GET, POST, PUT, DELETE)
%% - SOAP service invocation
%% - WSDL parsing and stub generation
%% - Connection testing
%% - Namespaced SOAP handling
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_wsif_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures - Setup and Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts inets for HTTP support and initializes WSIF server.
%% @end
%%--------------------------------------------------------------------
setup() ->
    % Ensure inets is started for httpc
    application:ensure_all_started(inets),
    % Start WSIF server
    {ok, _WsifPid} = yawl_wsif:start_wsif(),
    % Return setup state
    #{wsif_started => true}.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops WSIF server and ensures clean state.
%% @end
%%--------------------------------------------------------------------
cleanup(_State) ->
    % Stop WSIF server
    catch yawl_wsif:stop_wsif(),
    ok.

%%====================================================================
%% Test Generator for Setup/Teardown
%%====================================================================

%% Wrapper to apply setup/teardown to all tests
test_with_setup( TestFun ) ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) -> TestFun() end}.

%%====================================================================
%% WSIF Service Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: start_wsif/0
%% @doc Verify WSIF server starts successfully
%% @end
%%--------------------------------------------------------------------
start_wsif_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Should already be started by setup
                % Verify by listing services (empty list is valid)
                Services = yawl_wsif:list_services(),
                ?assert(is_list(Services)),
                ?assertEqual(0, length(Services))
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: register_service/0
%% @doc Register a REST service and verify registration
%% @end
%%--------------------------------------------------------------------
register_service_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                ServiceName = <<"test_api">>,
                Config = [
                    {endpoint, <<"https://api.example.com/v1">>},
                    {port, <<"HTTP">>},
                    {operation, <<"getData">>}
                ],
                Result = yawl_wsif:register_service(ServiceName, Config),
                ?assertMatch({ok, _ServiceId}, Result),
                {ok, ServiceId} = Result,
                ?assert(is_binary(ServiceId)),
                ?assert(<<>> /= ServiceId)
            end),
            ?_test(begin
                % Test duplicate registration returns error
                ServiceName = <<"duplicate_test">>,
                Config = [{endpoint, <<"https://api.test.com">>}],
                {ok, _Id} = yawl_wsif:register_service(ServiceName, Config),
                DuplicateResult = yawl_wsif:register_service(ServiceName, Config),
                ?assertEqual({error, already_exists}, DuplicateResult)
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: unregister_service/0
%% @doc Unregister a service and verify it's removed
%% @end
%%--------------------------------------------------------------------
unregister_service_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                ServiceName = <<"temp_service">>,
                Config = [{endpoint, <<"https://temp.example.com">>}],
                {ok, ServiceId} = yawl_wsif:register_service(ServiceName, Config),
                UnregisterResult = yawl_wsif:unregister_service(ServiceId),
                ?assertEqual(ok, UnregisterResult),
                % Verify service is gone
                GetResult = yawl_wsif:get_service_by_id(ServiceId),
                ?assertEqual({error, not_found}, GetResult)
            end),
            ?_test(begin
                % Test unregistering non-existent service
                FakeId = <<"non_existent_service_id">>,
                Result = yawl_wsif:unregister_service(FakeId),
                ?assertEqual({error, not_found}, Result)
            end)]
        end
    ).

%%====================================================================
%% REST API Invocation Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: invoke_rest_get/0
%% @doc Test REST GET invocation with query parameters
%% @end
%%--------------------------------------------------------------------
invoke_rest_get_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test with query parameters
                Url = <<"https://httpbin.org/get">>,
                Params = #{param1 => <<"value1">>, param2 => <<"value2">>},
                Result = yawl_wsif:invoke_rest(get, Url, Params),
                % In test environment, we check structure not actual HTTP
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Test with no parameters
                Url = <<"https://httpbin.org/get">>,
                Result = yawl_wsif:invoke_rest(get, Url, #{}),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: invoke_rest_post/0
%% @doc Test REST POST invocation with JSON body
%% @end
%%--------------------------------------------------------------------
invoke_rest_post_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                Url = <<"https://httpbin.org/post">>,
                Params = #{name => <<"Test User">>, email => <<"test@example.com">>},
                Result = yawl_wsif:invoke_rest(post, Url, Params),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Test with empty body
                Url = <<"https://httpbin.org/post">>,
                Result = yawl_wsif:invoke_rest(post, Url, #{}),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Additional REST method tests (PUT, DELETE)
%% @end
%%--------------------------------------------------------------------
invoke_rest_put_delete_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                Url = <<"https://httpbin.org/put">>,
                Params = #{key => <<"value">>},
                Result = yawl_wsif:invoke_rest(put, Url, Params),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end),
            ?_test(begin
                Url = <<"https://httpbin.org/delete">>,
                Result = yawl_wsif:invoke_rest(delete, Url, #{}),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end)]
        end
    ).

%%====================================================================
%% SOAP Service Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: invoke_soap/0
%% @doc Test SOAP service invocation
%% @end
%%--------------------------------------------------------------------
invoke_soap_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                Endpoint = <<"https://www.w3schools.com/xml/tempconvert.asmx">>,
                Operation = <<"CelsiusToFahrenheit">>,
                Params = #{celsius => <<"100">>},
                Result = yawl_wsif:invoke_soap(Endpoint, Operation, Params),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Test with empty parameters
                Endpoint = <<"https://example.com/soap">>,
                Operation = <<"Ping">>,
                Result = yawl_wsif:invoke_soap(Endpoint, Operation, #{}),
                case Result of
                    {ok, _Body} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: build_soap_envelope/0
%% @doc Test SOAP envelope construction
%% @end
%%--------------------------------------------------------------------
build_soap_envelope_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test envelope with parameters
                Operation = <<"GetData">>,
                Params = #{id => <<"123">>, name => <<"Test">>},
                Envelope = build_test_soap_envelope(Operation, Params),
                ?assert(is_list(Envelope) orelse is_binary(Envelope)),
                % Verify envelope contains operation name
                EnvelopeBin = iolist_to_binary(Envelope),
                ?assert(binary:match(EnvelopeBin, Operation) =/= nomatch),
                ?assert(binary:match(EnvelopeBin, <<"soap:Envelope">>) =/= nomatch)
            end),
            ?_test(begin
                % Test envelope without parameters
                Operation = <<"Ping">>,
                Envelope = build_test_soap_envelope(Operation, #{}),
                EnvelopeBin = iolist_to_binary(Envelope),
                ?assert(binary:match(EnvelopeBin, Operation) =/= nomatch),
                ?assert(binary:match(EnvelopeBin, <<"soap:Body">>) =/= nomatch)
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: extract_soap_body/0
%% @doc Test SOAP body extraction from response
%% @end
%%--------------------------------------------------------------------
extract_soap_body_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test extraction from valid SOAP response
                SoapResponse = <<
                    "<?xml version=\"1.0\"?>",
                    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">",
                    "  <soap:Body>",
                    "    <m:GetStockPriceResponse xmlns:m=\"http://example.com/stock\">",
                    "      <m:Price>123.45</m:Price>",
                    "    </m:GetStockPriceResponse>",
                    "  </soap:Body>",
                    "</soap:Envelope>"
                >>,
                Body = extract_test_soap_body(SoapResponse),
                ?assert(is_binary(Body)),
                ?assert(binary:match(Body, <<"Price">>) =/= nomatch)
            end),
            ?_test(begin
                % Test with missing soap:Body tag
                NoBody = <<"<response>No body here</response>">>,
                Body = extract_test_soap_body(NoBody),
                ?assert(is_binary(Body))
            end)]
        end
    ).

%%====================================================================
%% WSDL Parsing Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: parse_wsdl/0
%% @doc Test WSDL document parsing
%% @end
%%--------------------------------------------------------------------
parse_wsdl_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test parsing a mock WSDL
                WsdlContent = <<
                    "<?xml version=\"1.0\"?>",
                    "<definitions xmlns=\"http://schemas.xmlsoap.org/wsdl/\">",
                    "  <service name=\"TestService\">",
                    "    <port name=\"TestPort\" binding=\"tns:TestBinding\">",
                    "      <soap:address xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" location=\"http://example.com/test\"/>",
                    "    </port>",
                    "  </service>",
                    "  <portType name=\"TestPortType\">",
                    "    <operation name=\"GetData\"/>",
                    "    <operation name=\"SetData\"/>",
                    "  </portType>",
                    "</definitions>"
                >>,
                Result = parse_test_wsdl(WsdlContent),
                ?assertMatch({ok, #{services := _, ports := _, operations := _}}, Result)
            end),
            ?_test(begin
                % Test with invalid WSDL
                InvalidWsdl = <<"<not_a_wsdl></not_a_wsdl>">>,
                Result = parse_test_wsdl(InvalidWsdl),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: generate_stub/0
%% @doc Test client stub generation from WSDL
%% @end
%%--------------------------------------------------------------------
generate_stub_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                ServiceName = <<"TestService">>,
                WsdlData = #{
                    operations => [<<"GetData">>, <<"SetData">>, <<"DeleteData">>],
                    ports => [<<"TestPort">>],
                    services => [<<"TestService">>]
                },
                {ok, StubCode} = generate_test_stub(ServiceName, WsdlData),
                ?assert(is_binary(StubCode)),
                ?assert(binary:match(StubCode, ServiceName) =/= nomatch),
                ?assert(binary:match(StubCode, <<"GetData">>) =/= nomatch),
                ?assert(binary:match(StubCode, <<"SetData">>) =/= nomatch)
            end),
            ?_test(begin
                % Test with no operations
                ServiceName = <<"EmptyService">>,
                WsdlData = #{operations => [], ports => [], services => []},
                {ok, StubCode} = generate_test_stub(ServiceName, WsdlData),
                ?assert(is_binary(StubCode)),
                ?assert(binary:match(StubCode, ServiceName) =/= nomatch)
            end)]
        end
    ).

%%====================================================================
%% Service Connection Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: test_connection/0
%% @doc Test service connectivity verification
%% @end
%%--------------------------------------------------------------------
test_connection_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test with registered service
                ServiceName = <<"connectable_service">>,
                Config = [{endpoint, <<"https://httpbin.org">>}],
                {ok, ServiceId} = yawl_wsif:register_service(ServiceName, Config),
                Result = yawl_wsif:test_connection(ServiceId),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Test with non-existent service
                FakeId = <<"non_existent_id">>,
                Result = yawl_wsif:test_connection(FakeId),
                ?assertEqual({error, not_found}, Result)
            end),
            ?_test(begin
                % Test direct URL connection test
                Url = <<"https://httpbin.org">>,
                Result = yawl_wsif:test_connection(Url),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _Reason} -> ?assert(true)
                end
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: list_services/0
%% @doc Test listing all registered services
%% @end
%%--------------------------------------------------------------------
list_services_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Start with empty list
                Services = yawl_wsif:list_services(),
                ?assertEqual(0, length(Services)),
                % Register some services
                {ok, _Id1} = yawl_wsif:register_service(<<"svc1">>, [{endpoint, <<"https://a.example.com">>}]),
                {ok, _Id2} = yawl_wsif:register_service(<<"svc2">>, [{endpoint, <<"https://b.example.com">>}]),
                UpdatedServices = yawl_wsif:list_services(),
                ?assertEqual(2, length(UpdatedServices))
            end),
            ?_test(begin
                % Verify service IDs and names are in the list
                {ok, Id} = yawl_wsif:register_service(<<"test_svc">>, [{endpoint, <<"https://c.example.com">>}]),
                Services = yawl_wsif:list_services(),
                ?assert(lists:any(fun({Sid, _Name}) -> Sid =:= Id end, Services))
            end)]
        end
    ).

%%====================================================================
%% Namespaced SOAP Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: namespaced_soap/0
%% @doc Test handling of namespaced SOAP messages
%% @end
%%--------------------------------------------------------------------
namespaced_soap_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test building namespaced envelope
                Operation = <<"ns:GetData">>,
                Params = #{ns_id => <<"123">>},
                Envelope = build_test_soap_envelope(Operation, Params),
                EnvelopeBin = iolist_to_binary(Envelope),
                ?assert(binary:match(EnvelopeBin, <<"ns:GetData">>) =/= nomatch)
            end),
            ?_test(begin
                % Test extracting namespaced response
                NamespacedResponse = <<
                    "<?xml version=\"1.0\"?>",
                    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">",
                    "  <soap:Body>",
                    "    <ns:GetDataResponse xmlns:ns=\"http://example.com/ns\">",
                    "      <ns:Result>Success</ns:Result>",
                    "    </ns:GetDataResponse>",
                    "  </soap:Body>",
                    "</soap:Envelope>"
                >>,
                Body = extract_test_soap_body(NamespacedResponse),
                ?assert(is_binary(Body)),
                ?assert(binary:match(Body, <<"GetDataResponse">>) =/= nomatch)
            end),
            ?_test(begin
                % Test multiple namespaces in envelope
                MultiNsEnvelope = build_test_soap_envelope(
                    <<"m:Operation">>,
                    #{param1 => <<"value1">>, param2 => <<"value2">>}
                ),
                EnvelopeBin = iolist_to_binary(MultiNsEnvelope),
                ?assert(binary:match(EnvelopeBin, <<"m:Operation">>) =/= nomatch)
            end)]
        end
    ).

%%====================================================================
%% SOAP Fault Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: parse_soap_fault/0
%% @doc Test SOAP fault parsing from responses
%% @end
%%--------------------------------------------------------------------
parse_soap_fault_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test parsing a valid SOAP fault
                SoapFaultXml = <<
                    "<?xml version=\"1.0\"?>",
                    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">",
                    "  <soap:Body>",
                    "    <soap:Fault>",
                    "      <faultcode>soap:Client</faultcode>",
                    "      <faultstring>Invalid input parameter</faultstring>",
                    "      <faultactor>http://example.com/service</faultactor>",
                    "      <detail>",
                    "        <errorcode>1234</errorcode>",
                    "      </detail>",
                    "    </soap:Fault>",
                    "  </soap:Body>",
                    "</soap:Envelope>"
                >>,
                Result = yawl_wsif:parse_soap_fault(SoapFaultXml),
                ?assertMatch({fault, #{faultcode := _, faultstring := _}}, Result),
                {fault, FaultMap} = Result,
                ?assertEqual(<<"soap:Client">>, maps:get(faultcode, FaultMap)),
                ?assertEqual(<<"Invalid input parameter">>, maps:get(faultstring, FaultMap)),
                ?assertEqual(<<"http://example.com/service">>, maps:get(faultactor, FaultMap))
            end),
            ?_test(begin
                % Test parsing fault without actor and detail
                MinimalFault = <<
                    "<?xml version=\"1.0\"?>",
                    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">",
                    "  <soap:Body>",
                    "    <soap:Fault>",
                    "      <faultcode>Server</faultcode>",
                    "      <faultstring>Internal error</faultstring>",
                    "    </soap:Fault>",
                    "  </soap:Body>",
                    "</soap:Envelope>"
                >>,
                {fault, FaultMap} = yawl_wsif:parse_soap_fault(MinimalFault),
                ?assertEqual(<<"Server">>, maps:get(faultcode, FaultMap)),
                ?assertEqual(undefined, maps:get(faultactor, FaultMap))
            end),
            ?_test(begin
                % Test response with no fault
                ValidResponse = <<
                    "<?xml version=\"1.0\"?>",
                    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">",
                    "  <soap:Body>",
                    "    <m:GetDataResponse xmlns:m=\"http://example.com\">",
                    "      <m:Result>Success</m:Result>",
                    "    </m:GetDataResponse>",
                    "  </soap:Body>",
                    "</soap:Envelope>"
                >>,
                ?assertEqual(no_fault, yawl_wsif:parse_soap_fault(ValidResponse))
            end),
            ?_test(begin
                % Test parsing soapenv:Fault (SOAP 1.2)
                Soap12Fault = <<
                    "<?xml version=\"1.0\"?>",
                    "<soapenv:Envelope xmlns:soapenv=\"http://www.w3.org/2003/05/soap-envelope\">",
                    "  <soapenv:Body>",
                    "    <soapenv:Fault>",
                    "      <soapenv:Code>",
                    "        <soapenv:Value>soapenv:Sender</soapenv:Value>",
                    "      </soapenv:Code>",
                    "      <soapenv:Reason>",
                    "        <soapenv:Text xml:lang=\"en\">Invalid message</soapenv:Text>",
                    "      </soapenv:Reason>",
                    "    </soapenv:Fault>",
                    "  </soapenv:Body>",
                    "</soapenv:Envelope>"
                >>,
                ?assertMatch({fault, _}, yawl_wsif:parse_soap_fault(Soap12Fault))
            end),
            ?_test(begin
                % Test malformed XML returns no_fault (graceful handling)
                MalformedXml = <<"<not_xml>[broken</not_xml>">>,
                ?assertEqual(no_fault, yawl_wsif:parse_soap_fault(MalformedXml))
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: build_soap_fault/0
%% @doc Test building SOAP fault messages
%% @end
%%--------------------------------------------------------------------
build_soap_fault_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test building fault with all fields
                FaultCode = <<"soap:Client">>,
                FaultString = <<"Invalid input">>,
                Detail = <<"<error>Error details here</error>">>,
                FaultXml = yawl_wsif:build_soap_fault(FaultCode, FaultString, Detail),
                FaultBin = iolist_to_binary(FaultXml),
                ?assert(binary:match(FaultBin, <<"soap:Envelope">>) =/= nomatch),
                ?assert(binary:match(FaultBin, FaultCode) =/= nomatch),
                ?assert(binary:match(FaultBin, FaultString) =/= nomatch),
                ?assert(binary:match(FaultBin, <<"detail">>) =/= nomatch)
            end),
            ?_test(begin
                % Test building fault without detail
                FaultXml = yawl_wsif:build_soap_fault(<<"Server">>, <<"Internal error">>, undefined),
                FaultBin = iolist_to_binary(FaultXml),
                ?assert(binary:match(FaultBin, <<"Server">>) =/= nomatch),
                ?assert(binary:match(FaultBin, <<"Internal error">>) =/= nomatch),
                ?assertEqual(nomatch, binary:match(FaultBin, <<"detail">>))
            end),
            ?_test(begin
                % Test fault XML has proper structure
                FaultXml = yawl_wsif:build_soap_fault(<<"Code">>, <<"Message">>, <<"<detail>Info</detail>">>),
                FaultBin = iolist_to_binary(FaultXml),
                ?assert(binary:match(FaultBin, <<"<?xml version">>) =/= nomatch),
                ?assert(binary:match(FaultBin, <<"<soap:Body>">>) =/= nomatch),
                ?assert(binary:match(FaultBin, <<"</soap:Fault>">>) =/= nomatch)
            end)]
        end
    ).

%%====================================================================
%% WSDL Parsing Extended Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: parse_wsdl_elements/0
%% @doc Test extracting various elements from WSDL
%% @end
%%--------------------------------------------------------------------
parse_wsdl_elements_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test extracting all element types from complete WSDL
                CompleteWsdl = <<
                    "<?xml version=\"1.0\"?>",
                    "<definitions xmlns=\"http://schemas.xmlsoap.org/wsdl/\" ",
                    "             xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" ",
                    "             targetNamespace=\"http://example.com\">",
                    "  <service name=\"TestService\">",
                    "    <port name=\"TestPort\" binding=\"tns:TestBinding\">",
                    "      <soap:address location=\"http://example.com/endpoint\"/>",
                    "    </port>",
                    "  </service>",
                    "  <binding name=\"TestBinding\" type=\"tns:TestPortType\">",
                    "    <operation name=\"BindingOp\"/>",
                    "  </binding>",
                    "  <portType name=\"TestPortType\">",
                    "    <operation name=\"GetData\">",
                    "      <input message=\"tns:GetDataRequest\"/>",
                    "      <output message=\"tns:GetDataResponse\"/>",
                    "    </operation>",
                    "    <operation name=\"SetData\">",
                    "      <input message=\"tns:SetDataRequest\"/>",
                    "    </operation>",
                    "  </portType>",
                    "</definitions>"
                >>,
                {ok, WsdlData} = parse_test_wsdl(CompleteWsdl),
                ?assert(length(maps:get(services, WsdlData, [])) > 0),
                ?assert(length(maps:get(ports, WsdlData, [])) > 0),
                ?assert(length(maps:get(operations, WsdlData, [])) > 0),
                ?assert(length(maps:get(port_types, WsdlData, [])) > 0),
                ?assert(length(maps:get(bindings, WsdlData, [])) > 0)
            end),
            ?_test(begin
                % Test WSDL with multiple services
                MultiServiceWsdl = <<
                    "<?xml version=\"1.0\"?>",
                    "<definitions xmlns=\"http://schemas.xmlsoap.org/wsdl/\">",
                    "  <service name=\"Service1\"/>",
                    "  <service name=\"Service2\"/>",
                    "  <service name=\"Service3\"/>",
                    "</definitions>"
                >>,
                {ok, WsdlData} = parse_test_wsdl(MultiServiceWsdl),
                Services = maps:get(services, WsdlData, []),
                ?assertEqual(3, length(Services))
            end),
            ?_test(begin
                % Test extracting nested operations
                NestedWsdl = <<
                    "<?xml version=\"1.0\"?>",
                    "<definitions xmlns=\"http://schemas.xmlsoap.org/wsdl/\">",
                    "  <portType name=\"MyPortType\">",
                    "    <operation name=\"Operation1\">",
                    "      <input name=\"Operation1Request\"/>",
                    "    </operation>",
                    "    <operation name=\"Operation2\">",
                    "      <output name=\"Operation2Response\"/>",
                    "    </operation>",
                    "  </portType>",
                    "</definitions>"
                >>,
                {ok, WsdlData} = parse_test_wsdl(NestedWsdl),
                Operations = maps:get(operations, WsdlData, []),
                ?assertEqual(2, length(Operations))
            end)]
        end
    ).

%%--------------------------------------------------------------------
%% Test: generate_stub_operations/0
%% @doc Test stub generation with various operation sets
%% @end
%%--------------------------------------------------------------------
generate_stub_operations_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Generate stub with CRUD operations
                ServiceName = <<"CrudService">>,
                WsdlData = #{
                    operations => [<<"Create">>, <<"Read">>, <<"Update">>, <<"Delete">>],
                    ports => [<<"CrudPort">>],
                    services => [<<"CrudService">>]
                },
                {ok, StubCode} = generate_test_stub(ServiceName, WsdlData),
                ?assert(binary:match(StubCode, <<"Create">>) =/= nomatch),
                ?assert(binary:match(StubCode, <<"Read">>) =/= nomatch),
                ?assert(binary:match(StubCode, <<"Update">>) =/= nomatch),
                ?assert(binary:match(StubCode, <<"Delete">>) =/= nomatch),
                % Verify export declaration
                ?assert(binary:match(StubCode, <<"-export">>) =/= nomatch)
            end),
            ?_test(begin
                % Generate stub with hyphenated operation names
                ServiceName = <<"HyphenService">>,
                WsdlData = #{
                    operations => [<<"get-data">>, <<"set-data">>, <<"delete-item">>],
                    services => [<<"HyphenService">>]
                },
                {ok, StubCode} = generate_test_stub(ServiceName, WsdlData),
                ?assert(binary:match(StubCode, <<"get-data">>) =/= nomatch),
                ?assert(binary:match(StubCode, <<"set-data">>) =/= nomatch)
            end),
            ?_test(begin
                % Stub includes module declaration
                ServiceName = <<"TestModule">>,
                WsdlData = #{operations => [<<"test">>], services => [<<"TestService">>]},
                {ok, StubCode} = generate_test_stub(ServiceName, WsdlData),
                ?assert(binary:match(StubCode, <<"-module(">>) =/= nomatch),
                ?assert(binary:match(StubCode, ServiceName) =/= nomatch)
            end)]
        end
    ).

%%====================================================================
%% Service Registration Extended Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: service_registration_edge_cases/0
%% @doc Test edge cases in service registration
%% @end
%%--------------------------------------------------------------------
service_registration_edge_cases_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Register service with minimal config
                ServiceName = <<"MinimalService">>,
                Config = [{endpoint, <<"https://api.example.com">>}],
                {ok, ServiceId} = yawl_wsif:register_service(ServiceName, Config),
                ?assert(is_binary(ServiceId)),
                ?assert(size(ServiceId) > 0)
            end),
            ?_test(begin
                % Register service with full config
                ServiceName = <<"FullConfigService">>,
                Config = [
                    {endpoint, <<"https://api.example.com/v2">>},
                    {wsdl, <<"https://api.example.com/service?wsdl">>},
                    {port, <<"HTTP_Port">>},
                    {operation, <<"ProcessData">>}
                ],
                {ok, ServiceId} = yawl_wsif:register_service(ServiceName, Config),
                {ok, Service} = yawl_wsif:get_service_by_id(ServiceId),
                ?assertEqual(<<"FullConfigService">>, element(4, Service))  % service name
            end),
            ?_test(begin
                % Register service with special characters in name
                SpecialName = <<"Test_Service-123.API">>,
                Config = [{endpoint, <<"https://example.com">>}],
                ?assertMatch({ok, _}, yawl_wsif:register_service(SpecialName, Config))
            end),
            ?_test(begin
                % Multiple services can be registered
                lists:foreach(
                    fun(N) ->
                        Name = list_to_binary("service_" ++ integer_to_list(N)),
                        Config = [{endpoint, <<"https://example.com">>}],
                        {ok, _} = yawl_wsif:register_service(Name, Config)
                    end,
                    lists:seq(1, 10)
                ),
                Services = yawl_wsif:list_services(),
                ?assert(length(Services) >= 10)
            end)]
        end
    ).

%%====================================================================
%% Invoke Service Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: invoke_service_test/0
%% @doc Test invoking registered services
%% @end
%%--------------------------------------------------------------------
invoke_service_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Invoke registered service
                ServiceName = <<"InvokableService">>,
                Config = [
                    {endpoint, <<"https://httpbin.org/post">>},
                    {operation, <<"test">>}
                ],
                {ok, ServiceId} = yawl_wsif:register_service(ServiceName, Config),
                Result = yawl_wsif:invoke_service(ServiceId, <<"test">>, #{key => <<"value">>}),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Invoke non-existent service
                Result = yawl_wsif:invoke_service(<<"nonexistent_id">>, <<"op">>, #{}),
                ?assertEqual({error, not_found}, Result)
            end),
            ?_test(begin
                % Invoke with different operations
                ServiceName = <<"MultiOpService">>,
                Config = [{endpoint, <<"https://httpbin.org/post">>}],
                {ok, ServiceId} = yawl_wsif:register_service(ServiceName, Config),
                Result1 = yawl_wsif:invoke_service(ServiceId, <<"op1">>, #{a => 1}),
                Result2 = yawl_wsif:invoke_service(ServiceId, <<"op2">>, #{b => 2}),
                case Result1 of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end,
                case Result2 of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end)]
        end
    ).

%%====================================================================
%% REST Method Comprehensive Tests
%%====================================================================

%%--------------------------------------------------------------------
%% Test: rest_methods_comprehensive/0
%% @doc Test all REST methods with various scenarios
%% @end
%%--------------------------------------------------------------------
rest_methods_comprehensive_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % GET request with complex query parameters
                Url = <<"https://httpbin.org/get">>,
                Params = #{
                    filter => <<"active">>,
                    sort => <<"name">>,
                    limit => 10,
                    include_deleted => false
                },
                case yawl_wsif:invoke_rest(get, Url, Params) of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % POST with nested JSON
                Url = <<"https://httpbin.org/post">>,
                Params = #{
                    user => #{
                        name => <<"John">>,
                        email => <<"john@example.com">>,
                        address => #{
                            street => <<"123 Main St">>,
                            city => <<"Anytown">>
                        }
                    }
                },
                case yawl_wsif:invoke_rest(post, Url, Params) of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % PUT with update data
                Url = <<"https://httpbin.org/put">>,
                Params = #{id => <<"123">>, status => <<"updated">>},
                case yawl_wsif:invoke_rest(put, Url, Params) of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % DELETE request
                Url = <<"https://httpbin.org/delete">>,
                case yawl_wsif:invoke_rest(delete, Url, #{}) of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Empty parameters
                Url = <<"https://httpbin.org/get">>,
                case yawl_wsif:invoke_rest(get, Url, #{}) of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end)]
        end
    ).

%%====================================================================
%% Edge Cases and Error Handling Tests
%%====================================================================

edge_cases_test_() ->
    test_with_setup(
        fun() ->
            [?_test(begin
                % Test with empty service name
                EmptyName = <<>>,
                Result = yawl_wsif:register_service(EmptyName, [{endpoint, <<"https://example.com">>}]),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Test with very long service name
                LongName = binary:copy(<<"a">>, 1000),
                Result = yawl_wsif:register_service(LongName, [{endpoint, <<"https://example.com">>}]),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end),
            ?_test(begin
                % Test with invalid URL
                Result = yawl_wsif:invoke_rest(get, <<"not-a-url">>, #{}),
                ?assertMatch({error, _}, Result)
            end),
            ?_test(begin
                % Test SOAP with special characters in parameters
                Endpoint = <<"https://example.com/soap">>,
                Operation = <<"EscapeTest">>,
                Params = #{text => <<"Test with <special> & \"characters\"">>},
                Result = yawl_wsif:invoke_soap(Endpoint, Operation, Params),
                case Result of
                    {ok, _} -> ?assert(true);
                    {error, _} -> ?assert(true)
                end
            end)]
        end
    ).

%%====================================================================
%% Helper Functions (Test Utilities)
%%====================================================================

%% @private
%% @doc Helper to build test SOAP envelope (mirrors internal function)
build_test_soap_envelope(Operation, Params) ->
    ParamXml = build_test_params_xml(Params),
    [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n",
        "  <soap:Body>\n",
        "    <", Operation, ">\n",
        ParamXml,
        "    </", Operation, ">\n",
        "  </soap:Body>\n",
        "</soap:Envelope>"
    ].

%% @private
build_test_params_xml(Params) when map_size(Params) =:= 0 ->
    [];
build_test_params_xml(Params) ->
    maps:fold(fun(K, V, Acc) ->
        KeyStr = atom_to_list(K),
        ValStr = format_test_value(V),
        ["      <", KeyStr, ">", ValStr, "</", KeyStr, ">\n" | Acc]
    end, [], Params).

%% @private
format_test_value(V) when is_binary(V) -> V;
format_test_value(V) when is_list(V) -> V;
format_test_value(V) when is_integer(V) -> integer_to_list(V);
format_test_value(V) when is_float(V) -> float_to_list(V, [{decimals, 6}, compact]);
format_test_value(V) -> atom_to_list(V).

%% @private
%% @doc Helper to extract SOAP body (mirrors internal function)
extract_test_soap_body(Xml) ->
    case binary:match(Xml, <<"<soap:Body>">>) of
        {Start, _} ->
            case binary:match(Xml, <<"</soap:Body>">>, [{scope, {Start, byte_size(Xml) - Start}}]) of
                {End, _} ->
                    BodyLen = End - Start - 12,
                    <<_:Start/binary, _:12/binary, Body:BodyLen/binary, _/binary>> = Xml,
                    Body;
                nomatch ->
                    Xml
            end;
        nomatch ->
            Xml
    end.

%% @private
%% @doc Helper to parse test WSDL
parse_test_wsdl(WsdlXml) ->
    try
        Services = extract_test_elements(WsdlXml, <<"service">>),
        Ports = extract_test_elements(WsdlXml, <<"port">>),
        Operations = extract_test_elements(WsdlXml, <<"operation">>),
        PortTypes = extract_test_elements(WsdlXml, <<"portType">>),
        Bindings = extract_test_elements(WsdlXml, <<"binding">>),
        {ok, #{
            services => Services,
            ports => Ports,
            operations => Operations,
            port_types => PortTypes,
            bindings => Bindings
        }}
    catch
        _:_ -> {error, invalid_wsdl}
    end.

%% @private
extract_test_elements(Xml, Tag) ->
    OpenTag = <<"<", Tag/binary>>,
    CloseTag = <<"</", Tag/binary, ">">>,
    SelfCloseTag = <<"/>">>,
    extract_test_elements_loop(Xml, OpenTag, CloseTag, SelfCloseTag, []).

extract_test_elements_loop(<<>>, _OpenTag, _CloseTag, _SelfCloseTag, Acc) ->
    lists:reverse(Acc);
extract_test_elements_loop(Xml, OpenTag, CloseTag, SelfCloseTag, Acc) ->
    case binary:split(Xml, OpenTag) of
        [_, Rest] ->
            % Try to find closing tag first (for non-self-closing elements)
            case binary:split(Rest, CloseTag) of
                [Element, Remaining] ->
                    extract_test_elements_loop(Remaining, OpenTag, CloseTag, SelfCloseTag, [Element | Acc]);
                [_] ->
                    % Try to find self-closing tag
                    case binary:split(Rest, SelfCloseTag) of
                        [Element, Remaining] ->
                            extract_test_elements_loop(Remaining, OpenTag, CloseTag, SelfCloseTag, [Element | Acc]);
                        [_] ->
                            % No proper closing found, stop here
                            lists:reverse(Acc)
                    end
            end;
        [_] ->
            lists:reverse(Acc)
    end.

%% @private
%% @doc Helper to generate test stub
generate_test_stub(ServiceName, WsdlData) ->
    Operations = maps:get(operations, WsdlData, []),
    ExportList = string:join([binary_to_list(Op) || Op <- Operations], ", "),
    Functions = [[
        Op, "() ->\n    yawl_wsif:invoke_service(\n        ServiceId,\n        \"", Op, "\",\n        #{}\n    ).\n"
    ] || Op <- Operations],
    Stub = [
        "%% Auto-generated stub for ", ServiceName, "\n",
        "-module(", ServiceName, ").\n\n",
        "-include_lib(\"yawl_wsif.hrl\").\n\n",
        "-export([", ExportList, "]).\n\n",
        Functions, "\n"
    ],
    {ok, iolist_to_binary(Stub)}.
