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
%% @doc YAWL Web Service Integration Framework (WSIF)
%%
%% This module provides integration with external web services,
%% supporting both REST and SOAP protocols.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>REST API calls via HTTP client (httpc or gun)</li>
%%   <li>SOAP 1.1/1.2 support with xmerl XML parsing</li>
%%   <li>WSDL parsing and stub generation</li>
%%   <li>Service registration and discovery</li>
%%   <li>Connection testing and health checks</li>
%%   <li>SOAP fault handling</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Create a web service record:
%% ```
%% 1> Service = yawl_wsif:ws_service(<<"http://example.com/wsdl">>,
%%                                  <<"MyService">>,
%%                                  <<"MyPort">>,
%%                                  <<"MyOperation">>,
%%                                  <<"http://example.com/endpoint">>,
%%                                  undefined).
%% {ws_service,<<"service_",...>>,...}
%% ```
%%
%% Parse a SOAP fault from XML:
%% ```
%% 1> FaultXml = <<"<soap:Envelope><soap:Body><soap:Fault><faultcode>Client</faultcode><faultstring>Invalid input</faultstring></soap:Fault></soap:Body></soap:Envelope>">>.
%% <<"...">>
%% 2> yawl_wsif:parse_soap_fault(FaultXml).
%% {fault,#{detail => undefined,faultactor => undefined,...}}
%% ```
%%
%% Build a SOAP fault message:
%% ```
%% 1> Fault = yawl_wsif:build_soap_fault(<<"Client">>, <<"Invalid input">>, undefined).
%% ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",...]
%% 2> iolist_to_binary(Fault).
%% <<"<soap:Envelope>...",...>>
%% ```
%%
%% Query string building:
%% ```
%% 1> yawl_wsif:build_query_string(#{a => 1, b => "test"}).
%% "a=1&b=test"
%% 2> yawl_wsif:build_query_string(#{}).
%% ""
%% ```
%%
%% JSON encoding:
%% ```
%% 1> yawl_wsif:encode_json(#{key => <<"value">>}).
%% <<"{\"key\":\"value\"}">>
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_wsif).
-behaviour(gen_server).

%% Include xmerl for XML parsing
-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Web Service Record
-export([ws_service/0, ws_service/6]).
-export([get_service_id/1, get_wsdl/1, get_service/1, get_port/1,
         get_operation/1, get_endpoint/1]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%% WSIF API
-export([start_wsif/0, start_wsif/1, stop_wsif/0]).
-export([register_service/2, unregister_service/1]).
-export([invoke_service/3, invoke_rest/3, invoke_soap/3]).
-export([parse_wsdl/1, generate_stub/2, test_connection/1]).
-export([parse_soap_fault/1, build_soap_fault/3]).
-export([list_services/0, get_service_by_id/1]).

%% Doctest
-export([doctest_test/0]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type service_id() :: binary().
-type wsdl_url() :: binary().
-type service_name() :: binary().
-type port_name() :: binary().
-type operation_name() :: binary().
-type endpoint_url() :: binary().

-type http_method() :: get | post | put | delete | patch | head | options.

-type soap_version() :: '1.1' | '1.2'.
-type wsdl_data() :: #{
    services => [binary()],
    ports => [binary()],
    operations => [binary()],
    port_types => [binary()],
    bindings => [binary()]
}.
-type soap_fault() :: #{
    faultcode => binary(),
    faultstring => binary(),
    faultactor => binary() | undefined,
    detail => binary() | undefined
}.

-record(ws_service, {
    id :: service_id(),
    wsdl :: wsdl_url() | undefined,
    service :: service_name(),
    port :: port_name(),
    operation :: operation_name(),
    endpoint :: endpoint_url()
}).

-record(wsif_state, {
    services = #{} :: #{service_id() => #ws_service{}},
    name_index = #{} :: #{service_name() => service_id()},
    http_client = httpc :: httpc | gun
}).

-type invoke_result() :: {ok, binary()} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the WSIF server with default options.
%% @end
%%--------------------------------------------------------------------
-spec start_wsif() -> {ok, pid()} | {error, term()}.
start_wsif() ->
    start_wsif([]).

%%--------------------------------------------------------------------
%% @doc Starts the WSIF server with options.
%% Options:
%%   - {http_client, httpc | gun}: Default httpc
%% @end
%%--------------------------------------------------------------------
-spec start_wsif(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_wsif(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Stops the WSIF server.
%% @end
%%--------------------------------------------------------------------
-spec stop_wsif() -> ok.
stop_wsif() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Registers a web service for later invocation.
%% @end
%%--------------------------------------------------------------------
-spec register_service(service_name(), proplists:proplist()) ->
          {ok, service_id()} | {error, term()}.
register_service(ServiceName, Config) when is_binary(ServiceName), is_list(Config) ->
    gen_server:call(?MODULE, {register_service, ServiceName, Config}).

%%--------------------------------------------------------------------
%% @doc Unregisters a web service.
%% @end
%%--------------------------------------------------------------------
-spec unregister_service(service_id()) -> ok | {error, not_found}.
unregister_service(ServiceId) when is_binary(ServiceId) ->
    gen_server:call(?MODULE, {unregister_service, ServiceId}).

%%--------------------------------------------------------------------
%% @doc Invokes a registered service with parameters.
%% @end
%%--------------------------------------------------------------------
-spec invoke_service(service_id(), binary(), map()) -> invoke_result().
invoke_service(ServiceId, Operation, Params) when is_binary(ServiceId) ->
    gen_server:call(?MODULE, {invoke_service, ServiceId, Operation, Params}).

%%--------------------------------------------------------------------
%% @doc Invokes a REST endpoint directly.
%% @end
%%--------------------------------------------------------------------
-spec invoke_rest(http_method(), endpoint_url(), map()) -> invoke_result().
invoke_rest(Method, Url, Params) when is_atom(Method), is_binary(Url) ->
    do_rest_request(Method, Url, Params).

%%--------------------------------------------------------------------
%% @doc Invokes a SOAP service with operation and parameters.
%% @end
%%--------------------------------------------------------------------
-spec invoke_soap(endpoint_url(), operation_name(), map()) -> invoke_result().
invoke_soap(Endpoint, Operation, Params) when is_binary(Endpoint), is_binary(Operation) ->
    do_soap_request(Endpoint, Operation, Params).

%%--------------------------------------------------------------------
%% @doc Parses a WSDL file and extracts service definitions.
%% @end
%%--------------------------------------------------------------------
-spec parse_wsdl(wsdl_url() | binary()) -> {ok, wsdl_data()} | {error, term()}.
parse_wsdl(WsdlUrl) when is_binary(WsdlUrl) ->
    case httpc:request(get, {binary_to_list(WsdlUrl), []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, WsdlXml}} ->
            parse_wsdl_xml(WsdlXml);
        {ok, {{_, Code, _}, _, _}} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Generates a client stub from WSDL for a service.
%% @end
%%--------------------------------------------------------------------
-spec generate_stub(wsdl_url(), service_name()) -> {ok, binary()} | {error, term()}.
generate_stub(WsdlUrl, ServiceName) when is_binary(WsdlUrl), is_binary(ServiceName) ->
    case parse_wsdl(WsdlUrl) of
        {ok, WsdlData} ->
            StubCode = generate_stub_code(ServiceName, WsdlData),
            {ok, StubCode};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Tests connectivity to a service endpoint.
%% @end
%%--------------------------------------------------------------------
-spec test_connection(service_id() | endpoint_url()) ->
          {ok, binary()} | {error, term()}.
test_connection(ServiceId) when is_binary(ServiceId) ->
    gen_server:call(?MODULE, {test_connection, ServiceId}).

%%--------------------------------------------------------------------
%% @doc Lists all registered services.
%% @end
%%--------------------------------------------------------------------
-spec list_services() -> [{service_id(), service_name()}].
list_services() ->
    gen_server:call(?MODULE, list_services).

%%--------------------------------------------------------------------
%% @doc Gets a service by ID.
%% @end
%%--------------------------------------------------------------------
-spec get_service_by_id(service_id()) -> {ok, #ws_service{}} | {error, not_found}.
get_service_by_id(ServiceId) when is_binary(ServiceId) ->
    gen_server:call(?MODULE, {get_service, ServiceId}).

%%--------------------------------------------------------------------
%% @doc Parses a SOAP fault from an XML response.
%% Returns `no_fault` if no fault element is found, or `{fault, FaultMap}`.
%% @end
%%--------------------------------------------------------------------
-spec parse_soap_fault(binary()) -> no_fault | {fault, soap_fault()}.
parse_soap_fault(XmlBin) ->
    try
        Xml = unicode:characters_to_list(XmlBin),
        {XmlDoc, _} = xmerl_scan:string(Xml, [{namespace_conformant, true}]),
        find_fault_element(XmlDoc)
    catch
        _:_ -> no_fault
    end.

%%--------------------------------------------------------------------
%% @doc Builds a SOAP fault XML message.
%% @end
%%--------------------------------------------------------------------
-spec build_soap_fault(binary(), binary(), binary() | undefined) -> iolist().
build_soap_fault(FaultCode, FaultString, Detail) ->
    DetailXml = case Detail of
        undefined -> [];
        _ -> ["      <detail>", Detail, "</detail>\n"]
    end,
    [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n",
        "  <soap:Body>\n",
        "    <soap:Fault>\n",
        "      <faultcode>", FaultCode, "</faultcode>\n",
        "      <faultstring>", FaultString, "</faultstring>\n",
        DetailXml,
        "    </soap:Fault>\n",
        "  </soap:Body>\n",
        "</soap:Envelope>"
    ].

%%====================================================================
%% Record Constructors and Accessors
%%====================================================================

%% @private
-spec ws_service() -> #ws_service{}.
ws_service() -> #ws_service{}.

%% @private
-spec ws_service(wsdl_url() | undefined, service_name(), port_name(),
                 operation_name(), endpoint_url(), service_id() | undefined) -> #ws_service{}.
ws_service(Wsdl, Service, Port, Operation, Endpoint, Id) ->
    #ws_service{
        id = coalesce(Id, generate_id(<<"service">>)),
        wsdl = Wsdl,
        service = Service,
        port = Port,
        operation = Operation,
        endpoint = Endpoint
    }.

%% @private
-spec get_service_id(#ws_service{}) -> service_id().
get_service_id(#ws_service{id = Id}) -> Id.

%% @private
-spec get_wsdl(#ws_service{}) -> wsdl_url() | undefined.
get_wsdl(#ws_service{wsdl = Wsdl}) -> Wsdl.

%% @private
-spec get_service(#ws_service{}) -> service_name().
get_service(#ws_service{service = Service}) -> Service.

%% @private
-spec get_port(#ws_service{}) -> port_name().
get_port(#ws_service{port = Port}) -> Port.

%% @private
-spec get_operation(#ws_service{}) -> operation_name().
get_operation(#ws_service{operation = Operation}) -> Operation.

%% @private
-spec get_endpoint(#ws_service{}) -> endpoint_url().
get_endpoint(#ws_service{endpoint = Endpoint}) -> Endpoint.

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

%% @private
init(Options) ->
    HttpClient = proplists:get_value(http_client, Options, httpc),
    inets:start(httpc, [{profile, default}]),
    State = #wsif_state{http_client = HttpClient},
    {ok, State}.

%% @private
handle_call({register_service, ServiceName, Config}, _From, State) ->
    {Reply, NewState} = do_register_service(ServiceName, Config, State),
    {reply, Reply, NewState};

handle_call({unregister_service, ServiceId}, _From, State) ->
    {Reply, NewState} = do_unregister_service(ServiceId, State),
    {reply, Reply, NewState};

handle_call({invoke_service, ServiceId, Operation, Params}, _From, State) ->
    Reply = do_invoke_service(ServiceId, Operation, Params, State),
    {reply, Reply, State};

handle_call({test_connection, ServiceId}, _From, State) ->
    Reply = do_test_connection(ServiceId, State),
    {reply, Reply, State};

handle_call(list_services, _From, #wsif_state{services = Services} = State) ->
    List = [{Id, S#ws_service.service} || Id <- maps:keys(Services), S <- [maps:get(Id, Services)]],
    {reply, List, State};

handle_call({get_service, ServiceId}, _From, #wsif_state{services = Services} = State) ->
    Reply = case maps:get(ServiceId, Services, undefined) of
        undefined -> {error, not_found};
        Service -> {ok, Service}
    end,
    {reply, Reply, State};

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
%% @private Registers a service.
%% @end
%%--------------------------------------------------------------------
-spec do_register_service(service_name(), proplists:proplist(), #wsif_state{}) ->
          {{ok, service_id()} | {error, already_exists}, #wsif_state{}}.
do_register_service(ServiceName, Config, #wsif_state{services = Services,
                                                      name_index = NameIndex} = State) ->
    case maps:get(ServiceName, NameIndex, undefined) of
        undefined ->
            Wsdl = proplists:get_value(wsdl, Config),
            Port = proplists:get_value(port, Config, <<"default">>),
            Operation = proplists:get_value(operation, Config, <<"default">>),
            Endpoint = proplists:get_value(endpoint, Config),

            Service = #ws_service{
                id = generate_id(<<"service">>),
                wsdl = Wsdl,
                service = ServiceName,
                port = Port,
                operation = Operation,
                endpoint = Endpoint
            },
            ServiceId = Service#ws_service.id,
            NewServices = maps:put(ServiceId, Service, Services),
            NewIndex = maps:put(ServiceName, ServiceId, NameIndex),
            {{ok, ServiceId}, State#wsif_state{services = NewServices, name_index = NewIndex}};
        _ServiceId ->
            {{error, already_exists}, State}
    end.

%%--------------------------------------------------------------------
%% @private Unregisters a service.
%% @end
%%--------------------------------------------------------------------
-spec do_unregister_service(service_id(), #wsif_state{}) ->
          {ok | {error, not_found}, #wsif_state{}}.
do_unregister_service(ServiceId, #wsif_state{services = Services,
                                             name_index = NameIndex} = State) ->
    case maps:get(ServiceId, Services, undefined) of
        undefined ->
            {{error, not_found}, State};
        #ws_service{service = ServiceName} ->
            NewServices = maps:remove(ServiceId, Services),
            NewIndex = maps:remove(ServiceName, NameIndex),
            {ok, State#wsif_state{services = NewServices, name_index = NewIndex}}
    end.

%%--------------------------------------------------------------------
%% @private Invokes a registered service.
%% @end
%%--------------------------------------------------------------------
-spec do_invoke_service(service_id(), binary(), map(), #wsif_state{}) -> invoke_result().
do_invoke_service(ServiceId, Operation, Params, #wsif_state{services = Services}) ->
    case maps:get(ServiceId, Services, undefined) of
        undefined ->
            {error, not_found};
        #ws_service{endpoint = Endpoint, operation = DefaultOp} = Service ->
            Op = coalesce(Operation, DefaultOp),
            invoke_service_internal(Service#ws_service{operation = Op}, Endpoint, Params)
    end.

%%--------------------------------------------------------------------
%% @private Internal service invocation.
%% @end
%%--------------------------------------------------------------------
-spec invoke_service_internal(#ws_service{}, endpoint_url(), map()) -> invoke_result().
invoke_service_internal(#ws_service{wsdl = undefined}, Endpoint, Params) ->
    % Assume REST if no WSDL
    do_rest_request(post, Endpoint, Params);
invoke_service_internal(#ws_service{wsdl = _Wsdl, operation = Operation}, Endpoint, Params) ->
    % Use SOAP if WSDL is defined
    do_soap_request(Endpoint, Operation, Params).

%%--------------------------------------------------------------------
%% @private Performs a REST request.
%% @end
%%--------------------------------------------------------------------
-spec do_rest_request(http_method(), endpoint_url(), map()) -> invoke_result().
do_rest_request(Method, Url, Params) ->
    UrlStr = binary_to_list(Url),
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"}],

    case Method of
        get ->
            QueryParams = build_query_string(Params),
            FullUrl = case QueryParams of
                "" -> UrlStr;
                _ -> UrlStr ++ "?" ++ QueryParams
            end,
            case httpc:request(get, {FullUrl, Headers}, [], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, Body}} -> {ok, Body};
                {ok, {{_, Code, _}, _, Body}} -> {error, {http_error, Code, Body}};
                {error, Reason} -> {error, Reason}
            end;
        post ->
            Body = encode_json(Params),
            case httpc:request(post, {UrlStr, Headers, "application/json", Body}, [], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, RespBody}} -> {ok, RespBody};
                {ok, {{_, 201, _}, _, RespBody}} -> {ok, RespBody};
                {ok, {{_, Code, _}, _, RespBody}} -> {error, {http_error, Code, RespBody}};
                {error, Reason} -> {error, Reason}
            end;
        put ->
            Body = encode_json(Params),
            case httpc:request(put, {UrlStr, Headers, "application/json", Body}, [], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, RespBody}} -> {ok, RespBody};
                {ok, {{_, Code, _}, _, RespBody}} -> {error, {http_error, Code, RespBody}};
                {error, Reason} -> {error, Reason}
            end;
        delete ->
            case httpc:request(delete, {UrlStr, Headers}, [], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, RespBody}} -> {ok, RespBody};
                {ok, {{_, 204, _}, _, _}} -> {ok, <<"">>};
                {ok, {{_, Code, _}, _, RespBody}} -> {error, {http_error, Code, RespBody}};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, unsupported_method}
    end.

%%--------------------------------------------------------------------
%% @private Performs a SOAP request.
%% @end
%%--------------------------------------------------------------------
-spec do_soap_request(endpoint_url(), operation_name(), map()) -> invoke_result().
do_soap_request(Endpoint, Operation, Params) ->
    SoapEnvelope = build_soap_envelope(Operation, Params, '1.1'),
    Headers = [{"Content-Type", "text/xml; charset=utf-8"},
               {"SOAPAction", lists:flatten([$", binary_to_list(Operation), $"])}],

    case httpc:request(post, {binary_to_list(Endpoint), Headers, "text/xml", SoapEnvelope},
                       [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            case parse_soap_fault(Body) of
                no_fault ->
                    {ok, extract_soap_body(Body)};
                {fault, Fault} ->
                    {error, {soap_fault, Fault}}
            end;
        {ok, {{_, 500, _}, _, Body}} ->
            case parse_soap_fault(Body) of
                no_fault -> {error, {http_error, 500, Body}};
                {fault, Fault} -> {error, {soap_fault, Fault}}
            end;
        {ok, {{_, Code, _}, _, Body}} ->
            {error, {http_error, Code, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private Builds a SOAP envelope with version support.
%% @end
%%--------------------------------------------------------------------
-spec build_soap_envelope(operation_name(), map(), soap_version()) -> iolist().
build_soap_envelope(Operation, Params, Version) ->
    {NsPrefix, NsUri} = soap_namespace(Version),
    ParamXml = build_params_xml(Params),
    [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<", NsPrefix, ":Envelope xmlns:", NsPrefix, "=\"", NsUri, "\">\n",
        "  <", NsPrefix, ":Body>\n",
        "    <", Operation, ">\n",
        ParamXml,
        "    </", Operation, ">\n",
        "  </", NsPrefix, ":Body>\n",
        "</", NsPrefix, ":Envelope>"
    ].

%%--------------------------------------------------------------------
%% @private Returns the namespace prefix and URI for a SOAP version.
%% @end
%%--------------------------------------------------------------------
-spec soap_namespace(soap_version()) -> {binary(), binary()}.
soap_namespace('1.1') ->
    {<<"soap">>, <<"http://schemas.xmlsoap.org/soap/envelope/">>};
soap_namespace('1.2') ->
    {<<"soapenv">>, <<"http://www.w3.org/2003/05/soap-envelope">>}.

%%--------------------------------------------------------------------
%% @private Builds XML for parameters.
%% @end
%%--------------------------------------------------------------------
-spec build_params_xml(map()) -> iolist().
build_params_xml(Params) when map_size(Params) =:= 0 ->
    [];
build_params_xml(Params) ->
    maps:fold(fun(K, V, Acc) ->
        ["      <", atom_to_list(K), ">", format_value(V), "</", atom_to_list(K), ">\n" | Acc]
    end, [], Params).

%%--------------------------------------------------------------------
%% @private Formats a value for XML.
%% @end
%%--------------------------------------------------------------------
-spec format_value(term()) -> iolist().
format_value(V) when is_binary(V) -> V;
format_value(V) when is_list(V) -> V;
format_value(V) when is_integer(V) -> integer_to_list(V);
format_value(V) when is_float(V) -> float_to_list(V, [{decimals, 6}, compact]);
format_value(V) -> atom_to_list(V).

%%--------------------------------------------------------------------
%% @private Extracts the SOAP body from a response using xmerl.
%% Handles both soap:Body and soapenv:Body namespaces.
%% @end
%%--------------------------------------------------------------------
-spec extract_soap_body(binary()) -> binary().
extract_soap_body(XmlBin) ->
    try
        Xml = unicode:characters_to_list(XmlBin),
        {XmlDoc, _} = xmerl_scan:string(Xml, [{namespace_conformant, true}]),
        case find_soap_body(XmlDoc) of
            {ok, BodyContent} ->
                BodyXml = xmerl:export_simple([BodyContent], xmerl_xml),
                unicode:characters_to_binary(BodyXml);
            {error, _} ->
                XmlBin
        end
    catch
        _:_ -> XmlBin
    end.

%%--------------------------------------------------------------------
%% @private Finds the SOAP Body element in an xmerl document.
%% Handles both soap:Body and soapenv:Body.
%% @end
%%--------------------------------------------------------------------
-spec find_soap_body(#xmlElement{}) -> {ok, #xmlElement{}} | {error, term()}.
find_soap_body(#xmlElement{content = Content}) ->
    find_element_by_local_name(Content, <<"Body">>);
find_soap_body(_) ->
    {error, not_an_element}.

%%--------------------------------------------------------------------
%% @private Finds a SOAP Fault element and extracts its details.
%% @end
%%--------------------------------------------------------------------
-spec find_fault_element(#xmlElement{} | [#xmlElement{}]) -> no_fault | {fault, soap_fault()}.
find_fault_element([]) ->
    no_fault;
find_fault_element([#xmlElement{name = Name} = El | Rest]) ->
    case is_fault_element(Name) of
        true ->
            {fault, extract_fault_details(El)};
        false ->
            % Not a fault, check children and then continue with rest
            case find_fault_element(El) of
                no_fault -> find_fault_element(Rest);
                Found -> Found
            end
    end;
find_fault_element([#xmlText{} | Rest]) ->
    find_fault_element(Rest);
find_fault_element([#xmlComment{} | Rest]) ->
    find_fault_element(Rest);
find_fault_element([_ | Rest]) ->
    find_fault_element(Rest);
find_fault_element(#xmlElement{name = Name} = El) ->
    % Single element - check if it's a fault
    case is_fault_element(Name) of
        true -> {fault, extract_fault_details(El)};
        false -> find_fault_element(element(9, El))
    end;
find_fault_element(_) ->
    no_fault.

%%--------------------------------------------------------------------
%% @private Checks if an element name indicates a SOAP Fault.
%% Handles namespaced and non-namespaced variants.
%% @end
%%--------------------------------------------------------------------
-spec is_fault_element(atom()) -> boolean().
is_fault_element('Fault') -> true;
is_fault_element('soap:Fault') -> true;
is_fault_element('soapenv:Fault') -> true;
is_fault_element(_) -> false.

%%--------------------------------------------------------------------
%% @private Extracts fault details from a SOAP Fault element.
%% @end
%%--------------------------------------------------------------------
-spec extract_fault_details(#xmlElement{}) -> soap_fault().
extract_fault_details(#xmlElement{content = Content}) ->
    FaultCode = extract_element_text(Content, <<"faultcode">>, <<"Client">>),
    FaultString = extract_element_text(Content, <<"faultstring">>, <<"Unknown error">>),
    FaultActor = extract_optional_text(Content, <<"faultactor">>),
    Detail = extract_optional_text(Content, <<"detail">>),
    #{
        faultcode => FaultCode,
        faultstring => FaultString,
        faultactor => FaultActor,
        detail => Detail
    }.

%%--------------------------------------------------------------------
%% @private Parses WSDL XML content using xmerl.
%% @end
%%--------------------------------------------------------------------
-spec parse_wsdl_xml(binary()) -> {ok, wsdl_data()} | {error, term()}.
parse_wsdl_xml(XmlBin) ->
    try
        Xml = unicode:characters_to_list(XmlBin),
        Opts = [{namespace_conformant, true}, {space, normalize}],
        {XmlDoc, _} = xmerl_scan:string(Xml, Opts),
        case validate_wsdl_structure(XmlDoc) of
            ok ->
                Services = extract_wsdl_services(XmlDoc),
                Ports = extract_wsdl_ports(XmlDoc),
                Operations = extract_wsdl_operations(XmlDoc),
                PortTypes = extract_wsdl_port_types(XmlDoc),
                Bindings = extract_wsdl_bindings(XmlDoc),
                {ok, #{
                    services => Services,
                    ports => Ports,
                    operations => Operations,
                    port_types => PortTypes,
                    bindings => Bindings
                }};
            {error, _} = Error ->
                Error
        end
    catch
        _:ParseReason -> {error, {wsdl_parse_error, ParseReason}}
    end.

%%--------------------------------------------------------------------
%% @private Validates that the WSDL has the required structure.
%% @end
%%--------------------------------------------------------------------
-spec validate_wsdl_structure(#xmlElement{}) -> ok | {error, term()}.
validate_wsdl_structure(#xmlElement{name = Name}) when Name =:= 'definitions'; Name =:= 'wsdl:definitions' ->
    ok;
validate_wsdl_structure(#xmlElement{content = Content}) ->
    case has_element_with_name(Content, 'definitions') orelse
         has_element_with_name(Content, 'wsdl:definitions') of
        true -> ok;
        false -> {error, missing_definitions_element}
    end;
validate_wsdl_structure(_) ->
    {error, invalid_wsdl_root}.

%%--------------------------------------------------------------------
%% @private Extracts service names from WSDL.
%% @end
%%--------------------------------------------------------------------
-spec extract_wsdl_services(#xmlElement{}) -> [binary()].
extract_wsdl_services(XmlDoc) ->
    Elements = find_all_elements(XmlDoc, 'service'),
    [extract_name_attribute(El) || El <- Elements].

%%--------------------------------------------------------------------
%% @private Extracts port names from WSDL.
%% @end
%%--------------------------------------------------------------------
-spec extract_wsdl_ports(#xmlElement{}) -> [binary()].
extract_wsdl_ports(XmlDoc) ->
    Elements = find_all_elements(XmlDoc, 'port'),
    [extract_name_attribute(El) || El <- Elements].

%%--------------------------------------------------------------------
%% @private Extracts operation names from WSDL.
%% @end
%%--------------------------------------------------------------------
-spec extract_wsdl_operations(#xmlElement{}) -> [binary()].
extract_wsdl_operations(XmlDoc) ->
    Elements = find_all_elements(XmlDoc, 'operation'),
    [extract_name_attribute(El) || El <- Elements].

%%--------------------------------------------------------------------
%% @private Extracts port type names from WSDL.
%% @end
%%--------------------------------------------------------------------
-spec extract_wsdl_port_types(#xmlElement{}) -> [binary()].
extract_wsdl_port_types(XmlDoc) ->
    Elements = find_all_elements(XmlDoc, 'portType'),
    [extract_name_attribute(El) || El <- Elements].

%%--------------------------------------------------------------------
%% @private Extracts binding names from WSDL.
%% @end
%%--------------------------------------------------------------------
-spec extract_wsdl_bindings(#xmlElement{}) -> [binary()].
extract_wsdl_bindings(XmlDoc) ->
    Elements = find_all_elements(XmlDoc, 'binding'),
    [extract_name_attribute(El) || El <- Elements].

%%--------------------------------------------------------------------
%% @private Finds all elements with a given name in an xmerl document.
%% @end
%%--------------------------------------------------------------------
-spec find_all_elements(#xmlElement{} | [#xmlNode{}], atom()) -> [#xmlElement{}].
find_all_elements(XmlDoc, TargetName) ->
    find_all_elements(XmlDoc, TargetName, []).

find_all_elements([], _TargetName, Acc) ->
    lists:reverse(Acc);
find_all_elements([#xmlElement{name = Name} = El | Rest], TargetName, Acc) ->
    NewAcc = case element_name_match(Name, TargetName) of
        true -> [El | Acc];
        false -> Acc
    end,
    find_all_children(El, TargetName, NewAcc, Rest);
find_all_elements([#xmlText{} | Rest], TargetName, Acc) ->
    find_all_elements(Rest, TargetName, Acc);
find_all_elements([#xmlComment{} | Rest], TargetName, Acc) ->
    find_all_elements(Rest, TargetName, Acc);
find_all_elements([#xmlPI{} | Rest], TargetName, Acc) ->
    find_all_elements(Rest, TargetName, Acc);
find_all_elements([#xmlDecl{} | Rest], TargetName, Acc) ->
    find_all_elements(Rest, TargetName, Acc);
find_all_elements([#xmlNamespace{} | Rest], TargetName, Acc) ->
    find_all_elements(Rest, TargetName, Acc);
find_all_elements([_ | Rest], TargetName, Acc) ->
    find_all_elements(Rest, TargetName, Acc).

find_all_children(#xmlElement{content = Content}, TargetName, Acc, Rest) ->
    find_all_elements(Content ++ Rest, TargetName, Acc).

%%--------------------------------------------------------------------
%% @private Checks if an element name matches (with or without namespace).
%% @end
%%--------------------------------------------------------------------
-spec element_name_match(atom(), atom()) -> boolean().
element_name_match(Name, Name) -> true;
element_name_match(_, _) -> false.

%%--------------------------------------------------------------------
%% @private Extracts the name attribute from an element.
%% @end
%%--------------------------------------------------------------------
-spec extract_name_attribute(#xmlElement{}) -> binary().
extract_name_attribute(#xmlElement{attributes = Attrs}) ->
    case lists:keyfind(name, 1, Attrs) of
        #xmlAttribute{value = Value} when is_list(Value) ->
            unicode:characters_to_binary(Value, utf8);
        #xmlAttribute{value = Value} when is_binary(Value) ->
            Value;
        false ->
            <<"unnamed">>
    end;
extract_name_attribute(_) ->
    <<"unnamed">>.

%%--------------------------------------------------------------------
%% @private Checks if content has an element with the given name.
%% @end
%%--------------------------------------------------------------------
-spec has_element_with_name([#xmlNode{}], atom()) -> boolean().
has_element_with_name([], _Name) ->
    false;
has_element_with_name([#xmlElement{name = Name} | _Rest], Name) ->
    true;
has_element_with_name([#xmlElement{content = Content} | Rest], Name) ->
    has_element_with_name(Content, Name) orelse has_element_with_name(Rest, Name);
has_element_with_name([_ | Rest], Name) ->
    has_element_with_name(Rest, Name).

%%--------------------------------------------------------------------
%% @private Finds an element by local name in content.
%% @end
%%--------------------------------------------------------------------
-spec find_element_by_local_name([#xmlNode{}], binary()) -> {ok, #xmlElement{}} | {error, term()}.
find_element_by_local_name([], _Name) ->
    {error, element_not_found};
find_element_by_local_name([#xmlElement{name = Name} = El | Rest], TargetName) ->
    NameBin = atom_to_binary(Name, utf8),
    TargetBin = TargetName,
    case binary:match(NameBin, TargetBin) of
        nomatch when NameBin =:= TargetBin ->
            {ok, El};
        nomatch ->
            case binary:match(NameBin, <<":">>) of
                {Pos, _} ->
                    LocalName = binary:part(NameBin, Pos + 1, byte_size(NameBin) - Pos - 1),
                    case LocalName of
                        TargetBin -> {ok, El};
                        _ -> find_element_by_local_name(Rest, TargetName)
                    end;
                nomatch ->
                    find_element_by_local_name(Rest, TargetName)
            end;
        _ ->
            {ok, El}
    end;
find_element_by_local_name([#xmlText{} | Rest], Name) ->
    find_element_by_local_name(Rest, Name);
find_element_by_local_name([#xmlComment{} | Rest], Name) ->
    find_element_by_local_name(Rest, Name);
find_element_by_local_name([_ | Rest], Name) ->
    find_element_by_local_name(Rest, Name).

%%--------------------------------------------------------------------
%% @private Extracts text from an element with a given name.
%% @end
%%--------------------------------------------------------------------
-spec extract_element_text([#xmlNode{}], binary(), binary()) -> binary().
extract_element_text(Content, Name, Default) ->
    case find_element_by_local_name(Content, Name) of
        {ok, #xmlElement{content = ElementContent}} ->
            Text = extract_text_content(ElementContent, <<>>),
            case Text of
                <<>> -> Default;
                _ -> Text
            end;
        {error, _} ->
            Default
    end.

%%--------------------------------------------------------------------
%% @private Extracts optional text from an element.
%% @end
%%--------------------------------------------------------------------
-spec extract_optional_text([#xmlNode{}], binary()) -> binary() | undefined.
extract_optional_text(Content, Name) ->
    case find_element_by_local_name(Content, Name) of
        {ok, #xmlElement{content = ElementContent}} ->
            Text = extract_text_content(ElementContent, <<>>),
            case Text of
                <<>> -> undefined;
                _ -> Text
            end;
        {error, _} ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @private Extracts text content from element content.
%% @end
%%--------------------------------------------------------------------
-spec extract_text_content([#xmlNode{}], binary()) -> binary().
extract_text_content([], Acc) ->
    BinAcc = case is_list(Acc) of
        true -> unicode:characters_to_binary(Acc, utf8);
        false -> Acc
    end,
    Trimmed = trim_binary(BinAcc),
    Trimmed;
extract_text_content([#xmlText{value = Text} | Rest], Acc) when is_list(Text); is_binary(Text) ->
    BinAcc = case is_list(Acc) of
        true -> unicode:characters_to_binary(Acc, utf8);
        false -> Acc
    end,
    BinText = case is_list(Text) of
        true -> unicode:characters_to_binary(Text, utf8);
        false -> Text
    end,
    extract_text_content(Rest, <<BinAcc/binary, BinText/binary>>);
extract_text_content([_ | Rest], Acc) ->
    extract_text_content(Rest, Acc).

%% @private Trims leading and trailing whitespace from a binary.
trim_binary(Bin) ->
    TrimmedL = trim_leading(Bin),
    trim_trailing(TrimmedL).

trim_leading(<<$\s, Rest/binary>>) -> trim_leading(Rest);
trim_leading(<<$\n, Rest/binary>>) -> trim_leading(Rest);
trim_leading(<<$\r, Rest/binary>>) -> trim_leading(Rest);
trim_leading(<<$\t, Rest/binary>>) -> trim_leading(Rest);
trim_leading(Bin) -> Bin.

trim_trailing(Bin) -> trim_trailing(Bin, size(Bin) - 1).

trim_trailing(Bin, Pos) when Pos < 0 -> Bin;
trim_trailing(Bin, Pos) ->
    case Bin of
        <<Pre:Pos/binary, $\s>> -> trim_trailing(Pre, Pos - 1);
        <<Pre:Pos/binary, $\n>> -> trim_trailing(Pre, Pos - 1);
        <<Pre:Pos/binary, $\r>> -> trim_trailing(Pre, Pos - 1);
        <<Pre:Pos/binary, $\t>> -> trim_trailing(Pre, Pos - 1);
        _ -> Bin
    end.

%%--------------------------------------------------------------------
%% @private Tests connection to a service.
%% @end
%%--------------------------------------------------------------------
-spec do_test_connection(service_id() | endpoint_url(), #wsif_state{}) ->
          {ok, binary()} | {error, term()}.
do_test_connection(ServiceId, #wsif_state{services = Services}) when is_binary(ServiceId) ->
    case maps:get(ServiceId, Services, undefined) of
        undefined ->
            {error, not_found};
        #ws_service{endpoint = Endpoint} ->
            test_endpoint(Endpoint)
    end;
do_test_connection(Endpoint, _State) ->
    test_endpoint(Endpoint).

%%--------------------------------------------------------------------
%% @private Tests an endpoint with a simple HEAD request.
%% @end
%%--------------------------------------------------------------------
-spec test_endpoint(endpoint_url()) -> {ok, binary()} | {error, term()}.
test_endpoint(Endpoint) ->
    UrlStr = binary_to_list(Endpoint),
    case httpc:request(head, {UrlStr, []}, [], []) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200, Code < 500 ->
            {ok, <<"Connected">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private Generates stub code from parsed WSDL.
%% @end
%%--------------------------------------------------------------------
-spec generate_stub_code(service_name(), wsdl_data()) -> binary().
generate_stub_code(ServiceName, WsdlData) ->
    Operations = maps:get(operations, WsdlData, []),
    Functions = [[
        "-export([", Op/0, "]).\n",
        Op, "() ->\n    yawl_wsif:invoke_service(\n        ServiceId,\n        \"", Op, "\",\n        #{}\n    ).\n"
    ] || Op <- Operations],

    Stub = [
        "%% Auto-generated stub for ", ServiceName, "\n",
        "-module(", ServiceName, ").\n\n",
        "-include_lib(\"yawl_wsif.hrl\").\n\n",
        Functions, "\n"
    ],
    iolist_to_binary(Stub).

%%--------------------------------------------------------------------
%% @private Builds a query string from a map.
%% @end
%%--------------------------------------------------------------------
-spec build_query_string(map()) -> string().
build_query_string(Params) when map_size(Params) =:= 0 ->
    "";
build_query_string(Params) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KeyStr = case K of
            _ when is_atom(K) -> atom_to_list(K);
            _ when is_binary(K) -> binary_to_list(K);
            _ when is_list(K) -> K
        end,
        Key = urlencode(KeyStr),
        Value = urlencode(format_value(V)),
        [Key ++ "=" ++ Value | Acc]
    end, [], Params),
    string:join(lists:reverse(Pairs), "&").

%%--------------------------------------------------------------------
%% @private URL encodes a string.
%% @end
%%--------------------------------------------------------------------
-spec urlencode(binary() | string()) -> string().
urlencode(Str) when is_binary(Str) ->
    urlencode(binary_to_list(Str), []);
urlencode(Str) when is_list(Str) ->
    urlencode(Str, []).

urlencode([], Acc) ->
    lists:reverse(Acc);
urlencode([C | Rest], Acc) when C >= $0, C =< $9; C >= $a, C =< $z; C >= $A, C =< $Z; C == $-; C == $_; C == $.; C == $~ ->
    urlencode(Rest, [C | Acc]);
urlencode([C | Rest], Acc) ->
    Hex = erlang:integer_to_list(C, 16),
    urlencode(Rest, lists:reverse([$% | Hex]) ++ Acc).

%%--------------------------------------------------------------------
%% @private Encodes a map to JSON (simple implementation).
%% @end
%%--------------------------------------------------------------------
-spec encode_json(map()) -> binary().
encode_json(Map) when is_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KeyStr = case K of
            _ when is_atom(K) -> atom_to_list(K);
            _ when is_binary(K) -> binary_to_list(K);
            _ when is_list(K) -> K
        end,
        Key = io_lib:format("\"~s\"", [KeyStr]),
        Value = encode_json_value(V),
        [[Key, ":", Value] | Acc]
    end, [], Map),
    case lists:reverse(Pairs) of
        [] -> <<"{}">>;
        RevPairs ->
            Joined = lists:join(",", RevPairs),
            iolist_to_binary(["{", Joined, "}"])
    end.

encode_json_value(V) when is_binary(V) ->
    ["\"", binary_to_list(V), "\""];
encode_json_value(V) when is_list(V) ->
    ["\"", V, "\""];
encode_json_value(V) when is_integer(V) ->
    integer_to_list(V);
encode_json_value(V) when is_float(V) ->
    float_to_list(V, [{decimals, 6}, compact]);
encode_json_value(V) when is_boolean(V) ->
    atom_to_list(V);
encode_json_value(null) ->
    "null";
encode_json_value(Map) when is_map(Map) ->
    binary_to_list(encode_json(Map)).

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

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Runs doctests for the yawl_wsif module.
%%
%% Usage:
%% ```erlang
%% 1> yawl_wsif:doctest_test().
%% ok
%% ```
%%
%% The test validates:
%% 1. Service record construction
%% 2. SOAP fault parsing
%% 3. SOAP fault building
%% 4. Query string building
%% 5. JSON encoding
%% 6. URL encoding
%% 7. Service ID generation
%% 8. Coalesce function
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Service record construction
    Service = ws_service(
        <<"http://example.com/wsdl">>,
        <<"MyService">>,
        <<"MyPort">>,
        <<"MyOperation">>,
        <<"http://example.com/endpoint">>,
        undefined
    ),
    true = is_record(Service, ws_service),
    <<"MyService">> = get_service(Service),
    <<"MyPort">> = get_port(Service),

    %% Test 2: SOAP fault element detection
    true = is_fault_element('Fault'),
    true = is_fault_element('soap:Fault'),
    true = is_fault_element('soapenv:Fault'),
    false = is_fault_element('Body'),

    %% Test 3: Invalid XML returns no_fault (catch block)
    no_fault = parse_soap_fault(<<"not valid xml">>),

    %% Test 4: Build SOAP fault
    FaultIolist = build_soap_fault(<<"Server">>, <<"Internal error">>, undefined),
    FaultBin = iolist_to_binary(FaultIolist),
    true = binary:match(FaultBin, <<"<faultcode>Server</faultcode>">>) =/= nomatch,
    true = binary:match(FaultBin, <<"<faultstring>Internal error</faultstring>">>) =/= nomatch,

    %% Test 5: Build SOAP fault with detail
    FaultWithDetail = build_soap_fault(<<"Client">>, <<"Bad request">>, <<"Detailed error info">>),
    DetailBin = iolist_to_binary(FaultWithDetail),
    true = binary:match(DetailBin, <<"<detail>Detailed error info</detail>">>) =/= nomatch,

    %% Test 6: Query string building
    Query1 = build_query_string(#{a => 1, b => "test"}),
    true = string:str(Query1, "a=1") > 0,
    true = string:str(Query1, "b=test") > 0,
    true = string:str(Query1, "&") > 0,

    %% Test 7: Empty query string
    "" = build_query_string(#{}),

    %% Test 8: JSON encoding
    Json1 = encode_json(#{key => <<"value">>}),
    true = binary:match(Json1, <<"\"key\"">>) =/= nomatch,
    true = binary:match(Json1, <<"\"value\"">>) =/= nomatch,

    %% Test 9: Empty JSON object
    <<"{}">> = encode_json(#{}),

    %% Test 10: URL encoding
    "test%20value" = urlencode(<<"test value">>),
    "abc123-_.~" = urlencode(<<"abc123-_.~">>),

    %% Test 11: Service ID generation
    ServiceId = generate_id(<<"test">>),
    true = is_binary(ServiceId),
    true = byte_size(ServiceId) > 4,
    <<"test_", _/binary>> = ServiceId,

    %% Test 12: Coalesce function
    default = coalesce(undefined, default),
    value = coalesce(value, default),

    %% Test 13: Format values
    "123" = format_value(123),
    <<"test">> = format_value(<<"test">>),
    "string" = format_value("string"),
    "true" = format_value(true),

    %% Test 14: SOAP namespace
    {<<"soap">>, _} = soap_namespace('1.1'),
    {<<"soapenv">>, _} = soap_namespace('1.2'),

    %% Test 15: Empty params XML
    [] = build_params_xml(#{}),

    ok.
