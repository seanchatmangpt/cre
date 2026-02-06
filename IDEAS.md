# gen_pnet
###### A generic Petri net OTP behavior.
[![hex.pm](https://img.shields.io/hexpm/v/gen_pnet.svg?style=flat-square)](https://hex.pm/packages/gen_pnet)

The major advantage of modeling applications with Petri nets is that they provide a natural view on the concurrent behavior of an application. This is achieved by making explicit the preconditions for an operation to be carried out while leaving implicit how and when an operation is triggered and how independent operations are timed.

This OTP behavior allows programming with Petri nets. It implements a very general form of Petri nets using Erlang terms as tokens. This means that (i) tokens are not only markers but can be any data structure conceivable in Erlang, (ii) a place can hold any number of tokens not just one, (iii) transitions can perform any computation conceivable in Erlang.

The Petri net is specified by implementing a set of callback functions (much like the gen_fsm behavior) declaring the place names, the transition names, the preset for each transition, in what modes a transition is enabled, what happens, when a transition fires in a given mode, and the net's initial marking. To communicate with the outside world, callback functions handling calls, casts, and unformatted messages can be provided. Finally, the user can specify a trigger function that is called for each token that is about to emerge on a place. This trigger function can devise side effects and can either let the token be created normally or make it vanish. Both terminating and live nets can be defined using gen_pnet and even though a live net never finishes to make progress, the net instance is constantly responsive to outside requests. Conflicting transitions fire randomly and fairly.

The [documentation](https://cuneiform-lang.org/man/gen_pnet/index.html) of the gen_pnet module's API is available online.

## Usage

This section shows how the gen_pnet library can be added to your project, how Petri nets are defined, and how Petri net instances are started, queried, and manipulated. We demonstrate the API by constructing a cookie vending machine. The [source code](https://github.com/joergen7/gen_pnet_examples/blob/master/src/cvm.erl) of the cookie vending machine module is part of the [example collection](https://github.com/joergen7/gen_pnet_examples) for gen_pnet.

![Cookie vending machine Petri net](https://github.com/joergen7/gen_pnet/blob/dev/priv/cvm2.png)

*Cookie vending machine example net. Place and transition names are atoms while, in this example, tokens are also atoms.*

### Adding gen_pnet to a Project

#### rebar3

To integrate gen_pnet into a rebar3 managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{gen_pnet, "0.1.7"}`.

```erlang
{deps, [{gen_pnet, "0.1.7"}]}.
```

#### mix

```elixir
{:gen_pnet, "~> 0.1.7"}
```

### Defining a Petri net

Petri nets are defined by creating a callback module that implements the gen_pnet behavior by providing a number of callback functions.

### Callback Functions for the Net Structure

There are six callbacks that define the Petri net structure and its initial marking:

- `place_lst/0` returns the names of the places in the net
- `trsn_lst/0` returns the names of the transitions in the net
- `init_marking/2` returns the initial marking for a given place
- `preset/1` returns the preset places of a given transition
- `is_enabled/2` determines whether a given transition is enabled in a given mode
- `fire/3` returns which tokens are produced on what places if a given transition is fired in a given mode that enables this transition

We have a look at each of them in turn.

#### place_lst/0

```erlang
-callback place_lst() -> [atom()].
```

The `place_lst/0` function lets us define the names of all places in the net.

```erlang
place_lst() ->
  [coin_slot, cash_box, signal, storage, compartment].
```

Here, we define the net to have the five places in the cookie vending machine.

#### trsn_lst/0

```erlang
-callback trsn_lst() -> [atom()].
```

The `trsn_lst/0` function lets us define the names of all transitions in the net.

```erlang
trsn_lst() ->
  [a, b].
```

Here, we define the net to have the two places `a` and `b` in the cookie vending machine.

#### preset/1

```erlang
-callback preset( Trsn :: atom() ) -> [atom()].
```

The `preset/1` lets us define the preset places of a given transition.

```erlang
preset( a ) -> [coin_slot];
preset( b ) -> [signal, storage].
```

Here, we define the preset of the transition `a` to be just the place `coin_slot` while the transition `b` has the places `signal` and `storage` in its preset.

#### init_marking/2

```erlang
-callback init_marking( Place :: atom(), UsrInfo :: _ ) -> [_].
```

The `init_marking/2` function lets us define the initial marking for a given place in the form of a token list.

```erlang
init_marking( storage, _UsrInfo ) -> [cookie_box, cookie_box, cookie_box];
init_marking( _Place, _UsrInfo )  -> [].
```

Here, we initialize the storage place with three `cookie_box` tokens. All other places are left empty.

#### is_enabled/3

```erlang
-callback is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]}, UsrInfo :: _ ) ->
            boolean().
```

The `is_enabled/3` function is a predicate determining whether a given transition is enabled in a given mode.

```erlang
is_enabled( a, #{ coin_slot := [coin] }, _UsrInfo )                      -> true;
is_enabled( b, #{ signal := [sig], storage := [cookie_box] }, _UsrInfo ) -> true;
is_enabled( _Trsn, _Mode, _UsrInfo )                                     -> false.
```

Here, we state that the transition `a` is enabled if it can consume a single `coin` from the `coin_slot` place. Similarly, the transition `b` is enabled if it can consume a `sig` token from the `signal` place and a `cookie_box` token from the `storage` place. No other configuration can enable a transition. E.g., managing to get a `button` token on the `coin_slot` place will not enable any transition.

#### fire/3

```erlang
-callback fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.
```

The `fire/3` function defines what tokens are produced when a transition fires in a given mode. As arguments it takes the name of the transition, a firing mode in the form of a hash map mapping place names to token lists, and a user info field that was generated by `init/1`. The `fire/3` function is called only on modes for which `is_enabled/2` returns `true`. The `fire/3` function is expected to return either a `{produce, ProduceMap}` tuple or the atom `abort`. If `abort` is returned, the firing is canceled. I.e., nothing is produced or consumed.

```erlang
fire( a, _Mode, _UsrInfo ) ->
  {produce, #{ cash_box => [coin], signal => [sig] }};

fire( b, _Mode, _UsrInfo ) ->
  {produce, #{ compartment => [cookie_box] }}.
```

Here, the firing of the transition `a` produces a `coin` token on the `cash_box` place and a `sig` token on the `signal` place. Similarly, the firing of the transition `b` produces a `cookie_box` token on the `compartment` place. We do not need to state the tokens to be consumed because the firing mode already uniquely identifies the tokens to be consumed.


### Callback Functions for the Actor Interface

In addition to the structure callback functions there are another seven callback functions that determine how the net instance appears as an Erlang actor to the outside world:

- `code_change/3` determines what happens when a hot code reload appears
- `handle_call/3` synchronous message exchange
- `handle_cast/2` asynchronous message reception
- `handle_info/2` asynchronous reception of an unformatted message
- `init/1` initializes the gen_pnet instance
- `terminate/2` determines what happens when the net instance is stopped
- `trigger/3` allows to add a side effects to the generation of a token

#### code_change/3

```erlang
-callback code_change( OldVsn :: _, NetState :: #net_state{}, Extra :: _ ) ->
            {ok, #net_state{}} | {error, _}.
```

The `code_change/3` function determines what happens when a hot code reload appears. This callback is identical to the `code_change/3` function in the gen_server behavior.

```erlang
code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.
```

#### handle_call/3

```erlang
-callback handle_call( Request :: _, From :: {pid(), _},
                       NetState :: #net_state{} ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.
```

The `handle_call/3` function performs a synchronous exchange of messages between the caller and the net instance. The first argument is the request message, the second argument is a tuple identifying the caller, and the third argument is a `#net_state{}` record instance describing the current state of the net. The `handle_call/3` function can either generate a reply without changing the net marking by returning a `{reply, Reply}` tuple or it can generate a reply, consuming or producing tokens by returning a `{reply, Reply, ConsumeMap, ProduceMap}` tuple.

```erlang
handle_call( insert_coin, _From, _NetState ) ->
  {reply, ok, #{}, #{ coin_slot => [coin] }};

handle_call( remove_cookie_box, _From, NetState ) ->

  case gen_pnet:get_ls( compartment, NetState ) of
    []    -> {reply, {error, empty_compartment}};
    [_|_] -> {reply, ok, #{ compartment => [cookie_box] }, #{}}
  end;

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.
```

Here, we react to two kinds of messages: Inserting a coin in the coin slot and removing a cookie box from the compartment. Thus, we react to an `insert_coin` message by replying with `ok`, consuming nothing and producing a `coin` token on the `coin_slot` place. When receiving a `remove_cookie_box` message, we check whether the `compartment` place is empty, replying with an error message if it is, otherwise replying with `ok`, consuming one `cookie_box` token from the `compartment` place, and producing nothing. We can inspect the tokens on a given place by using the `get_ls/2` accessor function. Calls that are neither `insert_coin` nor `remove_cookie_box` are responded to with an error message.

#### handle_cast/2

```erlang
-callback handle_cast( Request :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.
```

The `handle_cast/2` function reacts to an asynchronous message received by the net instance. The first argument is the request while the second argument is a `#net_state{}` record instance. The `handle_cast/2` function can either leave the net unchanged by returning `noreply` or it can consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}` tuple.

```erlang
handle_cast( _Request, _NetState ) -> noreply.
```

Here, we just ignore any cast.

#### handle_info/2

```erlang
-callback handle_info( Info :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.
```

The `handle_info/2` function reacts to an asynchronous, unformatted message received by the net instance. The first argument is the message term while the second argument is a `#net_state{}` record instance. The `handle_info/2` function can either leave the net unchanged by returning `noreply` or it can consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}` tuple.

```erlang
handle_info( _Request, _NetState ) -> noreply.
```

Here, we just ignore any message.

#### init/1

```erlang
-callback init( Args :: _ ) -> UsrInfo :: _.
```

The `init/1` function initializes the net instance. It is given an initial argument which is provided with `gen_pnet:start_link/n`. The `init/1` function is expected to return a user info field which is later handed to other callback functions.

```erlang
init( _Args ) -> [].
```

Here, we return the empty list as a dummy user info field.

#### terminate/2

```erlang
-callback terminate( Reason :: _, NetState :: #net_state{} ) -> ok.
```

The `terminate/2` function determines what happens when the net instance is stopped. The first argument is the reason for termination while the second argument is a `#net_state{}` record instance. This callback is identical to the `terminate/2` function in the gen_server behavior.

```erlang
terminate( _Reason, _NetState ) -> ok.
```

#### trigger/3

```erlang
-callback trigger( Place :: atom(), Token :: _, NetState :: #net_state{} ) ->
            pass | drop.
```

The `trigger/3` function determines what happens when a token is produced on a given place. Its first argument is the place name, its second argument is the token about to be produced, and its third argument is the user info field generated by `init/1`. The `trigger/3` function is expected to return either `pass` in which case the token is produced normally, or `drop` in which case the token is forgotten.

```erlang
trigger( _Place, _Token, _UsrInfo ) -> pass.
```

Here, we simply let any token pass.

### Starting and Querying the Cookie Vending Machine Example

In the following we demonstrate how to start and play with the previously defined cookie vending machine example. You can either copy the above code in an Erlang callback module of your own or you can obtain the module from the [joergen7/gen_pnet_examples](https://github.com/joergen7/gen_pnet_examples) repository. Here, we clone it from GitHub and compile it. Then we start an interactive Erlang shell using [rebar3](https://github.com/erlang/rebar3).

    git clone https://github.com/joergen7/gen_pnet_examples.git
    cd gen_pnet_examples
    rebar3 shell

Compiling with rebar3 also fetches the gen_pnet library. We start the cookie vending machine which is stored in the callback module `src/cmv.erl` by using `gen_pnet:start_link/3`.

    {ok, Pid} = gen_pnet:start_link( cvm, [], [] ).
    {ok, <0.115.0>}

 The first argument is the callback module defining the cookie vending machine. It must implement all callback functions in the gen_pnet behavior. The second argument is an option list, identical to the one used in the `gen_server:start_link/n` functions. On success, `gen_pnet:start_link/3` returns the process id of the just created Petri net process. Now that the Petri net is running we can query the content of its places with `gen_pnet:ls/2`. This Petri net has five places: `coin_slot`, `cash_box`, `signal`, `compartment`, and `storage`. Initially, all places are empty except the `storage` place which holds three cookie packages.

    gen_pnet:ls( Pid, storage ).
    {ok,[cookie_box,cookie_box,cookie_box]}

    gen_pnet:ls( Pid, compartment ).
    {ok, []}

    gen_pnet:ls( Pid, some_place_that_does_not_exist ).
    {error,{bad_place,some_place_that_does_not_exist}}

To interact with the cookie vending machine we insert a coin by adding an according token to the `coin_slot` place. This can be done with the `gen_pnet:produce_token/3` function which takes a reference to a Petri net instance, a place name, and a token which is added to that place.

    gen_pnet:call( Pid, insert_coin ).
    ok

The effect of calling the net instance with `insert_coin` is that a coin is produced on the `coin_slot` place. Immediately, transition `a` fires making the coin wander to the `cash_box` place, leaving the `coin_slot` place empty again. Immediately afterwards, the transition `b` fires, making a `cookie_box` token appear in the formerly empty `compartment` place while the number of `cookie_box` tokens on the `storage` has reduced by one. We can check this by querying the places as previously.

    gen_pnet:ls( Pid, cash_box ). 
    {ok,[coin]}

    gen_pnet:ls( Pid, storage ).
    {ok,[cookie_box,cookie_box]}

    gen_pnet:ls( Pid, compartment ).
    {ok,[cookie_box]}

Now, we can remove the cookie box from the compartment by calling the net instance with `remove_cookie_box`.

    gen_pnet:call( Pid, remove_cookie_box ).
    ok

Calling with `remove_cookie_box` a second time will yield an error, since only one cookie box was bought.

    gen_pnet:call( Pid, remove_cookie_box ).
    {error,empty_compartment}

## System Requirements

- Erlang OTP 18.0 or higher
- Rebar3 3.0.0 or higher

## Resources

- [aabs/gen_pn](https://github.com/aabs/gen_pn). An alternative Erlang/OTP compatible Petri net library.
- [joergen7/gen_pnet_examples](https://github.com/joergen7/gen_pnet_examples). A collection of examples using gen_pnet.
- [joergen7/gruff](https://github.com/joergen7/gruff). A basic worker pool manager for Erlang to showcase gen_pnet.
- [joergen7/cre](https://github.com/joergen7/cre). A common runtime environment for distributed workflow languages.

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)


%% -*- erlang -*-
%%
%% A generic Petri net OTP behavior.
%%
%% Copyright 2016 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.7
%% @copyright 2016 Jörgen Brandt
%%
%% @doc Callback function definitions and API for the `gen_pnet' behavior.
%%
%% <h3>Net Structure Callback Functions</h3>
%%
%% There are six callbacks that define the Petri net structure and its initial
%% marking:
%%
%% <ul>
%%   <li>`place_lst/0' returns the names of the places in the net</li>
%%   <li>`trsn_lst/0' returns the names of the transitions in the net</li>
%%   <li>`init_marking/2' returns the initial marking for a given place</li>
%%   <li>`preset/1' returns the preset places of a given transition</li>
%%   <li>`is_enabled/3' determines whether a given transition is enabled in a
%%       given mode</li>
%%   <li>`fire/3' returns which tokens are produced on what places if a given
%%       transition is fired in a given mode that enables this transition</li>
%% </ul>
%%
%% We have a look at each of them in turn.
%%
%% <h4>place_lst/0</h4>
%%
%% The `place_lst/0' function lets us define the names of all places in the net.
%%
%% Example:
%% ```
%% place_lst() ->
%%   [coin_slot, cash_box, signal, storage, compartment].
%% '''
%% Here, we define the net to have the five places in the cookie vending
%% machine.
%%
%% <h4>trsn_lst/0</h4>
%%
%% The `trsn_lst/0' function lets us define the names of all transitions in the
%% net.
%%
%% Example:
%% ```
%% trsn_lst() ->
%%   [a, b].
%% '''
%% Here, we define the net to have the two places `a' and `b' in the cookie
%% vending machine.
%%
%% <h4>preset/1</h4>
%%
%% The `preset/1' lets us define the preset places of a given transition.
%%
%% Example:
%% ```
%% preset( a ) -> [coin_slot];
%% preset( b ) -> [signal, storage].
%% '''
%% Here, we define the preset of the transition `a' to be just the place
%% `coin_slot' while the transition `b' has the places `signal' and `storage'
%% in its preset.
%%
%% <h4>init_marking/2</h4>
%%
%% The `init_marking/2' function lets us define the initial marking for a given
%% place in the form of a token list. The argument `UsrInfo' is the user info
%% field that has been generated in the actor interface callback `init/1'.
%%
%% Example:
%% ```
%% init_marking( storage, _UsrInfo ) -> [cookie_box, cookie_box, cookie_box];
%% init_marking( _Place, _UsrInfo )  -> [].
%% '''
%% Here, we initialize the storage place with three `cookie_box' tokens. All
%% other places are left empty.
%%
%% <h4>is_enabled/3</h4>
%%
%% The `is_enabled/3' function is a predicate determining whether a given
%% transition is enabled in a given mode. The `UsrInfo' argument is the user
%% info field that has been created with `init/1'.
%%
%% Example:
%% ```
%% is_enabled( a, #{ coin_slot := [coin] }, _UsrInfo )                      -> true;
%% is_enabled( b, #{ signal := [sig], storage := [cookie_box] }, _UsrInfo ) -> true;
%% is_enabled( _Trsn, _Mode, _UsrInfo )                                     -> false.
%% '''
%% Here, we state that the transition `a' is enabled if it can consume a single
%% `coin' from the `coin_slot' place. Similarly, the transition `b' is enabled
%% if it can consume a `sig' token from the `signal' place and a `cookie_box'
%% token from the `storage` place. No other configuration can enable a
%% transition. E.g., managing to get a `button' token on the `coin_slot' place
%% will not enable any transition.
%%
%% <h4>fire/3</h4>
%%
%% The `fire/3' function defines what tokens are produced when a given
%% transition fires in a given mode. As arguments it takes the name of the
%% transition, and a firing mode in the form of a hash map mapping place names
%% to token lists. The `fire/3' function is called only on modes for which
%% `is_enabled/3' returns `true'. The `fire/3' function is expected to return
%% either a `{produce, ProduceMap}' tuple or the term `abort'. If `abort' is
%% returned, the firing is aborted. Nothing is produced or consumed.
%%
%% Example:
%% ```
%% fire( a, _Mode, _UsrInfo ) ->
%%   {produce, #{ cash_box => [coin], signal => [sig] }};
%% fire( b, _Mode, _UsrInfo ) ->
%%   {produce, #{ compartment => [cookie_box] }}.
%% '''
%% Here, the firing of the transition `a' produces a `coin' token on the
%% `cash_box' place and a `sig' token on the `signal' place. Similarly, the
%% firing of the transition `b' produces a `cookie_box' token on the
%% `compartment' place. We do not need to state the tokens to be consumed
%% because the firing mode already uniquely identifies the tokens to be
%% consumed.
%%
%%
%% <h3>Interface Callback Functions</h3>
%%
%% In addition to the structure callback functions there are another seven
%% callback functions that determine how the net instance appears as an Erlang
%% actor to the outside world:
%%
%% <ul>
%%   <li>`code_change/3' determines what happens when a hot code reload
%%       appears</li>
%%   <li>`handle_call/3' synchronous message exchange</li>
%%   <li>`handle_cast/2' asynchronous message reception</li>
%%   <li>`handle_info/2' asynchronous reception of an unformatted message</li>
%%   <li>`init/1' initializes the gen_pnet instance</li>
%%   <li>`terminate/2' determines what happens when the net instance is
%%       stopped</li>
%%   <li>`trigger/3' allows to add a side effects to the generation of a
%%       token</li>
%% </ul>
%%
%% <h4>code_change/3</h4>
%%
%% The `code_change/3' function determines what happens when a hot code reload
%% appears. This callback is identical to the `code_change/3' function in the
%% `gen_server' behavior.
%%
%% Example:
%% ```
%% code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.
%% '''
%%
%% <h4>handle_call/3</h4>
%%
%% The `handle_call/3' function performs a synchronous exchange of messages
%% between the caller and the net instance. The first argument is the request
%% message, the second argument is a tuple identifying the caller, and the third
%% argument is a `#net_state{}' record instance describing the current state of
%% the net. The `handle_call/3' function can generate a reply without changing
%% the net marking by returning a `{reply, Reply}' tuple, it can generate a
%% reply, producing tokens by returning a `{reply, Reply, ProduceMap}' tuple, it
%% can defer replying without changing the net marking by returning `noreply',
%% it can defer replying, producing tokens by returning a
%% `{noreply, ProduceMap}' tuple, or it can stop the net instance by returning
%% `{stop, Reason, Reply}'.
%%
%% Example:
%% ```
%% handle_call( insert_coin, _From, _NetState ) ->
%%   {reply, ok, #{ coin_slot => [coin] }};
%%
%% handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.
%% '''
%% Here, we react to two kinds of messages: Inserting a coin in the coin slot
%% and unrecognized messages. Thus, we react to an `insert_coin' message by
%% replying with `ok', consuming nothing and producing a `coin' token on the
%% `coin_slot' place. Calls that are not `insert_coin' are responded to with
%% an error message.
%%
%% <h4>handle_cast/2</h4>
%%
%% The `handle_cast/2' function reacts to an asynchronous message received by
%% the net instance. The first argument is the request while the second argument
%% is a `#net_state{}' record instance. The `handle_cast/2' function can either
%% leave the net unchanged by returning `noreply' or it can produce tokens by
%% returning a `{noreply, ProduceMap}' tuple.
%%
%% Example:
%% ```
%% handle_cast( _Request, _NetState ) -> noreply.
%% '''
%% Here, we just ignore any cast.
%%
%% <h4>handle_info/2</h4>
%%
%% The `handle_info/2' function reacts to an asynchronous, unformatted message
%% received by the net instance. The first argument is the message term while
%% the second argument is a `#net_state{}' record instance. The `handle_info/2'
%% function can either leave the net unchanged by returning `noreply' or it can
%% produce tokens by returning a `{noreply, ProduceMap}' tuple.
%%
%% Example:
%% ```
%% handle_info( _Request, _NetState ) -> noreply.
%% '''
%% Here, we just ignore any message.
%%
%% <h4>init/1</h4>
%%
%% The `init/1' function initializes the net instance. It is given an initial
%% argument which is provided with `gen_pnet:start_link/n'. The `init/1'
%% function is expected to return a user info field which is later handed to
%% other callback functions.
%%
%% Example:
%% ```
%% init( _NetArg ) -> [].
%% '''
%% Here, we return the empty list as a dummy user info field.
%%
%% <h4>terminate/2</h4>
%%
%% The `terminate/2' function determines what happens when the net instance is
%% stopped. The first argument is the reason for termination while the second
%% argument is a `#net_state{}' record instance. This callback is identical to
%% the `terminate/2' function in the `gen_server' behavior.
%%
%% Example:
%% ```
%% terminate( _Reason, _NetState ) -> ok.
%% '''
%%
%% <h4>trigger/3</h4>
%%
%% The `trigger/3' function determines what happens when a token is produced on
%% a given place. Its first argument `Place' is the place name, its second
%% argument `Token' is the token about to be produced, and its third argument
%% `NetState' is the current state of the net. The `trigger/3' function is
%% expected to return either `pass' in which case the token is produced
%% normally, or `drop' in which case the token is forgotten.
%%
%% Example:
%% ```
%% trigger( _Place, _Token, _NetState ) -> pass.
%% '''
%% Here, we simply let any token pass.
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_pnet).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

% API functions
-export([start_link/3, start_link/4,
         ls/2,
         marking/1,
         call/2, call/3,
         cast/2,
         stats/1,
         reply/2,
         reset_stats/1,
         stop/1,
         usr_info/1,
         state_property/3]).

% Net state constructor and accessor functions
-export([get_ls/2, get_usr_info/1, get_stats/1]).

% gen_server callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_pnet.hrl").

%%====================================================================
%% Type definitions
%%====================================================================

-type name() :: atom() |
                {atom(), atom()} |
                {global, _} |
                {via, atom(), _} |
                pid().

-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

-type start_link_result() :: {ok, pid()} |
                             ignore |
                             {error, _}.

-type handle_call_request() :: {ls, atom()} |
                               marking |
                               usr_info |
                               {call, _} |
                               stats |
                               reset_stats.

-type handle_call_result() :: {reply, _, #net_state{}} |
                              {noreply, #net_state{}} |
                              {stop, _, _, #net_state{}}.

-type handle_cast_request() :: continue |
                               {cast, _}.

-type handle_cast_result() :: {noreply, #net_state{}} |
                              {stop, _, #net_state{}}.

-type handle_info_result() :: {noreply, #net_state{}} |
                              {stop, _, #net_state{}}.

-type prop() :: {debug, [log | statistics | trace | {_, _}]} |
                {hibernate_after, infinity | non_neg_integer()} |
                {spawn_opt, [link | monitor | {_, _}]} |
                {timeout, infinity | non_neg_integer()}.

%%====================================================================
%% Callback definitions
%%====================================================================

%% Structure callbacks


-callback place_lst() -> [atom()].

-callback trsn_lst() -> [atom()].

-callback init_marking(Place :: atom(), UsrInfo :: _) -> [_].

-callback preset(Trsn :: atom()) -> [atom()].

-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              boolean().

-callback fire(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              abort | {produce, #{atom() => [_]}}.

-callback trigger(Place :: atom(), Token :: _, NetState :: #net_state{}) ->
              pass | drop.

%% Interface callbacks

-callback code_change(OldVsn :: _, NetState :: #net_state{}, Extra :: _) ->
              {ok, #net_state{}} | {error, _}.

-callback handle_call(Request :: _,
                      From :: {pid(), _},
                      NetState :: #net_state{}) ->
              {reply, _} |
              {reply, _, #{atom() => [_]}} |
              noreply |
              {noreply, #{atom() => [_]}} |
              {stop, _, _}.

-callback handle_cast(Request :: _, NetState :: #net_state{}) ->
              noreply |
              {noreply, #{atom() => [_]}} |
              {stop, _}.

-callback handle_info(Info :: _, NetState :: #net_state{}) ->
              noreply |
              {noreply, #{atom() => [_]}} |
              {stop, _}.

-callback init(NetArg :: _) -> _.

-callback terminate(Reason :: _, NetState :: #net_state{}) -> ok.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an unregistered net instance.
%% @see start_link/4
-spec start_link(NetMod, NetArg, Options) -> start_link_result()
              when NetMod :: atom(),
                   NetArg :: _,
                   Options :: [prop()].

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).


%% @doc Starts a net instance registered as `ServerName' using the callback
%%      module `NetMod' as the callback module for this net instance.
%%
%%      The `InitArg' argument is later handed to the `init/1' callback. The
%%      `ServerName' argument can be
%%      `{local, Name} | {global, Name} | {via, Module, ViaName}'. Internally,
%%      the server name `ServerName' and option list `Options' are handed down
%%      to `gen_server:start_link/4' as is.
%%
%% @see init/1


-spec start_link(ServerName, NetMod, InitArg, Options) -> start_link_result()
              when ServerName :: server_name(),
                   NetMod :: atom(),
                   InitArg :: _,
                   Options :: [prop()].

start_link(ServerName, NetMod, InitArg, Options)
  when is_tuple(ServerName), is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(ServerName, ?MODULE, {NetMod, InitArg}, Options).


%% @doc Query the list of tokens on the place named `Place' in the net instance
%%      identified as `Name'.
%%
%%      Herein, `Name' can be a process id or a registered process name. The
%%      return value is either `{ok, [_]}' if the place exists or a
%%      `{error, #bad_place{}}' tuple.


-spec ls(Name, Place) -> {ok, [_]} | {error, #bad_place{}}
              when Name :: name(),
                   Place :: atom().

ls(Name, Place) when is_atom(Place) -> gen_server:call(Name, {ls, Place}).


%% @doc Query the marking map of the net instance identified as `Name'
%%      associating to each place name the list of tokens that this place holds.
%%
%%      Herein, `Name' can be a process id or a registered process name. The
%%      return value is the Petri net's marking map.


-spec marking(Name :: name()) -> #{atom() => [_]}.

marking(Name) -> gen_server:call(Name, marking).


%% @doc Query the user info term from the net instance identified as `Name'.


-spec usr_info(Name :: name()) -> _.

usr_info(Name) -> gen_server:call(Name, usr_info).


%% @doc Query the statistics gathered by the net instance identified as `Name'.
%%
%%      The throughput is given as a `#stats{}' record consisting of three
%%      `#stat{}' record instances characterizing the current, maximum, and
%%      minimum throughput of this net in transition firings per second.


-spec stats(Name :: name()) -> #stats{}.

stats(Name) -> gen_server:call(Name, stats).


%% @doc Requests the net instance identified as `Name' to clear its stats.


-spec reset_stats(Name :: name()) -> ok.

reset_stats(Name) -> gen_server:call(Name, reset_stats).


%% @doc Requests the net instance identified as `Name' to stop.


-spec stop(Name :: name()) -> ok.

stop(Name) -> gen_server:stop(Name).


%% @doc Synchronously send the term `Request' to the net instance identified as
%%      `Name' and return the reply.

%%      The timeout is implicitly set to five seconds.
%%
%% @see call/3


-spec call(Name :: name(), Request :: _) -> _.

call(Name, Request) -> gen_server:call(Name, {call, Request}).


%% @doc Synchronously send the term `Request' to the net instance identified as
%%      `Name' and return the reply.
%%
%%      The timeout is explicitly set to `Timeout'. The request is handled by
%%      the `handle_call/3' callback function of the interface module. Herein
%%      `Timeout' must be a non-negative integer or the atom `infinity'.


-spec call(Name, Request, Timeout) -> _
              when Name :: name(),
                   Request :: _,
                   Timeout :: non_neg_integer() | infinity.

call(Name, Request, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    gen_server:call(Name, {call, Request}, Timeout);

call(Name, Request, infinity) ->
    gen_server:call(Name, {call, Request}, infinity).


%% @doc Asynchronously send the term `Request' to the net instance identified as
%%      `Name'.
%%
%%      The request is handled by the `handle_cast/2' callback function of the
%%      interface module. Note that the cast succeeds even if a non-existing
%%      process is addressed or the net instance is down.


-spec cast(Name :: name(), Request :: _) -> ok.

cast(Name, Request) ->
    gen_server:cast(Name, {cast, Request}).


%% @doc Sends a reply to a calling client process.
%%
%%      This function is to be used when the reply to a caller has been
%%      deferred by returning `{noreply, _, _}' in `handle_call/3'.
%%
%% @see handle_call/3


-spec reply(Client, Reply) -> Result
              when Client :: {pid(), gen_server:reply_tag()},
                   Reply :: _,
                   Result :: ok.

reply(Client, Reply) when is_tuple(Client) ->
    gen_server:reply(Client, Reply).


%% @doc Checks if a predicate about the state of the net holds.
%%
%%      The function takes a Petri net instance identified as `Name' and asks it
%%      to verify the predicate `Pred' over its marking. Herein, `Pred' is a
%%      function that takes n token lists, where each of the token lists subsume
%%      the tokens present on the places identified by the `PlaceLst' argument.
%%      The predicate is expected to return either `ok' or `{error, Reason}'
%%      where Reason can be any Erlang term.


-spec state_property(Name, Pred, PlaceLst) -> ok | {error, Reason}
              when Name :: name(),
                   Pred :: fun((...) -> ok | {error, Reason}),
                   PlaceLst :: [atom()].

state_property(Name, Pred, PlaceLst)
  when is_list(PlaceLst),
       is_function(Pred, length(PlaceLst)) ->

    Marking = gen_pnet:marking(Name),
    ArgLst = [ maps:get(Place, Marking) || Place <- PlaceLst ],
    apply(Pred, ArgLst).


%%====================================================================
%% Net state constructor and accessor functions
%%====================================================================


%% @doc Extracts the list of tokens on a given place from a given net state.
%%
%%      Throws an error if the list does not exist.
-spec get_ls(Place :: atom(), NetState :: #net_state{}) -> [_].

get_ls(Place, #net_state{marking = Marking}) -> maps:get(Place, Marking).


%% @doc Extracts the user info field from a given net state.
-spec get_usr_info(NetState :: #net_state{}) -> _.

get_usr_info(#net_state{usr_info = UsrInfo}) -> UsrInfo.


%% @doc Extracts the stats field from a given net instance.
-spec get_stats(NetState :: #net_state{}) -> #stats{}.

get_stats(#net_state{stats = Stats}) -> Stats.


%%====================================================================
%% Generic server callback functions
%%====================================================================


%% @private
-spec code_change(OldVsn, NetState, Extra) -> {ok, #net_state{}} | {error, _}
              when OldVsn :: _,
                   NetState :: #net_state{},
                   Extra :: _.

code_change(OldVsn, NetState = #net_state{net_mod = NetMod}, Extra) ->
    NetMod:code_change(OldVsn, NetState, Extra).


%% @private
-spec handle_call(Request, From, NetState) -> handle_call_result()
              when Request :: handle_call_request(),
                   From :: {pid(), _},
                   NetState :: #net_state{}.

handle_call({ls, Place}, _From, NetState = #net_state{marking = Marking}) ->

    Reply = case maps:is_key(Place, Marking) of
                true -> {ok, maps:get(Place, Marking)};
                false -> {error, #bad_place{name = Place}}
            end,

    {reply, Reply, NetState};

handle_call(marking, _From, NetState = #net_state{marking = Marking}) ->
    {reply, Marking, NetState};

handle_call(usr_info, _From, NetState = #net_state{usr_info = UsrInfo}) ->
    {reply, UsrInfo, NetState};

handle_call({call, Request},
            From,
            NetState = #net_state{net_mod = NetMod}) ->

    case NetMod:handle_call(Request, From, NetState) of

        {reply, Reply} ->
            {reply, Reply, NetState};

        {reply, Reply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {reply, Reply, NetState1};

        noreply ->
            {noreply, NetState};

        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {noreply, NetState1};

        {stop, Reason, Reply} ->
            {stop, Reason, Reply, NetState}

    end;

handle_call(stats, _From, NetState = #net_state{stats = Stats}) ->
    {reply, Stats, NetState};

handle_call(reset_stats, _From, NetState) ->
    {reply, ok, NetState#net_state{stats = undefined}}.


%% @private
-spec handle_cast(Request, NetState) -> handle_cast_result()
              when Request :: handle_cast_request(),
                   NetState :: #net_state{}.

handle_cast(continue,
            NetState = #net_state{
                         stats = Stats,
                         tstart = T1,
                         cnt = Cnt
                        }) ->

    case progress(NetState) of

        abort ->
            {noreply, NetState};

        {delta, Mode, Pm} ->

            NetState1 = cns(Mode, NetState),
            NetState2 = handle_trigger(Pm, NetState1),
            continue(self()),

            NetState3 = if
                            Cnt < 1000 -> NetState2#net_state{cnt = Cnt + 1};
                            true ->

                                T2 = os:system_time(),
                                Tmean = round((T1 + T2) / 2),
                                Tdelta = T2 - T1,
                                CurrentFps = 1000000000000 / Tdelta,

                                Current = #stat{t = Tmean, fps = CurrentFps},

                                {Hi1, Lo1} = case Stats of
                                                 undefined -> {Current, Current};
                                                 #stats{hi = H, lo = L} -> {H, L}
                                             end,

                                #stat{fps = HiFps} = Hi1,
                                #stat{fps = LoFps} = Lo1,

                                Hi2 = if
                                          CurrentFps > HiFps -> Current;
                                          true -> Hi1
                                      end,

                                Lo2 = if
                                          CurrentFps < LoFps -> Current;
                                          true -> Lo1
                                      end,

                                NetState2#net_state{
                                  stats = #stats{
                                            current = Current,
                                            hi = Hi2,
                                            lo = Lo2
                                           },
                                  tstart = T2,
                                  cnt = 0
                                 }
                        end,

            {noreply, NetState3}

    end;

handle_cast({cast, Request}, NetState = #net_state{net_mod = NetMod}) ->

    case NetMod:handle_cast(Request, NetState) of

        noreply ->
            {noreply, NetState};

        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {noreply, NetState1};

        {stop, Reason} ->
            {stop, Reason, NetState}

    end.


%% @private
-spec handle_info(Info, NetState) -> handle_info_result()
              when Info :: _,
                   NetState :: #net_state{}.

handle_info(Info, NetState = #net_state{net_mod = NetMod}) ->

    case NetMod:handle_info(Info, NetState) of

        noreply ->
            {noreply, NetState};

        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {noreply, NetState1};

        {stop, Reason} ->
            {stop, Reason, NetState}

    end.


%% @private
-spec init(ArgPair :: {atom(), _}) -> {ok, #net_state{}}.

init({NetMod, NetArg}) ->

    UsrInfo = NetMod:init(NetArg),

    PlaceLst = NetMod:place_lst(),

    F = fun(P, Acc) ->
                Acc#{P => NetMod:init_marking(P, UsrInfo)}
        end,

    InitMarking = lists:foldl(F, #{}, PlaceLst),

    continue(self()),

    {ok, #net_state{
           net_mod = NetMod,
           usr_info = UsrInfo,
           marking = InitMarking,
           stats = undefined,
           tstart = os:system_time(),
           cnt = 0
          }}.


%% @private
-spec terminate(Reason :: _, NetState :: #net_state{}) -> ok.

terminate(Reason, NetState = #net_state{net_mod = NetMod}) ->
    NetMod:terminate(Reason, NetState).


%%====================================================================
%% Internal functions
%%====================================================================


%% @doc Continue making progress in net instance under process id `Name'.
%%
%%      Note that continuing succeeds even if a non-existing process is
%%      addressed or the net instance is down.
-spec continue(Name :: pid()) -> ok.

continue(Name) ->
    gen_server:cast(Name, continue).


-spec handle_trigger(ProdMap, NetState) -> #net_state{}
              when ProdMap :: #{atom() => [_]},
                   NetState :: #net_state{}.

handle_trigger(ProdMap, NetState = #net_state{net_mod = NetMod}) ->

    G = fun(P, TkLst, Acc) ->

                F = fun(Tk, A) ->
                            case NetMod:trigger(P, Tk, NetState) of
                                pass -> [Tk | A];
                                drop -> A
                            end
                    end,

                TkLst1 = lists:foldl(F, [], TkLst),
                Acc#{P => TkLst1}

        end,

    ProdMap1 = maps:fold(G, #{}, ProdMap),
    prd(ProdMap1, NetState).


-spec cns(Mode, NetState) -> #net_state{}
              when Mode :: #{atom() => [_]},
                   NetState :: #net_state{}.

cns(Mode, NetState = #net_state{marking = Marking}) ->

    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst -- maps:get(T, Mode, [])}
        end,

    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.


-spec prd(ProdMap, NetState) -> #net_state{}
              when ProdMap :: #{atom() => [_]},
                   NetState :: #net_state{}.

prd(ProdMap, NetState = #net_state{marking = Marking}) ->

    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst ++ maps:get(T, ProdMap, [])}
        end,

    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.


-spec progress(NetState :: #net_state{}) ->
          abort | {delta, #{atom() => [_]}, #{atom() => [_]}}.

progress(#net_state{
           marking = Marking,
           net_mod = NetMod,
           usr_info = UsrInfo
          }) ->

    % get all transitions in the net
    TrsnLst = NetMod:trsn_lst(),

    F = fun(T, Acc) ->
                Preset = NetMod:preset(T),
                MLst = enum_mode(Preset, Marking),
                IsEnabled = fun(M) -> NetMod:is_enabled(T, M, UsrInfo) end,
                EnabledMLst = lists:filter(IsEnabled, MLst),
                case EnabledMLst of
                    [] -> Acc;
                    [_ | _] -> Acc#{T => EnabledMLst}
                end
        end,

    % derive a map listing all enabled modes for each transition
    ModeMap = lists:foldl(F, #{}, TrsnLst),

    % delegate enabled mode map to attempt_progress function
    attempt_progress(ModeMap, NetMod, UsrInfo).


-spec attempt_progress(ModeMap, NetMod, UsrInfo) -> abort | {delta, _, _}
              when ModeMap :: #{atom() => [_]},
                   NetMod :: atom(),
                   UsrInfo :: _.

attempt_progress(ModeMap, NetMod, UsrInfo) ->

    case maps:size(ModeMap) of

        0 -> abort;
        _ ->

            TrsnLst = maps:keys(ModeMap),
            Trsn = lib_combin:pick_from(TrsnLst),
            #{Trsn := ModeLst} = ModeMap,
            Mode = lib_combin:pick_from(ModeLst),

            case NetMod:fire(Trsn, Mode, UsrInfo) of

                {produce, ProdMap} ->
                    {delta, Mode, ProdMap};

                abort ->
                    ModeLst1 = ModeLst -- [Mode],
                    case ModeLst1 of
                        [] ->
                            attempt_progress(maps:remove(Trsn, ModeMap), NetMod, UsrInfo);
                        [_ | _] ->
                            attempt_progress(ModeMap#{Trsn := ModeLst1}, NetMod, UsrInfo)
                    end

            end
    end.


-spec enum_mode(Preset, Marking) -> [#{atom() => [_]}]
              when Preset :: [atom()],
                   Marking :: #{atom() => [_]}.

enum_mode(Preset, Marking) ->

    F = fun(P, Acc) ->
                N = maps:get(P, Acc, 0),
                Acc#{P => N + 1}
        end,

    % gather count map
    CountMap = lists:foldl(F, #{}, Preset),

    G = fun(P, N, Acc) ->
                #{P := TkLst} = Marking,
                Acc#{P => lib_combin:cnr(N, TkLst)}
        end,

    % enumerate drawing combinations for each preset place individually
    CmbMap = maps:fold(G, #{}, CountMap),

    % enumerate permutations of map containing drawing combinations
    lib_combin:permut_map(CmbMap).


# Plan: Complete YAWL Refactoring to gen_pnet

## Executive Summary

**Objective**: Completely refactor the YAWL workflow system to use the actual `gen_pnet` OTP behavior instead of the current mock/custom Petri net implementation.

**Current State**: The system mocks gen_pnet callbacks but implements its own Petri net engine. The actual `gen_pnet` library is a dependency but not fully utilized.

**Target State**: All YAWL patterns and workflow execution use genuine `gen_pnet` behavior with proper OTP integration.

---

## Current Architecture Analysis

### Current Implementation

| Component | Current Approach | Target |
|-----------|-----------------|--------|
| `yawl_patterns.erl` | Mock gen_pnet callbacks, custom execution | Real gen_pnet behavior module |
| `yawl_workflow_instance.erl` | gen_statem with custom token logic | gen_pnet-based workflow instance |
| `yawl_orchestrator.erl` | Manages custom workflow instances | Manages gen_pnet workflow instances |
| Token passing | Custom implementation | gen_pnet native token passing |

### Files That Change

**Primary Refactoring Targets:**
1. `src/yawl_patterns.erl` - Convert to proper gen_pnet callback module
2. `src/yawl_workflow_instance.erl` - Replace gen_statem with gen_pnet
3. `src/yawl_orchestrator.erl` - Update for gen_pnet workflow management
4. `src/yawl_workitem_processor.erl` - Integrate with gen_pnet triggers

**Supporting Changes:**
5. `include/yawl_types.hrl` - Update records for gen_pnet compatibility
6. `src/yawl_cpn.erl` - Colored Petri net extensions for gen_pnet
7. `src/yawl_persistence.erl` - Persist gen_pnet net_state
8. `src/a2a_erl_sup.erl` - Update supervision tree

---

## Implementation Plan

### Phase 1: Core gen_pnet Integration (Foundation)

**Goal**: Create a working gen_pnet-based YAWL pattern implementation

**Actions:**

1. **Create `src/yawl_pnet.erl`** - gen_pnet behavior wrapper for YAWL
   ```erlang
   -module(yawl_pnet).
   -behaviour(gen_pnet).

   % Net structure callbacks
   -export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
            is_enabled/3, fire/3, trigger/3]).

   % OTP callbacks
   -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).
   ```

2. **Implement basic pattern to Petri net mapping**
   - Each YAWL pattern becomes a gen_pnet module
   - Define places, transitions, preset for each pattern
   - Map YAWL tokens to Petri net tokens

3. **Create pattern modules** (43 total):
   ```
   src/yawl_patterns/
   ├── basic_sequential.erl
   ├── parallel_split.erl
   ├── parallel_join.erl
   ├── exclusive_choice.erl
   ├── simple_merge.erl
   ├── iterative_loop.erl
   ├── multi_instance.erl
   ... (43 pattern modules)
   ```

**Deliverable**: `yawl_pnet.erl` with basic sequential pattern working

---

### Phase 2: Pattern Migration

**Goal**: Migrate all 43 YAWL patterns to gen_pnet

**Pattern Categories:**

1. **Basic Control Flow (5 patterns)**
   - basic_sequential, parallel_split, parallel_join
   - exclusive_choice, simple_merge

2. **Advanced Control Flow (8 patterns)**
   - implicit_merge, multiple_merge, deferred_choice
   - interleaved_routing, milestone, discriminator
   - n_out_of_m, arbitrary_cycle

3. **Iteration Patterns (6 patterns)**
   - iterative_loop, structured_loop, recursion
   - multi_instance, sequential_multi_instance, parallel_multi_instance

4. **Cancellation Patterns (17 patterns)**
   - cancelation_block, cancelation_scope
   - cancelation_thread, cancelation_subprocess
   - cancelation_multiple_instances, cancelation_point
   - cancelation_end, cancelation_cancel
   - cancelation_thread_after, cancelation_subprocess_after
   - cancelation_multiple_instances_after
   - cancelation_thread_or, cancelation_subprocess_or
   - cancelation_multiple_instances_or
   - cancelation_thread_and, cancelation_subprocess_and
   - cancelation_multiple_instances_and

5. **Other Patterns (7 patterns)**
   - critical_section, multiple_instances_with_prior_knowledge
   - static_partial_join, dynamic_partial_join
   - blocking_pattern, forced_execution, strict_sequence

**Actions:**
- Create individual gen_pnet modules for each pattern
- Implement place_lst/0, trsn_lst/0, preset/1, is_enabled/3, fire/3
- Add pattern-specific triggers for side effects

**Deliverable**: All 43 patterns as gen_pnet modules

---

### Phase 3: Workflow Instance Refactoring

**Goal**: Replace custom workflow instance with gen_pnet-based instance

**Current**: `yawl_workflow_instance.erl` (gen_statem)
**Target**: `yawl_workflow_instance_pnet.erl` (gen_pnet)

**Actions:**

1. **Create `src/yawl_workflow_instance_pnet.erl`**
   - Implements gen_pnet behavior
   - Places represent workflow states
   - Transitions represent state changes
   - Tokens represent work items

2. **Define workflow net structure:**
   ```erlang
   place_lst() ->
       [idle, starting, running, waiting, completing, terminated].

   trsn_lst() ->
       [start, execute, suspend, resume, complete, terminate].

   preset(start) -> [idle];
   preset(execute) -> [starting];
   preset(suspend) -> [running];
   preset(resume) -> [waiting];
   preset(complete) -> [completing];
   preset(terminate) -> [running, waiting, completing].
   ```

3. **Handle workflow execution in fire/3**
   - Execute tasks when tokens reach appropriate places
   - Create workitems for task execution
   - Handle completion and failure

**Deliverable**: gen_pnet-based workflow instance

---

### Phase 4: Orchestrator Integration

**Goal**: Update orchestrator to manage gen_pnet workflow instances

**Actions:**

1. **Update `src/yawl_orchestrator.erl`**
   - Replace custom workflow instance spawning with gen_pnet:start_link
   - Update workflow creation to use gen_pnet modules
   - Modify state queries to use gen_pnet:ls/2

2. **Workflow creation API:**
   ```erlang
   %% Before:
   {ok, Pid} = yawl_workflow_instance:start_link(WorkflowDef)

   %% After:
   {ok, Pid} = gen_pnet:start_link(
       yawl_workflow_instance_pnet,
       [WorkflowDef],
       []
   )
   ```

3. **Status queries:**
   ```erlang
   %% Before:
   {ok, Status} = gen_statem:call(WorkflowPid, get_status)

   %% After:
   {ok, Tokens} = gen_pnet:ls(WorkflowPid, running)
   ```

**Deliverable**: Orchestrator using gen_pnet instances

---

### Phase 5: Colored Petri Net Extensions

**Goal**: Extend gen_pnet for colored tokens (data-carrying tokens)

**Actions:**

1. **Create `src/yawl_cpn_pnet.erl`**
   - Wrapper around gen_pnet for colored tokens
   - Token structure: `#{data => term(), color => atom(), timestamp => integer()}`

2. **Implement colored token semantics:**
   ```erlang
   -callback token_color(Token) -> atom().
   -callback token_data(Token) -> term().
   -callback merge_tokens(Tokens) -> MergedTokens.
   ```

3. **Add guard support:**
   ```erlang
   is_enabled(Transition, Mode, _UsrInfo) ->
       InputTokens = get_input_tokens(Transition, Mode),
       lists:all(fun(T) -> check_guard(T, Transition) end, InputTokens).
   ```

**Deliverable**: Colored Petri net support for gen_pnet

---

### Phase 6: Testing and Validation

**Goal**: Ensure all functionality works with gen_pnet

**Tests:**

1. **Unit tests for each pattern** (43 test suites)
2. **Integration tests for workflow execution**
3. **Performance benchmarks** (compare old vs new)
4. **Regression tests** (existing workflows must still work)

**Validation:**
- All 43 patterns execute correctly
- Token passing semantics match YAWL specification
- Cancellation patterns work as expected
- Colored tokens carry data correctly

---

### Phase 7: Migration and Backward Compatibility

**Goal**: Provide migration path from old to new implementation

**Actions:**

1. **Create migration layer**
   - Old API calls wrapper to new gen_pnet implementation
   - Data migration from old state to gen_pnet state

2. **Deprecation warnings**
   - Mark old modules as deprecated
   - Provide migration guide

3. **Feature parity**
   - Ensure all old features work in new implementation
   - Performance must be comparable or better

---

## Critical Files to Create/Modify

### New Files

```
src/
├── yawl_pnet.erl                    # gen_pnet wrapper for YAWL
├── yawl_workflow_instance_pnet.erl  # gen_pnet workflow instance
├── yawl_cpn_pnet.erl                # Colored Petri nets for gen_pnet
├── yawl_patterns/                   # Pattern modules directory
│   ├── basic_sequential.erl
│   ├── parallel_split.erl
│   ├── parallel_join.erl
│   ├── exclusive_choice.erl
│   ├── simple_merge.erl
│   ├── iterative_loop.erl
│   ├── multi_instance.erl
│   └── ... (43 pattern modules)
```

### Modified Files

```
src/
├── yawl_orchestrator.erl            # Use gen_pnet instances
├── yawl_workitem_processor.erl      # Integrate with gen_pnet triggers
├── yawl_persistence.erl             # Persist gen_pnet net_state
├── a2a_erl_sup.erl                  # Update supervision tree
└── a2a_erl.app.src                  # Add new modules

include/
└── yawl_types.hrl                   # Add gen_pnet records
```

### Deprecated Files

```
src/
├── yawl_workflow_instance.erl       # Replaced by _pnet version
└── (kept for backward compatibility)
```

---

## Implementation Details

### Pattern to Petri Net Mapping Example

**Basic Sequential Pattern:**

```erlang
%% File: src/yawl_patterns/basic_sequential.erl
-module(basic_sequential).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1]).

place_lst() ->
    [start, task1, task2, end].

trsn_lst() ->
    [start_task1, complete_task1, complete_task2].

init_marking(start, _UsrInfo) ->
    [token];
init_marking(_Place, _UsrInfo) ->
    [].

preset(start_task1) ->
    [start];
preset(complete_task1) ->
    [task1];
preset(complete_task2) ->
    [task2].

is_enabled(Transition, Mode, _UsrInfo) ->
    %% Check if preset places have tokens
    Preset = preset(Transition),
    lists:all(fun(P) ->
        case maps:get(P, Mode, []) of
            [] -> false;
            _ -> true
        end
    end, Preset).

fire(start_task1, _Mode, _UsrInfo) ->
    {produce, #{task1 => [token]}};
fire(complete_task1, _Mode, _UsrInfo) ->
    {produce, #{task2 => [token]}};
fire(complete_task2, _Mode, _UsrInfo) ->
    {produce, #{end => [token]}}.

init(_Args) ->
    #{pattern => basic_sequential}.
```

### Cancellation Pattern Example

```erlang
%% File: src/yawl_patterns/cancelation_scope.erl
-module(cancelation_scope).
-behaviour(gen_pnet).

place_lst() ->
    [start, active_region, cancel_trigger, cancelled, end].

trsn_lst/0 ->
    [enter_region, cancel, complete, cancel_complete].

preset(enter_region) -> [start];
preset(cancel) -> [cancel_trigger];
preset(complete) -> [active_region];
preset(cancel_complete) -> [active_region, cancelled].

is_enabled(cancel, Mode, _UsrInfo) ->
    %% Cancel is enabled if cancel_trigger has token
    case maps:get(cancel_trigger, Mode, []) of
        [] -> false;
        _ -> true
    end;
is_enabled(Transition, Mode, _UsrInfo) ->
    Preset = preset(Transition),
    lists:all(fun(P) ->
        case maps:get(P, Mode, []) of
            [] -> false;
            _ -> true
        end
    end, Preset).

fire(enter_region, _Mode, _UsrInfo) ->
    {produce, #{active_region => [token]}};
fire(cancel, Mode, _UsrInfo) ->
    %% Cancel consumes all tokens in active_region
    {produce, #{cancelled => [token], active_region => []}};
fire(complete, Mode, _UsrInfo) ->
    {produce, #{end => [token]}};
fire(cancel_complete, _Mode, _UsrInfo) ->
    {produce, #{end => [token]}}.
```

---

## Verification Plan

### Step 1: Compile All Modules
```bash
cd /Users/sac/A2A/erlang/a2a_erl
rebar3 compile
# Expect: No errors, all new modules compile
```

### Step 2: Test Basic Pattern
```erlang
%% Start gen_pnet workflow with basic sequential pattern
{ok, Pid} = gen_pnet:start_link(basic_sequential, [], []).

%% Check initial marking
gen_pnet:ls(Pid, start).
% Expect: {ok, [token]}

%% Trigger transition (tokens move automatically)
%% Check task1 place
gen_pnet:ls(Pid, task1).
% Expect: {ok, [token]}

%% Check end place after execution
gen_pnet:ls(Pid, end).
% Expect: {ok, [token]}
```

### Step 3: Test Parallel Split
```erlang
{ok, Pid} = gen_pnet:start_link(parallel_split, [], []).

%% After split, tokens should be in both branches
gen_pnet:ls(Pid, branch1).
% Expect: {ok, [token]}

gen_pnet:ls(Pid, branch2).
% Expect: {ok, [token]}
```

### Step 4: Test Cancellation Pattern
```erlang
{ok, Pid} = gen_pnet:start_link(cancelation_scope, [], []).

%% Trigger cancel by adding token to cancel_trigger
gen_pnet:produce_token(Pid, cancel_trigger, cancel_token).

%% Check that active_region is empty (cancelled)
gen_pnet:ls(Pid, active_region).
% Expect: {ok, []}

%% Check cancelled place
gen_pnet:ls(Pid, cancelled).
% Expect: {ok, [token]}
```

### Step 5: Integration Test
```bash
rebar3 ct --suite yawl_pnet_SUITE
# Expect: All pattern tests pass

rebar3 ct --suite yawl_workflow_integration_SUITE
# Expect: End-to-end workflow tests pass
```

### Step 6: Performance Benchmark
```erlang
%% Compare execution times
yawl_bench:compare_implementation().
%% Should show gen_pnet is comparable or faster
```

---

## Timeline Estimate

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| 1. Core gen_pnet Integration | 3-5 days | Basic pattern working |
| 2. Pattern Migration | 7-10 days | All 43 patterns as gen_pnet |
| 3. Workflow Instance Refactoring | 3-5 days | gen_pnet workflow instance |
| 4. Orchestrator Integration | 2-3 days | Orchestrator using gen_pnet |
| 5. Colored Petri Net Extensions | 3-4 days | CPN support |
| 6. Testing and Validation | 5-7 days | Full test coverage |
| 7. Migration and Compatibility | 2-3 days | Migration layer |

**Total**: 25-37 days (5-7 weeks)

---

## Risks and Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| gen_pnet doesn't support YAWL-specific features | High | Implement extensions as gen_pnet plugins |
| Performance degradation | Medium | Benchmark and optimize hot paths |
| Breaking existing workflows | High | Provide backward compatibility layer |
| Cancellation pattern complexity | High | Dedicated research and prototyping phase |
| Large refactoring scope | High | Incremental rollout with feature flags |

---

## Success Criteria

✅ **Minimum Viable:**
- All 43 YAWL patterns work as gen_pnet modules
- Workflows execute end-to-end
- Token passing semantics correct

✅ **Full Success:**
- All existing tests pass
- Performance matches or exceeds current implementation
- Colored Petri net features working
- Zero breaking changes for existing users
- Documentation updated
