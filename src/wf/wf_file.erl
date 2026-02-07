%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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

-module(wf_file).
-moduledoc """
File reference tokens with metadata.

File references are data maps, not actual file handles. They represent
a file path along with associated metadata like MIME type and checksums.

## Creating File References

```erlang
> F = wf_file:ref(<<"/tmp/invoice.pdf">>, #{mime => <<"application/pdf">>, sha256 => <<1:256>>}).
#{mime => <<"application/pdf">>,path => <<"/tmp/invoice.pdf">>,sha256 => <<1:256>>}
```

## Accessing File Properties

```erlang
> F = wf_file:ref(<<"/tmp/invoice.pdf">>, #{mime => <<"application/pdf">>}).
#{mime => <<"application/pdf">>,path => <<"/tmp/invoice.pdf">>}

> wf_file:path(F).
<<"/tmp/invoice.pdf">>

> wf_file:mime(F).
<<"application/pdf">>
```

## Integration with wf_data

File references integrate seamlessly with the workflow data store:

```erlang
> F = wf_file:ref(<<"/tmp/invoice.pdf">>, #{mime => <<"application/pdf">>, sha256 => <<1:256>>}).
#{mime => <<"application/pdf">>,path => <<"/tmp/invoice.pdf">>,sha256 => <<1:256>>}

> D0 = wf_data:new(), D1 = wf_data:put(D0, invoice, F), wf_data:get(D1, invoice).
#{mime => <<"application/pdf">>,path => <<"/tmp/invoice.pdf">>,sha256 => <<1:256>>}
```

## File Reference Structure

All file references are maps with the following keys:
- <b>path:</b> Binary file path (required)
- <b>mime:</b> MIME type binary (optional, defaults to <<"application/octet-stream">>)
- <b>sha256:</b> SHA-256 checksum binary (optional, for integrity verification)

Additional metadata may be stored in the map as needed.
""".

%%====================================================================
%% Exports
%%====================================================================

%% File reference constructors and accessors
-export([ref/2, path/1, mime/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc File path as binary.
%%
%% Absolute or relative path to a file on the filesystem.
%%--------------------------------------------------------------------
-type path() :: binary().

%%--------------------------------------------------------------------
%% @doc MIME type identifier as binary.
%%
%% Standard MIME type strings (e.g., <<"application/pdf">>, <<"image/png">>).
%%--------------------------------------------------------------------
-type mime() :: binary().

%%--------------------------------------------------------------------
%% @doc SHA-256 checksum as 256-bit binary.
%%
%% Used for file integrity verification.
%%--------------------------------------------------------------------
-type sha256() :: <<_:256>>.

%%--------------------------------------------------------------------
%% @doc File reference map.
%%
%% Contains file path and associated metadata. Additional keys may
%% be present for extended metadata.
%%--------------------------------------------------------------------
-type file_ref() :: #{
    path := path(),
    mime => mime(),
    sha256 => sha256()
}.

%%--------------------------------------------------------------------
%% @doc Metadata map for file reference construction.
%%
%% Optional metadata including MIME type and checksums.
%%--------------------------------------------------------------------
-type metadata() :: #{
    mime => mime(),
    sha256 => sha256()
}.

%% Export types
-export_type([file_ref/0, path/0, mime/0, sha256/0, metadata/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a file reference from a path and metadata map.
%%
%% Constructs a file reference map containing the file path and any
%% provided metadata. The path is always included as the 'path' key.
%% MIME type defaults to <<"application/octet-stream">> if not provided.
%%
%% @param FilePath The file path as a binary
%% @param Metadata Optional metadata map (mime, sha256, etc.)
%% @returns A file reference map with path and metadata
%%
%% @end
%%--------------------------------------------------------------------
-spec ref(FilePath :: path(), Metadata :: metadata()) -> file_ref().

ref(FilePath, Metadata) when is_binary(FilePath), is_map(Metadata) ->
    Mime = maps:get(mime, Metadata, <<"application/octet-stream">>),
    Base = #{path => FilePath, mime => Mime},
    %% Merge any additional metadata (like sha256)
    maps:merge(Base, Metadata).

%%--------------------------------------------------------------------
%% @doc Extracts the file path from a file reference.
%%
%% Returns the 'path' value from the file reference map.
%%
%% @param FileRef A file reference map
%% @returns The file path as a binary
%%
%% @end
%%--------------------------------------------------------------------
-spec path(FileRef :: file_ref()) -> path().

path(#{path := Path}) ->
    Path.

%%--------------------------------------------------------------------
%% @doc Extracts the MIME type from a file reference.
%%
%% Returns the 'mime' value from the file reference map.
%%
%% @param FileRef A file reference map
%% @returns The MIME type as a binary
%%
%% @end
%%--------------------------------------------------------------------
-spec mime(FileRef :: file_ref()) -> mime().

mime(#{mime := Mime}) ->
    Mime.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test ref/2
ref_test() ->
    Path = <<"/tmp/test.pdf">>,
    Metadata = #{mime => <<"application/pdf">>, sha256 => <<1:256>>},
    Ref = ref(Path, Metadata),
    ?assertEqual(Path, maps:get(path, Ref)),
    ?assertEqual(<<"application/pdf">>, maps:get(mime, Ref)),
    ?assertEqual(<<1:256>>, maps:get(sha256, Ref)).

%% Test ref/2 with default MIME
ref_default_mime_test() ->
    Ref = ref(<<"/tmp/test.bin">>, #{}),
    ?assertEqual(<<"application/octet-stream">>, maps:get(mime, Ref)).

%% Test path/1
path_test() ->
    Ref = ref(<<"/tmp/invoice.pdf">>, #{mime => <<"application/pdf">>}),
    ?assertEqual(<<"/tmp/invoice.pdf">>, path(Ref)).

%% Test mime/1
mime_test() ->
    Ref = ref(<<"/tmp/data.json">>, #{mime => <<"application/json">>}),
    ?assertEqual(<<"application/json">>, mime(Ref)).

%% Test integration with wf_data
integration_test() ->
    D0 = wf_data:new(),
    F = ref(<<"/tmp/invoice.pdf">>, #{mime => <<"application/pdf">>, sha256 => <<1:256>>}),
    D1 = wf_data:put(D0, invoice, F),
    ?assertEqual(F, wf_data:get(D1, invoice)).

-endif.
