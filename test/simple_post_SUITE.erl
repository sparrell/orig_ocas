%%%-------------------------------------------------------------------
%%% Copyright (c) 2016, sFractal Consulting, LLC

%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at

%%%     http://www.apache.org/licenses/LICENSE-2.0

%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc test simple post
%% @end
%%%-------------------------------------------------------------------

-module(simple_post_SUITE).
-author("Duncan Sparrell").
-copyright("2016, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_post
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap,{minutes,2}}].

%% setup config parameters
init_per_suite(Config) ->
    {ok, AppList} = application:ensure_all_started(lager),
    lager:info("AppList: ~p~n", [AppList]),

    {ok, AppList2} = application:ensure_all_started(shotgun),
    lager:info("AppList2: ~p~n", [AppList2]),

    %% since ct doesn't read sys.config, set configs here
    application:set_env(ocas, port, 8080),
    application:set_env(ocas, listener_count, 5),

    %% start application
    {ok, AppList3} = application:ensure_all_started(ocas),
    lager:info("AppList3: ~p~n", [AppList3]),

    Config.

test_post(_Config) ->
    MyPort = application:get_env(ocas, port, 8080),
    lager:info("test_post:port= ~p", [MyPort]),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>,<<"application/json">>} ],
    SomeData = #{ fractalAlg => julian
                , colorAlg => simplest
                },
    Body = jsx:encode(SomeData),
    Options = #{},
    {ok, Response} = shotgun:post(Conn, "/status", Headers, Body, Options),
    #{ status_code := 201, headers := RespHeaders } = Response,
    %% test header contents are correct
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, RespHeaders),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    { <<"content-length">>, <<"0">>} =  lists:keyfind(<<"content-length">>, 1, RespHeaders),
    { <<"content-type">>, <<"text/html">>} =  lists:keyfind(<<"content-type">>, 1, RespHeaders),

    ok.

