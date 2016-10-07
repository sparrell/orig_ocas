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
    [ test_get_ok
    , test_get_status
    , test_post
    , test_post_missing_body
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap,{minutes,2}}].

%% setup config parameters
init_per_suite(Config) ->
    {ok, _AppList} = application:ensure_all_started(lager),
    %%lager:info("AppList: ~p~n", [AppList]),

    {ok, _AppList2} = application:ensure_all_started(shotgun),
    %%lager:info("AppList2: ~p~n", [AppList2]),

    %% since ct doesn't read sys.config, set configs here
    application:set_env(ocas, port, 8080),
    application:set_env(ocas, listener_count, 5),

    %% start application
    {ok, _AppList3} = application:ensure_all_started(ocas),
    %%lager:info("AppList3: ~p~n", [AppList3]),

    Config.

test_get_ok(_Config) ->

    %% send request, get response, and deciper text response
    send_recieve( [ {<<"content-type">>, <<"application/text">>}  %% sending Headers
                  , {<<"accept">>, <<"text/plain">>} 
                  ] 
                , #{}      %% Options
                , "/ok"    %% Url
                , 200      %% ExpectedStatus
                , <<"ok">> %% ExpectedBody
                ),

    %% send request, get response, and deciper html response
    send_recieve( [ {<<"content-type">>, <<"application/text">>}  %% sending Headers
                  , {<<"accept">>, <<"text/html">>} 
                  ] 
                , #{}      %% Options
                , "/ok"    %% Url
                , 200      %% ExpectedStatus
                , <<"<html><body>ok</body></html>">> %% ExpectedBody
                ),

    %% send request, get response, and deciper json response
    send_recieve( [ {<<"content-type">>, <<"application/text">>}  %% sending Headers
                  , {<<"accept">>, <<"application/json">>} 
                  ] 
                , #{}      %% Options
                , "/ok"    %% Url
                , 200      %% ExpectedStatus
                , <<"{ \"status : ok\" }">> %% ExpectedBody
                ),

    ok.


test_get_status(_Config) ->
    MyPort = application:get_env(ocas, port, 8080),
    %%lager:info("test_post:port= ~p", [MyPort]),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    %%lager:info("connection = ~p", [Conn]),
    Headers = [ {<<"content-type">>,<<"application/text">>} ],
    Options = #{},
    ResponseToGet = shotgun:get(Conn, "/status", Headers, Options),
    %%lager:info("response = ~p", [ResponseToGet]),
    {ok, Response} = ResponseToGet,

    %% breakout the status, headers, body
    #{ status_code := RespStatus, headers := RespHeaders, body := RespBody } = Response,
    %%lager:info("status = ~p", [RespStatus]),
    %%lager:info("headers = ~p", [RespHeaders]),
    %%lager:info("body = ~p", [RespBody]),

    %% valididate response code is 200 (ok) (can get 201 also?)
    200 = RespStatus,

    %% test header contents are correct
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, RespHeaders),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    { <<"content-type">>, <<"text/html">>} =  lists:keyfind(<<"content-type">>, 1, RespHeaders),
    { <<"content-length">>, <<"57">>} =  lists:keyfind(<<"content-length">>, 1, RespHeaders),

    %% valididate body content
    <<"<html><body>Status Works - needs more later</body></html>">> = RespBody,

    ok.


test_post(_Config) ->
    MyPort = application:get_env(ocas, port, 8080),
    %%lager:info("test_post:port= ~p", [MyPort]),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>,<<"application/json">>} ],

    SomeJson = <<"{\"action\": \"mitigate\",
                   \"target\": {
                      \"type\":\"cybox:Hostname\",
                      \"specifiers\":{\"Hostname_Value\":\"cdn.badco.org\"}}}">>,
    %% validate Json
    true = jsx:is_json(SomeJson),

    Body = SomeJson,
    Options = #{},

    %% send json command to openc2
    %%lager:info("about to send json to openc2"),
    {ok, Response} = shotgun:post(Conn, "/openc2", Headers, Body, Options),
    lager:info("sent json, got: ~p", [Response] ),

    %% verify got 200 for status code
    #{ status_code := 200 } = Response,
    %%lager:info("status = ~p", [RespStatus]),
    #{ headers := RespHeaders} = Response,
    %%lager:info("headers = ~p", [RespHeaders]),
    #{ body := RespBody } = Response,
    lager:info("body = ~p", [RespBody]),

    %% test header contents are correct
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, RespHeaders),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    %%   note content length is likely to change as real commands get implemented
    { <<"content-length">>, <<"136">>} =  lists:keyfind(<<"content-length">>, 1, RespHeaders),
    { <<"content-type">>, <<"text/plain; charset=utf-8">>} =  lists:keyfind(<<"content-type">>, 1, RespHeaders),

    %% test body is what was expected
    %% need to add something here (once real body exists)
    
    ok.

test_post_missing_body(_Config) ->
    %% test proper reponse to bad input (no body to html request)

    MyPort = application:get_env(ocas, port, 8080),
    %%lager:info("test_post:port= ~p", [MyPort]),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>,<<"application/json">>} ],

    Body = "",
    Options = #{},

    %% send json command to openc2
    %%lager:info("about to send json to openc2"),
    {ok, Response} = shotgun:post(Conn, "/openc2", Headers, Body, Options),
    lager:info("sent json, got: ~p", [Response] ),

    %% verify got 400 (bad request) for status code
    #{ status_code := 400 } = Response,
    %%lager:info("status = ~p", [RespStatus]),

    #{ headers := RespHeaders} = Response,
    %%lager:info("headers = ~p", [RespHeaders]),
    #{ body := RespBody } = Response,
    lager:info("body = ~p", [RespBody]),

    %% test header contents are correct
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, RespHeaders),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    %% note content length is for error mesg "Missing Body."
    { <<"content-length">>, <<"13">>} =  lists:keyfind(<<"content-length">>, 1, RespHeaders),
    %% not sure why error response is in html?
    { <<"content-type">>, <<"text/html">>} =  lists:keyfind(<<"content-type">>, 1, RespHeaders),

    %% test body is what was expected
    RespBody = <<"Missing body.">>,

    ok.

%%%%%%%%%%%%%%%%%%%% Utilities

%% utility to save putting this in each test
send_recieve( Headers          % to send
            , Options          % to send
            , Url              % to send
            , ExpectedStatus  % test get this received
            , ExpectedBody
            ) ->

    MyPort = application:get_env(ocas, port, 8080),
    %%lager:info("test_post:port= ~p", [MyPort]),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    {ok, Response} = shotgun:get(Conn, Url, Headers, Options),
    %%lager:info("response = ~p", [Response]),

    %% get status code of response
    #{ status_code := RespStatus } = Response,
    %%lager:info("status = ~p", [RespStatus]),
    %% test what received was what was expected
    ExpectedStatus = RespStatus,

    %% get headers
    #{ headers := RespHeaders } = Response,
    %%lager:info("headers = ~p", [RespHeaders]),

    %% verify headers
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, RespHeaders),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    

    %% check if has body and if it is correct
    #{ body := RespBody } = Response,
    %%lager:info("RespBody = ~p", [RespBody]),
    %%lager:info("ExpectedBody = ~p", [ExpectedBody]),
    ExpectedBody = RespBody,

    %% return 
    ok.

    

