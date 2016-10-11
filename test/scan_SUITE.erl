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
%% @doc test scan action
%% @end
%%%-------------------------------------------------------------------

-module(scan_SUITE).
-author("Duncan Sparrell").
-copyright("2016, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_scan
    , test_bad_scan
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap, {minutes, 2}}].

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

test_scan(_Config) ->

    ReqHeaders = [ {<<"content-type">>, <<"application/json">>}
                 ],

    Url = "/openc2",

    Options = #{},

    Json = <<"{ \"action\": \"scan\",
              \"target\": { \"type\": \"cybox:Device\",
                          \"specifiers\": \"NetworkScanner\"
                         },
               \"actuator\": {
                  \"type\": \"network-scanner\",
                  \"specifiers\": \"scanner01\"
                           },
               \"modifiers\": { \"response\": \"ack\",
                              \"where\": \"perimeter\"}
             }">>,

    %% validate the json
    true = jsx:is_json(Json),

    %% send the json in the body of the request
    ReqBody = Json,

    %% expect to get 200 status code
    ExpectedStatus = 200,

    %% for now command to reply with dummy response (json of State)
    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"has_http_body">>, true}
                        , {<<"good_json">>, true}
                        , {<<"action_module">>, <<"act_scan">>}
                        , {<<"action_function">>, <<"scan_server">>}
                        , {<<"action_valid">>, true}
                        , {<<"has_actuator">>, true}
                        , {<<"has_modifiers">>, true}
                        , {<<"has_target">>, true}
                        , {<<"action_keepalive">>, true}
                        ],

    %% send request, test response
    send_recieve( ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , Url              % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonPairs
                ),

    ok.


test_bad_scan(_Config) ->

    ReqHeaders = [ {<<"content-type">>, <<"application/json">>}
                 ],

    Url = "/openc2",

    Options = #{},

    %% Scan requires a target and a target - leave off target and it should fail
    Json = <<"{ \"action\": \"scan\",
               \"actuator\": {
                  \"type\": \"network-scanner\",
                  \"specifiers\": \"scanner01\"
                           },
               \"modifiers\": { \"response\": \"ack\",
                              \"where\": \"perimeter\"}
             }">>,

    %% validate the json
    true = jsx:is_json(Json),

    %% send the json in the body of the request
    ReqBody = Json,

    %% expect to get 200 status code
    %%    for now since not doing action specific semantic checks yet
    ExpectedStatus = 200,

    %% but will get 'false' for has_actuator
    ExpectedJsonPairs = [ {<<"has_http_body">>, true}
                        , {<<"good_json">>, true}
                        , {<<"action_module">>, <<"act_scan">>}
                        , {<<"action_function">>, <<"scan_server">>}
                        , {<<"action_valid">>, true}
                        , {<<"has_actuator">>, true}
                        , {<<"has_modifiers">>, true}
                        , {<<"has_target">>, false}
                        , {<<"action_keepalive">>, true}
                        ],

    %% send request, test response
    send_recieve( ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , Url              % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonPairs
                ),


    ok.


%%%%%%%%%%%%%%%%%%%% Utilities

%% utility to save putting this in each test
send_recieve( ReqHeaders          % to send
            , Options          % to send
            , ReqBody          % to send
            , Url              % to send
            , ExpectedStatus  % test get this received
            , ExpectedJsonPairs   % list of key/values in json
            ) ->

    MyPort = application:get_env(ocas, port, 8080),
    %%lager:info("test_post:port= ~p", [MyPort]),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    {ok, Response} = shotgun:post(Conn, Url, ReqHeaders, ReqBody, Options),
    lager:info("response = ~p", [Response]),

    %% get status code of response
    #{ status_code := RespStatus } = Response,
    %%lager:info("status = ~p", [RespStatus]),
    %% test what received was what was expected
    ExpectedStatus = RespStatus,

    %% get headers
    #{ headers := RespHeaders } = Response,
    %%lager:info("headers = ~p", [RespHeaders]),

    %% verify headers
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),

    %% check if has body
    #{ body := RespBody } = Response,

    %% check body is json
    true = jsx:is_json(RespBody),

    %% decode json into erlang map
    JsonMap = jsx:decode( RespBody, [return_maps] ),

    lager:info("ExpectedJsonPairs: ~p", [ExpectedJsonPairs]),
    lager:info("JsonMap: ~p", [JsonMap]),

    %% check key/value pairs are as expected
    check_map(ExpectedJsonPairs, JsonMap),

    %% return
    ok.

check_map( [], _JsonMap ) ->
    %% done since list is empty
    ok;

check_map( [ {Key, Value} | RestOfExpectedJsonPairs ], JsonMap ) ->
    %% Grab  first item in list and verify
    lager:info("Testing Key/Value: ~p/~p", [Key, Value]),

    true = maps:is_key(Key, JsonMap),
    Value = maps:get(Key, JsonMap),

    %% recurse thru remaining items in list
    check_map( RestOfExpectedJsonPairs, JsonMap).


