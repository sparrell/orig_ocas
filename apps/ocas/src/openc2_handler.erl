-module(openc2_handler).
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-license("Apache 2.0").

-export([init/3
        , rest_init/2
        , allowed_methods/2
        , content_types_accepted/2
        , handle_json/2
        ]).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:info("rest_init:~s ~s", [Method, URL]),
    {ok, Req2, #{}}.

allowed_methods(Req, State) ->
    lager:info("got to allowed methods"),
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    lager:info("got to content_types"),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    lager:info("got to handle_json"),

    %% check for case of no body
    HasBody = cowboy_req:has_body(Req),
    body_check(HasBody, Req, State ).

%% handle case of whether body present or not
body_check(false, Req, State) ->
    %% no body so bad request
    State2 = maps:put(has_http_body, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing body.">>, Req),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

body_check(true, Req, State) ->
    %% body present so move to next test
    State2 = maps:put(has_http_body, true, State),

    %% get the body of the request
    { ok, Body, Req1} = cowboy_req:body(Req),
    State3 = maps:put(http_body, Body, State2),

    %% check if body is json as it should be
    IsJson = jsx:is_json(Body),

    is_body_json(IsJson, Req1, State3).

is_body_json(false, Req, State) ->
    %% decoding json failed so bad request
    State2 = maps:put(good_json, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Bad JSON">>, Req),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

is_body_json(true, Req, State) ->
    %% json decodes ok so move on to next test
    State2 = maps:put(good_json, true, State),

    Body = maps:get(http_body, State2),
    JsonMap = jsx:decode(Body, [return_maps]),
    lager:info("handle_json Json: InputMap ~p", [JsonMap] ),
    State3 = maps:put(json_map, JsonMap, State2),

    %% check for action
    ActionKeyExists = maps:is_key( <<"action">>, JsonMap ),
    has_action( ActionKeyExists, Req, State3 ).


has_action(false, Req, State ) ->
    %% Json doesn't have a action so abort ie bad request
    State2 = maps:put(has_action, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing action">>, Req),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

has_action(true, Req, State ) ->
    %% json has action so move on
    State2 = maps:put(has_action, true, State),

    %% move on to next step - is it a valid action
    JsonMap = maps:get(json_map, State2),
    ActionBin = maps:get( <<"action">>, JsonMap ),
    lager:info("action bintext: ~p", [ActionBin] ),
    { ActionValid, {ActionModule, ActionFunction} } = actions:is_valid_action(ActionBin),
    State3 = maps:put(action_valid, ActionValid, State2),
    State4 = maps:put(action_module, ActionModule, State3),
    State5 = maps:put(action_function, ActionFunction, State4),

    %% check if action is valid
    check_valid_action( ActionValid
                      , ActionModule
                      , ActionFunction
                      , Req
                      , State5 
                      ).

%% check if action is in valid
check_valid_action( false, _ActionModule, _ActionFunction, Req, State ) ->
    %% illegal action so bad input
    lager:info("check_valid_action: bad action" ),

    {ok, Req2} = cowboy_req:reply(400, [], <<"bad action value">>, Req),
    %% return - not tail recursive since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State};

check_valid_action( true, ActionModule, ActionFunction, Req, State ) ->
    %% valid action
    %%   check existence of target, actuator, modifiers
    %%   and then spin up action process
    lager:info("check_valid_action: good action: ~p", [ActionFunction] ),

    JsonMap = maps:get(json_map, State),
    TargetKeyExists = maps:is_key( <<"target">>, JsonMap ),
    State2 = maps:put(has_target, TargetKeyExists, State),
    ActuatorKeyExists = maps:is_key( <<"actuator">>, JsonMap ),
    State3 = maps:put(has_actuator, ActuatorKeyExists, State2),
    ModifiersKeyExists = maps:is_key( <<"modifiers">>, JsonMap ),
    State4 = maps:put(has_modifiers, ModifiersKeyExists, State3),

    %% spin up the action process
    { ActionKeepAliveWorked, ActionPid } = actions:spawn_action( ActionModule
                                                               , ActionFunction
                                                               , JsonMap ),
    lager:info("~p=~p keepalive: ~p", [ ActionFunction
                                      , ActionPid
                                      , ActionKeepAliveWorked
                                      ]),

    %% save whether process startup worked for now and then reply
    %% later fix so actually does stuff
    State5 = maps:put(action_keepalive, ActionKeepAliveWorked, State4),

    send_response(Req, State5).

send_response(Req, State) ->
    %% for now just reply with state as json
    ReplyBody = jsx:encode( State ),

    Headers = [ {<<"content-type">>, <<"application/json">>} ],
    cowboy_req:reply(200, Headers, ReplyBody, Req).




