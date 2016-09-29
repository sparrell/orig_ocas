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
%%        , resource_exists/2
        , content_types_accepted/2
        , handle_json/2
        ]).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:debug("~s ~s", [Method, URL]),
    {ok, Req2, #{}}.

allowed_methods(Req, State) ->
    %%lager:debug("got to allowed methods"),
    {[<<"POST">>], Req, State}.

%%resource_exists(Req, State) ->
    %% returning false since only method allowed is post
    %%    and this routine creates new resource
    %%    maybe should have used put instead
%%    {false, Req, State}.

content_types_accepted(Req, State) ->
    %%lager:debug("got to content_types"),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    %%lager:debug("got to handle_json"),
    %% put stuff here for actually doing stuff

    %% handle case of no body
    HasBody = cowboy_req:has_body(Req),
    Req1 = respond_json(HasBody, Req),

    lager:debug("got past resond_json"),
    {true, Req1, State}.

%% respond to not having the necessary json body
respond_json(false, Req) ->
    lager:debug("got to respond_json false"),
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);

%% respond when there is a body
respond_json(true, Req) ->
    %%lager:debug("got to respond_json true"),

    %% get the body of the request
    { ok, Body, Req1} = cowboy_req:body(Req),

    %% decode the json into a map of erlang terms
    JsonMap = jsx:decode(Body, [return_maps]),
    %%lager:debug("handle_json JsonInputMap ~p", [JsonMap] ),

    %% verify json - add this in eventually
    %% put stuff here

    %% separate out the commands and deal with each - add this in eventually
    %% put stuff here
    What = json_actions(JsonMap),
    
    %% for now just reply with what came in as plain text
    ReplyBody = jsx:encode(JsonMap, [{indent,2}]),
    Headers = [ {<<"content-type">>, <<"text/plain; charset=utf-8">>} ],
    %% put together the reply and return it
    lager:debug("about to do reply in respond json"),
    cowboy_req:reply(200, Headers, ReplyBody, Req1).
    

%% do different things for the different actions (and fail if action not found)

%%     match on scan
json_actions(#{ <<"action">> := <<"scan">> } ) ->
    lager:debug("GOT TO scan!!!!"),
    ok;

%%     match on locate
json_actions(#{ <<"action">> := <<"locate">> } ) ->
    lager:debug("GOT TO locate!!!!"),
    ok;

%%     match on query
json_actions(#{ <<"action">> := <<"query">> } ) ->
    lager:debug("GOT TO query!!!!"),
    ok;

%%     match on report
json_actions(#{ <<"action">> := <<"report">> } ) ->
    lager:debug("GOT TO report!!!!"),
    ok;

%%     match on get
json_actions(#{ <<"action">> := <<"get">> } ) ->
    lager:debug("GOT TO get!!!!"),
    ok;

%%     match on notify
json_actions(#{ <<"action">> := <<"notify">> } ) ->
    lager:debug("GOT TO notify!!!!"),
    ok;

%%     match on deny
json_actions(#{ <<"action">> := <<"deny">> } ) ->
    lager:debug("GOT TO deny!!!!"),
    ok;

%%     match on contain
json_actions(#{ <<"action">> := <<"contain">> } ) ->
    lager:debug("GOT TO contain!!!!"),
    ok;

%%     match on allow
json_actions(#{ <<"action">> := <<"allow">> } ) ->
    lager:debug("GOT TO allow!!!!"),
    ok;

%%     match on start
json_actions(#{ <<"action">> := <<"start">> } ) ->
    lager:debug("GOT TO start!!!!"),
    ok;

%%     match on stop
json_actions(#{ <<"action">> := <<"stop">> } ) ->
    lager:debug("GOT TO stop!!!!"),
    ok;

%%     match on restart
json_actions(#{ <<"action">> := <<"restart">> } ) ->
    lager:debug("GOT TO restart!!!!"),
    ok;

%%     match on pause
json_actions(#{ <<"action">> := <<"pause">> } ) ->
    lager:debug("GOT TO pause!!!!"),
    ok;

%%     match on resume
json_actions(#{ <<"action">> := <<"resume">> } ) ->
    lager:debug("GOT TO resume!!!!"),
    ok;

%%     match on cancel
json_actions(#{ <<"action">> := <<"cancel">> } ) ->
    lager:debug("GOT TO cancel!!!!"),
    ok;

%%     match on set
json_actions(#{ <<"action">> := <<"set">> } ) ->
    lager:debug("GOT TO set!!!!"),
    ok;

%%     match on update
json_actions(#{ <<"action">> := <<"update">> } ) ->
    lager:debug("GOT TO update!!!!"),
    ok;

%%     match on move
json_actions(#{ <<"action">> := <<"move">> } ) ->
    lager:debug("GOT TO move!!!!"),
    ok;

%%     match on redirect
json_actions(#{ <<"action">> := <<"redirect">> } ) ->
    lager:debug("GOT TO redirect!!!!"),
    ok;

%%     match on delete
json_actions(#{ <<"action">> := <<"delete">> } ) ->
    lager:debug("GOT TO delete!!!!"),
    ok;

%%     match on snapshot
json_actions(#{ <<"action">> := <<"snapshot">> } ) ->
    lager:debug("GOT TO snapshot!!!!"),
    ok;

%%     match on detonate
json_actions(#{ <<"action">> := <<"detonate">> } ) ->
    lager:debug("GOT TO detonate!!!!"),
    ok;

%%     match on restore
json_actions(#{ <<"action">> := <<"restore">> } ) ->
    lager:debug("GOT TO restore!!!!"),
    ok;

%%     match on save
json_actions(#{ <<"action">> := <<"save">> } ) ->
    lager:debug("GOT TO save!!!!"),
    ok;

%%     match on modify
json_actions(#{ <<"action">> := <<"modify">> } ) ->
    lager:debug("GOT TO modify!!!!"),
    ok;

%%     match on throttle
json_actions(#{ <<"action">> := <<"throttle">> } ) ->
    lager:debug("GOT TO throttle!!!!"),
    ok;

%%     match on delay
json_actions(#{ <<"action">> := <<"delay">> } ) ->
    lager:debug("GOT TO delay!!!!"),
    ok;

%%     match on substitute
json_actions(#{ <<"action">> := <<"substitute">> } ) ->
    lager:debug("GOT TO substitute!!!!"),
    ok;

%%     match on copy
json_actions(#{ <<"action">> := <<"copy">> } ) ->
    lager:debug("GOT TO copy!!!!"),
    ok;

%%     match on sync
json_actions(#{ <<"action">> := <<"sync">> } ) ->
    lager:debug("GOT TO sync!!!!"),
    ok;

%%     match on distill
json_actions(#{ <<"action">> := <<"distill">> } ) ->
    lager:debug("GOT TO distill!!!!"),
    ok;

%%     match on augment
json_actions(#{ <<"action">> := <<"augment">> } ) ->
    lager:debug("GOT TO augment!!!!"),
    ok;

%%     match on investigate
json_actions(#{ <<"action">> := <<"investigate">> } ) ->
    lager:debug("GOT TO investigate!!!!"),
    ok;

%%     match on mitigate
json_actions(#{ <<"action">> := <<"mitigate">> } ) ->
    lager:debug("GOT TO mitigate!!!!"),
    ok;

%%     match on remediate
json_actions(#{ <<"action">> := <<"remediate">> } ) ->
    lager:debug("GOT TO remediate!!!!"),
    ok;

%%     match on response
json_actions(#{ <<"action">> := <<"response">> } ) ->
    lager:debug("GOT TO response!!!!"),
    ok;

%%     match on alert
json_actions(#{ <<"action">> := <<"alert">> } ) ->
    lager:debug("GOT TO alert!!!!"),
    ok;

%%     match on scan
json_actions(#{ <<"action">> := <<"scan">> } ) ->
    lager:debug("GOT TO scan!!!!"),
    ok;

json_actions(JsonMap) ->
    %% something is wrong!!! Bad input json with unrecognized action
    lager:debug("Bad JsonMap Action: ~p", [JsonMap]),
    #{ <<"action">> := Action} = JsonMap,
   got_bad_action = Action,  %% should blow up here - fix later to respond with bad input
    ok.
