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
    lager:debug("got to allowed methods"),
    {[<<"POST">>], Req, State}.

%%resource_exists(Req, State) ->
    %% returning false since only method allowed is post
    %%    and this routine creates new resource
    %%    maybe should have used put instead
%%    {false, Req, State}.

content_types_accepted(Req, State) ->
    lager:debug("got to content_types"),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    lager:debug("got to handle_json"),
    %% put stuff here for actually doing stuff

    HasBody = cowboy_req:has_body(Req),
    Req1 = respond_json(HasBody, Req),
    %%respond_json(HasBody, Req),
    lager:debug("got past resond_json"),
    {true, Req1, State}.

%% respond to not having the necessary json body
respond_json(false, Req) ->
    lager:debug("got to respond_json false"),
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);

%% respond when there is a body
respond_json(true, Req) ->
    lager:debug("got to respond_json true"),

    %% get the body of the request
    { ok, Body, Req1} = cowboy_req:body(Req),

    %% decode the json into a map of erlang terms
    JsonMap = jsx:decode(Body, [return_maps]),
    lager:debug("handle_json JsonInputMap ~p", [JsonMap] ),

    %% verify json - add this in eventually
    %% put stuff here

    %% separate out the commands and deal with each - add this in eventually
    %% put stuff here
    
    %% for now just reply with what came in as plain text
    ReplyBody = jsx:encode(JsonMap, [{indent,2}]),
    %%Headers = #{ <<"content-type">> => <<"application/json">> },
    %%Headers = #{ <<"content-type">> => <<"text/plain; charset=utf-8">> },
    Headers = [ {<<"content-type">>, <<"text/plain; charset=utf-8">>} ],
    %% put together the reply and return it
    lager:debug("about to do reply in respond json"),
%%    {ok, Req2} = cowboy_req:reply(200, Headers, ReplyBody, Req1),
    cowboy_req:reply(200, Headers, ReplyBody, Req1).
%%    lager:debug("Req2: ~p", [Req2]),
%%    Req2.  %% change this to correctly return
    
