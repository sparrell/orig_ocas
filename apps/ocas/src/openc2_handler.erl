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
        , resource_exists/2
        , content_types_accepted/2
        , handle_json/2
        , content_types_provided/2
        , to_json/2
        , to_text/2
        , to_html/2
        ]).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:debug("~s ~s", [Method, URL]),
    {ok, Req2, #{}}.

allowed_methods(Req, State) ->
    lager:debug("got to allowed methods~n"),
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    %% returning false since only method allowed is post
    %%    and this routine creates new resource
    {false, Req, State}.

content_types_accepted(Req, State) ->
    lager:debug("got to content_types~n"),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    lager:debug("got to handle_json~n"),
    %% put stuff here for actually doing stuff
    { ok, Body, Req1} = cowboy_req:body(Req),
    JsonMap = jsx:decode(Body, [return_maps]),
    lager:debug("handle_json JsonInputMap ~p", [JsonMap] ),

    lager:debug("handle_json Req ~p", [Req1] ),
    lager:debug("handle_json State ~p", [State] ),

    %% verify json - add this in eventually

    %% store the json in State for later use
    State2 = maps:put(json, JsonMap, State),

    %% return
    {true, Req1, State2}.


%% What type of output allowed (and for now process input to output here)
content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/plain">>, to_text},
        {<<"text/html">>, to_html}
    ], Req, State}.

%% reply with json
to_json(Req, State) ->
  Json = maps:get(json, State, undefined),
  Body = jsx:encode(Json, [{indent,2}]),
  {Body, Req, State}.

%% reply with text
to_text(Req, State) ->
  Json = maps:get(json, State, undefined),
  RawText = io:format("~p", [Json]),
  BinText = list_to_binary(RawText),
  Body = BinText,
  {Body, Req, State}.

%% reply with html
to_html(Req, State) ->
  Json = maps:get(json, State, undefined),
  JsonText = jsx:encode(Json, [{indent,2}]),
  Head = "<html><head><meta charset=\"utf-8\"><title>Json as html text!</title></head><body>",
  Mid  = io:format("~p", [JsonText]),
  Tail = "</body></html>",
  RawText = Head + Mid + Tail,
  BinText = list_to_binary(RawText),
  Body = BinText,
  {Body, Req, State}.
