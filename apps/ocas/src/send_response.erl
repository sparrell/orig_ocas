-module(send_response).
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
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
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES. LOSS OF USE,
%%% DATA, OR PROFITS. OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-license("Apache 2.0").

-export([ send_response/2
        , unpid/3
        ]).

send_response(Req, State) ->

    %% for now just reply with some of State as json
    Keys = maps:keys(State),
    TransformedState = unpid( Keys, State, #{} ),
    lager:debug("TransformedState ~p", [TransformedState]),
    ReplyBody = jsx:encode( TransformedState ),

    Headers = [ {<<"content-type">>, <<"application/json">>} ],
    {ok, Req2} = cowboy_req:reply(200, Headers, ReplyBody, Req),
    {ok, Req2, State}.

unpid([], _OldMap, NewMap) ->
    %% done since no keys left. Return Map
    NewMap;

unpid( [ Key | RestOfKeys ], OldMap, NewMap ) ->
    %% get value to go with the key
    Value = maps:get(Key, OldMap),
    %% transform if it's not jsonifyable
    NewValue = transform_value(Value),

    %% put in the new map
    UpdatedMap = maps:put(Key, NewValue, NewMap),

    %% recurse thru rest of keys
    unpid( RestOfKeys, OldMap, UpdatedMap ).

transform_value(Value) ->
    %% return transformed value if value is a pid,
    %%        otherwise leave alone
    case is_pid(Value) of
        true ->
            %% is a pid, so return text rep instead
            list_to_binary( io_lib:format("~p", [Value]) );
        false ->
            Value
    end.
