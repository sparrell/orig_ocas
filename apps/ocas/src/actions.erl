-module(actions).
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
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES. LOSS OF USE,
%%% DATA, OR PROFITS. OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-license("Apache 2.0").

-export([ spawn_action/3 ]).

%% spawn action servers
spawn_action( <<"allow">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_allow:start(State),

    %% check with keep alive
    ActionKeepAlive = act_allow:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(allow, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"augment">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_augment:start(State),

    %% check with keep alive
    ActionKeepAlive = act_augment:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(augment, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"contain">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_contain:start(State),

    %% check with keep alive
    ActionKeepAlive = act_contain:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(contain, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"copy">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_copy:start(State),

    %% check with keep alive
    ActionKeepAlive = act_copy:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(copy, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"delay">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_delay:start(State),

    %% check with keep alive
    ActionKeepAlive = act_delay:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(delay, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"delete">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_delete:start(State),

    %% check with keep alive
    ActionKeepAlive = act_delete:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(delete, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"deny">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_deny:start(State),

    %% check with keep alive
    ActionKeepAlive = act_deny:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(deny, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"detonate">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_detonate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_detonate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(detonate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"distill">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_distill:start(State),

    %% check with keep alive
    ActionKeepAlive = act_distill:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(distill, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"get">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_get:start(State),

    %% check with keep alive
    ActionKeepAlive = act_get:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(get, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"investigate">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_investigate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_investigate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(investigate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"locate">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_locate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_locate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(locate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"mitigate">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_mitigate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_mitigate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(mitigate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"scan">>,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_scan:start(State),

    %% check with keep alive
    ActionKeepAlive = act_scan:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(scan, Pid, ActionKeepAlive, Req, State);

spawn_action( _ActionSvr,  Req, State ) ->
    %% no function for this action so reply accordingly
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Missing action function">>
                                 , Req
                                 ),
    {ok, Req2, State}.

action_valid(Action, Pid, ActionKeepAlive, Req, State) ->
    %% action was valid so update State
    State2 = maps:put(action_valid, true, State),
    State3 = maps:put(action, Action, State2),

    %% tail end recurse to verifying keepalive
    verify_keepalive( ActionKeepAlive, Pid, Req, State3).

verify_keepalive( {keepalive_received, Server}
                , Pid
                , Req
                , State
                ) ->
    %% keepalive worked as expected
    State2 = maps:put(action_keepalive, true, State),
    State3 = maps:put(action_server, Server, State2),

    %% tail recurse to sending response
    send_response(Pid, Req, State3);

verify_keepalive( UnexpectedKeepalive
                , _Pid
                , Req
                , State
                ) ->
    %% didnot get expected keepalive response
    State2 = maps:put(action_keepalive, false, State),
    lager:info("~p UnexpectedKeepalive", [UnexpectedKeepalive]),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"keepalive failed">>
                                 , Req
                                 ),
    %% don't continue on, return because of unexpected response
    {ok, Req2, State2}.

send_response(Pid, Req, State) ->

    %% for now just reply with some of State as json
    ReplyBody = jsx:encode( State ),

    State2 = maps:put(pid, Pid, State),

    Headers = [ {<<"content-type">>, <<"application/json">>} ],
    {ok, Req2} = cowboy_req:reply(200, Headers, ReplyBody, Req),
    {ok, Req2, State2}.
