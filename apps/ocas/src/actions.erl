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

-export([ is_valid_action/1
        , spawn_action/3
        , spinup/3
        ]).

is_valid_action(Action) ->
    %% this routine verifies action is on list of valid actions
    %% and returns atom which is routine to run.

    %% note it uses a list instead of a simple binary_to_atom
    %% this is intentional to prevent DoS attacks
    %%   (which would use up atoms by sending many invalid actions)

    ValidActions =
        #{ <<"alert">>        => { act_alert, alert_server}
         ,  <<"allow">>       => { act_allow, gen_server2 }
         ,  <<"augment">>     => { act_augment, gen_server2 }
         ,  <<"cancel">>      => { act_cancel, cancel_server }
         ,  <<"contain">>     => { act_contain, contain_server }
         ,  <<"copy">>        => { act_copy, copy_server }
         ,  <<"delay">>       => { act_delay, delay_server }
         ,  <<"delete">>      => { act_delete, delete_server }
         ,  <<"deny">>        => { act_deny, deny_server }
         ,  <<"detonate">>    => { act_detonate, detonate_server }
         ,  <<"distill">>     => { act_distill, distill_server }
         ,  <<"get">>         => { act_get, get_server }
         ,  <<"investigate">> => { act_investigate, investigate_server }
         ,  <<"locate">>      => { act_locate, locate_server }
         ,  <<"mitigate">>    => { act_mitigate, mitigate_server }
         ,  <<"modify">>      => { act_modify, modify_server }
         ,  <<"move">>        => { act_move, move_server }
         ,  <<"notify">>      => { act_notify, notify_server }
         ,  <<"pause">>       => { act_pause, pause_server }
         ,  <<"query">>       => { act_query, query_server }
         ,  <<"redirect">>    => { act_redirect, redirect_server }
         ,  <<"remediate">>   => { act_remediate, remediate_server }
         ,  <<"report">>      => { act_report, report_server }
         ,  <<"response">>    => { act_response, response_server }
         ,  <<"restart">>     => { act_restart, restart_server }
         ,  <<"restore">>     => { act_restore, restore_server }
         ,  <<"resume">>      => { act_resume, resume_server }
         ,  <<"save">>        => { act_save, save_server }
         ,  <<"scan">>        => { act_scan, scan_server }
         ,  <<"set">>         => { act_set, set_server }
         ,  <<"snapshot">>    => { act_snapshot, snapshot_server }
         ,  <<"start">>       => { act_start, start_server }
         ,  <<"stop">>        => { act_stop, stop_server }
         ,  <<"substitute">>  => { act_substitute, substitute_server }
         ,  <<"sync">>        => { act_sync, sync_server }
         ,  <<"throttle">>    => { act_throttle, throttle_server }
         ,  <<"update">>      => { act_update, update_server }
         },

    %% return {true, module/function to run} if valid action,
    %%      otherwise return {false, { undefined, undefined}
    ActionValid = maps:is_key(Action, ValidActions),
    DefaultAction = { undefined, undefined },
    ActionValue = maps:get(Action, ValidActions, DefaultAction),
    { ActionValid, ActionValue }.

spawn_action( ActionModule, ActionServer, Json ) ->
    lager:info( "Got to spawn_action for ~p:~p ", [ ActionModule
                                                  , ActionServer
                                                  ] ),

    %% spin up a process for this command and have it orchestrate
    ActionProcess = spawn(ActionModule, ActionServer, [Json]),

    %% check works by sending keepalive and verifying response
    ActionProcess!{self(), keepalive},
    receive
        %% get the keepalive
        {keepalive_received, ActionServer} ->
            lager:debug( "spawn_action(~p) keepalive from ~p", [ ActionServer
                                                               , ActionProcess
                                                               ] ),
            KeepAliveWorked = true
    after 500 ->   % timeout in 0.5 seconds
        lager:debug( "spawn_action(~p) no keepalive from ~p", [ ActionServer
                                                              , ActionProcess
                                                               ] ),
            KeepAliveWorked = false
    end,

    %% return whether keepalive worked, and spawned process id
    { KeepAliveWorked, ActionProcess}.

%% spawn action servers
spinup( act_allow,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_allow:start(State),

    %% check with keep alive
    ActionKeepAlive = act_allow:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    send_response(Pid, Req, State);

spinup( act_augment,  Req, State ) ->
    %% start gen_server for that action
    Pid = act_augment:start(State),

    %% check with keep alive
    ActionKeepAlive = act_augment:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    send_response(Pid, Req, State);

spinup( _ActionSvr,  Req, State ) ->
    %% no function for this action so reply accordingly
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing action function">>, Req),
    {ok, Req2, State}.

send_response(Pid, Req, State) ->
    %% for now just reply with state as json
    ReplyBody = jsx:encode( State ),

    State2 = maps:put(pid, Pid, State),

    Headers = [ {<<"content-type">>, <<"application/json">>} ],
    {ok, Req2} = cowboy_req:reply(200, Headers, ReplyBody, Req),
    {ok, Req2, State2}.
