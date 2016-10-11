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
        ]).

is_valid_action(Action) ->
    %% this routine verifies action is on list of valid actions
    %% and returns atom which is routine to run.

    %% note it uses a list instead of a simple binary_to_atom
    %% this is intentional to prevent DoS attacks
    %%   (which would use up atoms by sending many invalid actions)

    ValidActions =
        #{ <<"alert">>    => { act_alert, alert_server}
         , <<"deny">>     => { act_deny, deny_server }
         , <<"mitigate">> => { act_mitigate, mitigate_server }
         , <<"scan">>     => { act_scan, scan_server }
         ,  <<"allow">>   => allow
         ,  <<"augment">> => augment
         ,  <<"cancel">>  => cancel
         ,  <<"contain">> => contain
         ,  <<"copy">> => copy
         ,  <<"delay">> => delay
         ,  <<"delete">> => delete
         ,  <<"detonate">> => detonate
         ,  <<"distill">> => distill
         ,  <<"get">> => get
         ,  <<"investigate">> => investigate
         ,  <<"locate">> => locate
         ,  <<"modify">> => modify
         ,  <<"move">> => move
         ,  <<"notify">> => notify
         ,  <<"pause">> => pause
         ,  <<"query">> => query
         ,  <<"redirect">> => redirect
         ,  <<"remediate">> => remediate
         ,  <<"report">> => report
         ,  <<"response">> => response
         ,  <<"restart">> => restart
         ,  <<"restore">> => restore
         ,  <<"resume">> => resume
         ,  <<"save">> => save
         ,  <<"set">> => set
         ,  <<"snapshot">> => snapshot
         ,  <<"start">> => start
         ,  <<"stop">> => stop
         ,  <<"substitute">> => substitute
         ,  <<"sync">> => sync
         ,  <<"throttle">> => throttle
         ,  <<"update">> => update
         },

    %% return {true, module/function to run} if valid action,
    %%      otherwise return {false, { undefined, undefined}
    ActionValid = maps:is_key(Action, ValidActions),
    DefaultAction = { undefined, undefined },
    ActionValue = maps:get(Action, ValidActions, DefaultAction),
    { ActionValid, ActionValue }.

spawn_action( ActionModule, ActionServer, Json ) ->
    lager:info( "Got to spawn_action for ~p:~p ", [ ActionModule, ActionServer ] ),

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


