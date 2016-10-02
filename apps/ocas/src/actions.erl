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

-export([ get_valid_action/1
        , scan/2
        , locate/2
        , query/2
        , report/2
        , get/2
        , notify/2
        , deny/2
        , contain/2
        , allow/2
        , start/2
        , stop/2
        , restart/2
        , pause/2
        , resume/2
        , cancel/2
        , set/2
        , update/2
        , move/2
        , redirect/2
        , delete/2
        , snapshot/2
        , detonate/2
        , restore/2
        , save/2
        , modify/2
        , throttle/2
        , delay/2
        , substitute/2
        , copy/2
        , sync/2
        , distill/2
        , augment/2
        , investigate/2
        , mitigate/2
        , remediate/2
        , response/2
        , alert/2
        ]).

get_valid_action(Action) ->
    %% this routine verifies action is on list of valid actions
    %% and returns atom which is routine to run.

    %% note it uses a list instead of a simple binary_to_atom
    %% this is intentional to prevent DoS attacks 
    %%   (which would use up atoms by sending many invalid actions)

    ValidActions = 
        #{ <<"scan">> => scan 
        ,  <<"locate">> => locate 
        ,  <<"query">> => query 
        ,  <<"report">> => report 
        ,  <<"get">> => get 
        ,  <<"notify">> => notify 
        ,  <<"deny">> => deny 
        ,  <<"contain">> => contain 
        ,  <<"allow">> => allow 
        ,  <<"start">> => start 
        ,  <<"stop">> => stop 
        ,  <<"restart">> => restart 
        ,  <<"pause">> => pause 
        ,  <<"resume">> => resume 
        ,  <<"cancel">> => cancel 
        ,  <<"set">> => set 
        ,  <<"update">> => update 
        ,  <<"move">> => move 
        ,  <<"redirect">> => redirect 
        ,  <<"delete">> => delete 
        ,  <<"snapshot">> => snapshot 
        ,  <<"detonate">> => detonate 
        ,  <<"restore">> => restore 
        ,  <<"save">> => save 
        ,  <<"modify">> => modify 
        ,  <<"throttle">> => throttle 
        ,  <<"delay">> => delay 
        ,  <<"substitute">> => substitute 
        ,  <<"copy">> => copy 
        ,  <<"sync">> => sync 
        ,  <<"distill">> => distill 
        ,  <<"augment">> => augment 
        ,  <<"investigate">> => investigate 
        ,  <<"mitigate">> => mitigate 
        ,  <<"remediate">> => remediate 
        ,  <<"response">> => response 
        ,  <<"alert">> => alert 
        }, 

    %% return ActionAtom if valid action, otherwise return undefined
    maps:get(Action, ValidActions, undefined).

%% This routine API handles all the actions that can be taken

scan(_Json, _Whatever) ->
    lager:info("GOT TO scan!!!!"),
    ok.

locate(_Json, _Whatever) ->
    lager:info("GOT TO locate!!!!"),
    ok.

query(_Json, _Whatever) ->
    lager:debug("GOT TO query!!!!"),
    ok.

report(_Json, _Whatever) ->
    lager:debug("GOT TO report!!!!"),
    ok.

get(_Json, _Whatever) ->
    lager:debug("GOT TO get!!!!"),
    ok.

notify(_Json, _Whatever) ->
    lager:debug("GOT TO notify!!!!"),
    ok.

deny(_Json, _Whatever) ->
    lager:debug("GOT TO deny!!!!"),
    ok.

contain(_Json, _Whatever) ->
    lager:debug("GOT TO contain!!!!"),
    ok.

allow(_Json, _Whatever) ->
    lager:debug("GOT TO allow!!!!"),
    ok.

start(_Json, _Whatever) ->
    lager:debug("GOT TO start!!!!"),
    ok.

stop(_Json, _Whatever) ->
    lager:debug("GOT TO stop!!!!"),
    ok.

restart(_Json, _Whatever) ->
    lager:debug("GOT TO restart!!!!"),
    ok.

pause(_Json, _Whatever) ->
    lager:debug("GOT TO pause!!!!"),
    ok.

resume(_Json, _Whatever) ->
    lager:debug("GOT TO resume!!!!"),
    ok.

cancel(_Json, _Whatever) ->
    lager:debug("GOT TO cancel!!!!"),
    ok.

set(_Json, _Whatever) ->
    lager:debug("GOT TO set!!!!"),
    ok.

update(_Json, _Whatever) ->
    lager:debug("GOT TO update!!!!"),
    ok.

move(_Json, _Whatever) ->
    lager:debug("GOT TO move!!!!"),
    ok.

redirect(_Json, _Whatever) ->
    lager:debug("GOT TO redirect!!!!"),
    ok.

delete(_Json, _Whatever) ->
    lager:debug("GOT TO delete!!!!"),
    ok.

snapshot(_Json, _Whatever) ->
    lager:debug("GOT TO snapshot!!!!"),
    ok.

detonate(_Json, _Whatever) ->
    lager:debug("GOT TO detonate!!!!"),
    ok.

restore(_Json, _Whatever) ->
    lager:debug("GOT TO restore!!!!"),
    ok.

save(_Json, _Whatever) ->
    lager:debug("GOT TO save!!!!"),
    ok.

modify(_Json, _Whatever) ->
    lager:debug("GOT TO modify!!!!"),
    ok.

throttle(_Json, _Whatever) ->
    lager:debug("GOT TO throttle!!!!"),
    ok.

delay(_Json, _Whatever) ->
    lager:debug("GOT TO delay!!!!"),
    ok.

substitute(_Json, _Whatever) ->
    lager:debug("GOT TO substitute!!!!"),
    ok.

copy(_Json, _Whatever) ->
    lager:debug("GOT TO copy!!!!"),
    ok.

sync(_Json, _Whatever) ->
    lager:debug("GOT TO sync!!!!"),
    ok.

distill(_Json, _Whatever) ->
    lager:debug("GOT TO distill!!!!"),
    ok.

augment(_Json, _Whatever) ->
    lager:debug("GOT TO augment!!!!"),
    ok.

investigate(_Json, _Whatever) ->
    lager:debug("GOT TO investigate!!!!"),
    ok.

mitigate(_Json, _Whatever) ->
    lager:info("Got to mitigate!!!!"),
    %% what happens next? 
    %%     spin up a process for this command and have it orchestrate
    %%     have it spin up a process for target & actuator (or should actuators already be up?)
    ok.  %% should return the process

mitigate_process(_Json) ->
   ok. %% need to put in loop code


remediate(_Json, _Whatever) ->
    lager:debug("GOT TO remediate!!!!"),
    ok.

response(_Json, _Whatever) ->
    lager:debug("GOT TO response!!!!"),
    ok.

alert(_Json, _Whatever) ->
    lager:debug("GOT TO alert!!!!"),
    ok.

