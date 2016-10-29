%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  json for deny
-define(DENYWOTARGET, <<"{
\"action\": \"deny\",
\"actuator\": {
    \"type\": \"network-router\",
    \"specifiers\": {\"port\": \"2\"}
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).

