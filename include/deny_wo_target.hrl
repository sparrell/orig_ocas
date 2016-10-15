%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  json for deny
-define(JSON_DENY_WO_TARGET, <<"{
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

