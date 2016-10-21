%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%% json for bad scan
%%   Scan requires a target and a target - leave off target and it should fail
-define(SCANWOTARGET, <<"{
\"action\": \"scan\",
\"actuator\": {
    \"type\": \"network-scanner\",
    \"specifiers\": \"scanner01\"
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).
