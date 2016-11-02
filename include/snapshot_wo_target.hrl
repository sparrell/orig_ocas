%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-define(SNAPSHOTWOTARGET, <<"{
\"action\": \"snapshot\",
\"actuator\": {
    \"type\": \"network-firewall\",
    \"specifiers\": \"fw01\"
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).