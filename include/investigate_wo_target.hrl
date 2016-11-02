%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-define(INVESTIGATEWOTARGET, <<"{
\"action\": \"investigate\",
\"actuator\": {
    \"type\": \"network-firewall\",
    \"specifiers\": \"fw01\"
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).