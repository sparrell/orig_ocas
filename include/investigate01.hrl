%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-define(INVESTIGATE01, <<"{
\"action\": \"investigate\",
\"target\": { 
    \"type\": \"cybox:Device\",
    \"specifiers\": \"NetworkFirewall\"
    },
\"actuator\": {
    \"type\": \"network-firewall\",
    \"specifiers\": \"fw01\"
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).