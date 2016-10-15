%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  json for deny
-define(JSON_DENY_01, <<"{
\"action\": \"deny\",
\"target\": {
    \"type\": \"cybox:Network_Connection\",
    \"specifiers\": {
        \"Layer4Protocol\": \"UDP\",
        \"DestinationSocketAddress\": {
            \"IP_Address\": {\"Address_Value\": \"1.2.3.4\"},
            \"Port\": {\"Port_Value\": 443}
            }
        }
    },
\"actuator\": {
    \"type\": \"network-router\",
    \"specifiers\": {\"port\": \"2\"}
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).

