%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  json for scan
-define(SCAN01, <<"{
\"action\": \"scan\",
\"target\": { 
    \"type\": \"cybox:Device\",
    \"specifiers\": \"NetworkScanner\"
    },
\"actuator\": {
    \"type\": \"network-scanner\",
    \"specifiers\": \"scanner01\"
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).
