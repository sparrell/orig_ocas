%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  illegal action in json
-define(NONSENSE, <<"{
\"action\": \"nonsense\",
\"target\": {
    \"type\":\"cybox:Hostname\",
    \"specifiers\":{\"Hostname_Value\":\"cdn.badco.org\"}
    }
}">>).

