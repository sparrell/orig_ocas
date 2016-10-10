To understand the code, here are some scenarios showing what modules/functions are used.

ocas_app starts a cowboy webserver with ranch listeners.

routes are established within cowboy so that when 
http requests contain the following paths
then the following handlers are invoked:
- status    status_handler.erl
- ok        status_ok_handler.erl
- openc2    openc2_handler.erl

## status_ok_handler.erl
status_ok_handler.erl is a simple routing that only excepts GET, 
ignores any parameters,
and returns "ok".
This is used as a keepalive so you know the simulator is accepting requests.


## openc2_handler.erl

The sunny day path thru openc2_handler is documented in the following example.
For this example, it is assumed a POST to /openc2 was sent with valid JSON containg a valid SCAN action eg:

```erlang
{ "action": "scan",
              "target": { "type": "cybox:Device",
                          "specifiers": "NetworkScanner"
                         },
               "actuator": {
                  "type": "network-scanner",
                  "specifiers": "scanner01"
                           },
               "modifiers": { "response": "ack",
                              "where": "perimeter"}
             }
```


The flow is as follows:

1. init/3 initializes the handler to use REST (and therefore rest_init/2 is called
2. rest_init/2 just logs some info and sets the State variable to empty
3. cowboy calls allowed_methods/2 which passes since since a POST was sent
4. cowboy calls conten_types_accepted/2 which passes control to handle_json 
since the json header was sent
5. handle_json/2 checks if the request contains a body (it does) and tail recurses to body_check/3 with the first parameter true
6. body_check(true,...) checks if body is json (it is) and tail recurses to is_body_json/3 with the first parameter true
7. is_body_json(true,...) decodes the JSON into erlang terms (and stores them in State) and then checks if action is in the JSON (it is) and tail recurses to has_action/3 with first parameter true
8. has_action(true,...) pulls the action value from the json map in State and runs actions:is_valid_action/1 with the action value
9. In this example, the action was 'scan' which is a valid action so actions:is_valid_action returns {true, scan_server} ie that scan is a valid action and that the scan_server should be started.
10. has_action tail recurses to check_valid_action/4 with (true, scan_server, ...)
11. check_valid_action(true,...) checks for target/actuator/modifiers and stores in state and then spins up action process
12. check_valid_action should tail recurse to next step but since next step isn't coded yet, instead it tail recurses to last step to send reply. Ie it tail recurses tosend_reply/2
13. send_reply turns State into json and replies using it as the body (stop gap until a. the rest of the code is completed and b. openc2 figures out what the response should be)

above is all independent of cybox part at this point (ie cybox stuff is yet to be coded)

