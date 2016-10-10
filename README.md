ocas
=====

ocas - plural of oca, a edible tuber of Oxalis Tuberosa, a wood sorrel of the Andes; 
or maybe it's an acronym OCAS - OpenC2 API Simulator.

Ocas is an OTP application written in erlang to:
- prove out the OpenC2 spec (see http://openc2.org/ and https://github.com/OpenC2-org/)
- be a viable simulator for testing OpenC2 code and scenarios
- be a template for developing actual openc2 applications (ie replace the simulator code with code to actually perform the real function)

License
-----
Copyright 2016 sFractal Consulting

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Build
-----

    $ rebar3 compile

Build & Test
-----

    $ rebar3 ct

Vision
-----
The vision is to have this code running on a cloud server, with a protected interface, that could be used for interworking testing. See TBD for the glue to do this.

### Reference Implementation and Specification Validation
A primary purpose (and first use) of the simulator is to uncover bugaboos in the openC2 specification. One such has already been uncovered (the mixed case issue mentioned earlier). The openC2 specification is an attempt to simplify and standardize the command and control of all aspects of security. Alternative implementations, particularly if they can interoperate, help uncover discrepancies and different interpretations allowing the spec to be improved. Having ocas and the python reference implementation interact will go a long way towards validating the openC2 specification.

### One-time Single-command Simulator
The first phase of software development will focus on getting the simulator up and running and accepting a single command. The software is being architected to both allow for easily scaling to a full network simulation and for using as the base for either an orchestrator or an actuator.

### Playbook Simulator
Once the one-time single-command simulator is fully functional, it will be extended to multi-command to allow playbook simulation of a full network. This will involve retaining state information which will be then extended to allow multiple orchestrators and the study of race conditions and temporal vulnerabilities.

### Implementation Template
Another vision is for this code be forked and 'filled in' as actual working implementations.
The software has been architected to serve as a base for production security software as either an orchestrator or an actuator.
See TBD for one example.



Organization of this software
-----

The directory structure is follows the erlang OTP convention. Of interest:
- ./rebar.config contains the dependencies for building
  * cowboy is used for the webserver
  * jsx is used for json creation/parsing
  * lager is used for logging
  * shotgun is used just in testing as an http test client
- ./test contains tests for ct to execute. A test-driven-development is being attempted (eg first write a test, have it fail, and then develop the code to get it to pass, iterate).
- ./apps/ocas/src contains the meat 
  * ocas.app.src - OTP application resource file
  * ocas_sup.erl - OTP supervisor
  * ocas_app.erl - API for ocas

The cowboy webserver is used.

The start/2 module in ocas_app.erl is the callback run when the webserver is started. This module calls start_webserver() which contains the ocas specific software including a set of compiled routes which map url’s to the handler callbacks to run:
- /status will run status_hander callbacks 
   * for admin to find status
- /ok will run status_ok_handler callbacks
   * keepalive that just returns ok when all is ok
- /openc2 will run openc2_handler callbacks
   * receives the openc2 json, validates it, and executes what is in the openc2 command in the simulator

### status_handler 
This is a future feature to allow those with administrative accessto get status information about the simulator itself (as opposed to about the network being simulated which would use openC2 commands). At the current point this api takes no parameters and returns the html for “Status Works - needs more later”.

### status_ok_handler
This api returns a simple “ok” in either text, html, or json. This is to serve as a keepalive is one is needed.

### openc2_handler
This module is the heart of the simulator. When the url = /openc2 then the openc2_handler is used. It contains the following for it’s API:
-	rest_init/2 - to tell cowboy this is a REST API
-	allowed_methods/2 – to tell cowboy to only allow the POST method
-	content_types_accepted/2 – to tell cowboy that only JSON is allowed, and to pass control to handle_json/2 when json is posted on this url 
-	handle_json/2 (and it’s helper routines) :
o	verifies there is a body in the http request
o	decodes and verifies the json
o	spawns the necessary processes for action/target/actuator/modifier (more on this further down)
o	does the simulation
o	sends the appropriate http response 

The openC2 language is in JSON and consists of 
two mandatory top-level fields (action, target) 
and two optional top-level fields (actuator, modifiers). 
The nature of the openc2 command set poses some interesting challenges 
in how to organize the software 
(see add ref to MulitMode talk on 29-Sep-2016 at OpenC2 face2face). 
Ocas is organized taking advantage of erlang’s concurrency, small lightweight processes, and messaging. 
This allows the action/target/actuator/modifier code 
to remain independent of each other. 
For example, if a new actuator is added, it should not require changes to the actions software.

This is accomplished by spinning up erlang processes for the particular action (eg deny), 
target(eg network connection), actuator (eg network firewall), 
and modifier (eg acknowledge). 
These processes interact via messages to accomplish the desired simulation result.

Although this may be overkill in these first phases of the project 
(specification validation and one-time, single-command simulator); 
it allows the same code to scale up for the full network simulation 
including initializing the network to be simulated, playbook simulation, 
multiple concurrent orchestrators 
(eg looking for race conditions and time-dependent security vulnerabilities), 
and pen testing via simulation.

The establishment of a separate erlang process for each entity 
and each action allows the simulator to scale both the scope (processes) 
and the context (messaging) of the network being simulated 
in addition to the modularizing the software that was mentioned earlier.

###Actions
The Actions module contains get_valid_action/1 
which both verifies the request’s json action is valid, 
and it spawns the process for that action. The spawned process 
runs the function for that action in the actions module 
(eg deny in the json action field spawns the process running the deny_server/1 function. 
As of this report, skeleton code exists for all 37 actions defined in the openC2 specification 
to at least verify a valid action. 
For two of the actions, deny and mitigate, 
the processes are actually spun up (albeit they only do a simple keepalive).

###More on software design
See README.md in apps/ocas/src for more on the software design
including a sunny day walk thru the modules/functions.


Examples
-----

put stuff here

