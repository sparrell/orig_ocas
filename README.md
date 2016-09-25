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

put stuff here

Examples
-----

put stuff here

Vision
-----
The vision is to have this code running on a cloud server, with a protected interface, that could be used for interworking testing. See TBD for the glue to do this.

Another vision is for this code be forked and 'filled in' as actual working implementations.
See TBD for one example.
put stuff here
