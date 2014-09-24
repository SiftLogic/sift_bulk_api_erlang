Erlang FTP API
===============

This demonstrates how to connect to the ftp server with Erlang. You will need to know your apikey and password which can be found in the UI: API Keys -\> Manage -\> Actions -\> Access Details. Once you
have that you can try the main.js file for an upload demo. For example:
<!-- <pre>
  <code>
    ./main.js -f test.csv -l /tmp -u aUsername -p e261742d-fe2f-4569-95e6-312689d04903 --poll 10
  </code>
</pre>
The CLI is described in more detail with <code>./main.js</code> -->

It is recommended to require the operations file and use the functions in there to customize your process. The functions are described in file. If you want to cut down on code, this file only requires...

Licensing
=========

Copyright 2014 SiftLogic LLC

SiftLogic LLC hereby grants to SiftLogic Customers a revocable, non-exclusive, non-transferable, limited license for use of SiftLogic sample code for the sole purpose of integration with the SiftLogic platform.

Installation
============

Make sure Erlang \>= <b>R15B1</b> is installed, then compile and generate: 
<pre>
  <code>
    ./rebar get-deps clean compile generate
  </code>
</pre>

and run:

<pre>
  <code>
    ./rel/siftbulk/bin/siftbulk console
  </code>
</pre>

and to test:

<pre>
  <code>
    ./rebar compile eunit skip_deps=true
  </code>
</pre>

Configuration
=============

Default application configuration parameters can be modified in sys.config under siftbulk.

Files And Folders
=================

This project uses the [standard application layout structure](http://www.erlang.org/doc/design_principles/applications.html). Non standard files + directories.

* **apps:** Standard structure of the applications for this project (just siftbulk).
 * **apps/siftbulk/priv/test.csv:** A small sample records file.
* **deps:** Any dependencies.
* **rel:** Rebar release will be generated here, see rebar.config.
* **rebar:** Rebar binary.
* **rebar.config:** Description of how Rebar should generate things.
* **sys.config:** Configuration for the application, default connection options are set here.
* **test.csv:** A small sample records file.