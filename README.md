testmailsrv
=====

An Erlang/OTP&trade; based SMTP/POP3 server for testing purposes.

Requirements
-----

You need an [Erlang&trade; 19 or newer](https://www.erlang.org/) installed on your machine.

Build
-----

    $ rebar3 release


Command line arguments
-----

| Argument                                             | Description
|------------------------------------------------------|------------------
| -smtp **&lt;port&gt;**                               | Start the SMTP server on the given port
| -pop3 **&lt;port&gt;**                               | Start the POP3 server on the given port
| -maildir  **&lt;path&gt;**                           | Path to directory to store user's mailboxes. Setting **%MAILDIR%** environment variable also enabled instead. 
| -tempdir  **&lt;path&gt;**                           | Path to directory to store temporary files. Setting **%TEMP%** or **%TMP%** environment variables also enabled instead. 
| -redirect-to-sender **&lt;true\|false\|yes\|no&gt;** | Enable or disable redirecting incoming messages to the sender's mailbox. **Enabled by default**.      

Java version
-----

A [Java&trade;-based version with GUI](https://github.com/eugenehr/test-mail-server) is also available.

Licensing mumbo-jumbo
-----

This software is licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).