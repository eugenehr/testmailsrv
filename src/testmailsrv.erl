%% -*- coding: utf-8 -*-
%%
%% Copyright (c) Eugene Khrustalev 2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%% @author Eugene Khrustalev <eugene.khrustalev@gmail.com>
%% @doc Test mail server API module

-module(testmailsrv).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").


%% API exports
-export([start/0, stop/0, start_smtp/1, stop_smtp/0, is_smtp_started/0, start_pop3/1, stop_pop3/0, is_pop3_started/0]).


%%====================================================================
%% API exports
%%====================================================================

%% @doc Start the testmailsrv application and all its dependecies
start() ->
    application:ensure_all_started(testmailsrv).


%% @doc Stop the testmailsrv application
stop() ->
    application:stop(testmailsrv).


%% @doc Start the SMTP server in the given port
start_smtp(Port) when is_integer(Port) ->
    case is_smtp_started() of 
        true -> stop_smtp();
        _    -> ok 
    end,
    {ok, _} = ranch:start_listener(smtp, ranch_tcp, [{port, Port}, {num_acceptors, 3}], testmailsrv_smtp, []).


%% @doc Stop the STMP server
stop_smtp() ->
    case is_smtp_started() of 
        true -> ranch:stop_listener(smtp);
        _    -> ok
    end.    


%% @doc Test if the SMTP server is started
is_smtp_started() ->
    case proplists:lookup(smtp, ranch:info()) of
        none -> false;
        _    -> true 
    end.    


%% @doc Start the POP3 server in the given port
start_pop3(Port) when is_integer(Port) ->
    case is_pop3_started() of
        true -> stop_pop3();
        _    -> ok
    end,
    {ok, _} = ranch:start_listener(pop3, ranch_tcp, [{port, Port}, {num_acceptors, 3}], testmailsrv_pop3, []).


%% @doc Stop the POP3 server
stop_pop3() ->
    case is_pop3_started() of 
        true -> ranch:stop_listener(pop3);
        _    -> ok
    end.    


%% @doc Test if the POP3 server is started
is_pop3_started() ->
    case proplists:lookup(pop3, ranch:info()) of
        none -> false;
        _    -> true 
    end.    
