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
%% @doc Test mail server application

-module(testmailsrv_app).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = testmailsrv_sup:start_link(),
    %% Set up mailboxes directory
    case init:get_argument(maildir) of
        {ok, [[MailDir]]} -> testmailsrv_mailboxes:set_maildir(MailDir);
        _ -> case os:getenv("MAILDIR") of
                false   -> ok;
                MailDir -> testmailsrv_mailboxes:set_maildir(MailDir)
             end
    end,
    %% Set up directory for temporary files
    case init:get_argument(tempdir) of
        {ok, [[TempDir]]} -> testmailsrv_mailboxes:set_tempdir(TempDir);
        _ -> case os:getenv("TEMP") of
                false   -> case os:getenv("TMP") of
                               false   -> ok;
                               TempDir -> testmailsrv_mailboxes:set_tempdir(TempDir)
                           end;
                TempDir -> testmailsrv_mailboxes:set_tempdir(TempDir)
             end
    end,    
    %% Set up SMTP mail redirecting
    case init:get_argument('redirect-to') of
        {ok, [[Redirect]]} -> case Redirect of
            "sender"  -> testmailsrv_mailboxes:set_redirect(sender);
            "none"    -> testmailsrv_mailboxes:set_redirect(none);
            _         -> testmailsrv_mailboxes:set_redirect(Redirect)
        end;
        _ -> ok    
    end,
    %% Start the SMTP server if -smtp parameter is given
    case init:get_argument(smtp) of
        {ok, [[SmtpPort]]} -> 
            {SmtpNum, _} = string:to_integer(SmtpPort),
            testmailsrv:start_smtp(SmtpNum);
        _ -> ok
    end,
    %% Start the POP3 server if -smtp parameter is given
    case init:get_argument(pop3) of
        {ok, [[Pop3Port]]} -> 
            {Pop3Num, _} = string:to_integer(Pop3Port),
            testmailsrv:start_pop3(Pop3Num);
        _ -> ok
    end,
    {ok, Pid}.


stop(_State) ->
    ok.

    