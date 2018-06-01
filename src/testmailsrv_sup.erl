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
%% @doc Test mail server top level supervisor

-module(testmailsrv_sup).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").

-behaviour(supervisor).

%% API exports
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API exports
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        {testmailsrv_mailboxes, {testmailsrv_mailboxes, start_link, []}, permanent, 5000, worker, dynamic}
    ]}}.


