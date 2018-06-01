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
%% @doc Test mail server mailboxes manager

-module(testmailsrv_mailboxes).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").

-behaviour(gen_server).

%% API exports
-export([start_link/0, set_maildir/1, set_tempdir/1, redirect_to_sender/1, 
         create_tempfile/0, add_message/3, lock_mailbox/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%====================================================================
%% API exports
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Set a directory to store user's mailboxes
set_maildir(Dir) ->
    gen_server:call(?MODULE, {set_maildir, Dir}).


%% @doc Set a directory to store temporary files
set_tempdir(Dir) ->
    gen_server:call(?MODULE, {set_tempdir, Dir}).


%% @doc Enable or disable redirecting SMTP messages to sender's mailbox
redirect_to_sender(TrueFalse) ->
    gen_server:call(?MODULE, {redirect_to_sender, TrueFalse}).


%% @doc Set a temporary file
create_tempfile() ->
    gen_server:call(?MODULE, {create_tempfile, self()}).


%% @doc Add a message to the mailbox
add_message(Sender, Recipients, Message) ->
    gen_server:call(?MODULE, {add_message, Sender, Recipients, Message}).


lock_mailbox(Mailbox) ->
    gen_server:call(?MODULE, {lock_mailbox, Mailbox, self()}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

-record(state, {
    maildir,
    tempdir,
    redirect_to_sender = true,
    locks = [],
    tempfiles = #{}
}).

%% @private
init(_Arg) ->
    {ok, Cwd} = file:get_cwd(),
    % Mailboxes directory
    MailDir = filename:join(Cwd, "mailboxes"),
    % Temporary directory
    TempDir = case os:getenv("TEMP") of
        false -> case os:getenv("TMP") of
                     false -> filename:join(Cwd, "temp");
                     Dir1  -> Dir1    
                 end;
        Dir2   -> Dir2    
    end,    
    {ok, #state{maildir = MailDir, tempdir = TempDir}}.


%% @private
%% @doc Set new mailbox directory
handle_call({set_maildir, MailDir}, _From, State) ->
    {reply, ok, State#state{maildir = MailDir}};


%% @private
%% @doc Set new temporary directory
handle_call({set_tempdir, TempDir}, _From, State) ->
    {reply, ok, State#state{tempdir = TempDir}};


%% @private
%% @doc Enable or disable redirecting SMTP messages to sender's mailbox
handle_call({redirect_to_sender, TrueFalse}, _From, State) ->
    {reply, ok, State#state{redirect_to_sender = TrueFalse}};


%% @private
%% @doc Set new temporary directory
handle_call({create_tempfile, Pid}, _From, #state{tempdir = TempDir, tempfiles = Tempfiles} = State) ->
    Filename = filename:join(TempDir, "mail" ++ integer_to_list(erlang:phash2(make_ref())) ++ ".msg"),
    ok = filelib:ensure_dir(Filename),
    Ref = erlang:monitor(process, Pid), % Monitor process to automatically remove temporary file
    {reply, {ok, Filename}, State#state{tempfiles = Tempfiles#{Ref => Filename}}};


%% @private
%% @doc Add message to user's mailboxes
handle_call({add_message, Sender, Recipients, Message}, _From, #state{maildir = MailDir} = State) ->
    Mailboxes = case State#state.redirect_to_sender of
        true -> [Sender];
        _    -> Recipients
    end,
    MSecs = erlang:system_time(millisecond),
    Filename = lists:flatten(io_lib:format("~p.msg",[MSecs])),
    Filenames = [filename:join([MailDir, Mailbox, Filename]) || Mailbox <- Mailboxes],
    % Copy temporary file to user's mailboxes
    lists:foreach(fun(Dest) -> ok = filelib:ensure_dir(Dest), {ok, _} = file:copy(Message, Dest) end, Filenames),
    {reply, {ok, Filenames}, State};


%% @private
%% @doc Lock a user's mailbox
handle_call({lock_mailbox, Mailbox, Pid}, _From, #state{maildir = MailDir, locks = Locks} = State) ->
    case proplists:lookup(Mailbox, Locks) of
        none -> Files = filelib:wildcard(binary_to_list(filename:join([MailDir, Mailbox, "*.msg"]))), 
                Messages = [{filename:basename(File), File} || File <- Files],
                Ref = erlang:monitor(process, Pid), % Monitor process to automatically unlock mailbox
                {reply, {ok, Messages}, State#state{locks = Locks ++ [{Mailbox, Ref}]}};
        _    -> {reply, {error, locked}, State}    
    end;    


%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.


%% @private
handle_cast(_Request, State) ->
    {noreply, State}.


%% @private
handle_info({'DOWN', Ref, _Type, _Object, _Info}, #state{locks = Locks, tempfiles = Tempfiles} = State) ->
    % Automatically unlock mailbox
    Locks2 = [Lock || {_M, _Ref} = Lock <- Locks, Ref =/= _Ref],
    % Remove temporary file if exists
    Tempfiles2 = case maps:find(Ref, Tempfiles) of
        {ok, Filename} -> 
            file:delete(Filename),
            maps:remove(Ref, Tempfiles);
        _ -> 
            Tempfiles    
    end,
    {noreply, State#state{locks = Locks2, tempfiles = Tempfiles2}};


%% @private
handle_info(_Info, State) ->
    {noreply, State}.


%% @private
terminate(_Reason, _State) ->
    ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
