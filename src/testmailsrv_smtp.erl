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
%% @doc Test mail server SMTP handler

-module(testmailsrv_smtp).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").

-behaviour(ranch_protocol).

%% ranch_protocol callbacks
-export([start_link/4, init/4]).


%%====================================================================
%% ranch_protocol callbacks
%%====================================================================

-record(state, {
    mailbox = null,
    recipients = [],
    tail = null,
    data = false,
    tempfile = null
}).


start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.


%% @private
init(Ref, Socket, Transport, _Opts) ->
    ok = ranch:accept_ack(Ref),
    Transport:send(Socket, "220 Test Mail Server ready\r\n"),
    loop(Socket, Transport, #state{}).


%% @private
loop(Socket, Transport, State) ->
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} -> pre_handle(Socket, Transport, State, Data);          
        _          -> Transport:close(Socket)
    end.


%% @private
pre_handle(Socket, Transport, #state{tail = Tail} = State, Data) when is_binary(Tail) ->
    % Concat data to previous tail and continue
    pre_handle(Socket, Transport, State#state{tail = null}, <<Tail/binary, Data/binary>>);


%% @private
pre_handle(Socket, Transport, State, Data) ->
    Parts = binary:split(Data, <<"\r\n">>, [global, trim]),
    Skip = byte_size(Data) - 2,
    {Parts2, State2} = case Data of
        <<_:Skip/binary, "\r\n">> -> {Parts, State};
        _  -> {Parts1, [Tail]} = lists:split(length(Parts) - 1, Parts),
              {Parts1, State#state{tail = Tail}}
    end,
    handle_parts(Socket, Transport, State2, Parts2).


%% @private
handle_parts(Socket, Transport, State, []) ->
    loop(Socket, Transport, State);


%% @private
handle_parts(Socket, Transport, State, [Part | Tail]) ->
    State2 = handle(Socket, Transport, State, Part),
    handle_parts(Socket, Transport, State2, Tail).


%% @private 
%% @doc handle HELO command
handle(Socket, Transport, #state{data = false} = State, <<"HELO",_/binary>>) ->
    Transport:send(Socket, "250 OK\r\n"),
    State;


%% @private 
%% @doc handle EHLO command
handle(Socket, Transport, #state{data = false} = State, <<"EHLO",_/binary>>) ->
    Transport:send(Socket, "250 OK\r\n"),
    State;


%% @private 
%% @doc handle NOOP command
handle(Socket, Transport, #state{data = false} = State, <<"NOOP">>) ->
    Transport:send(Socket, "250 OK\r\n"),
    State;


%% @private 
%% @doc handle MAIL FROM: command
handle(Socket, Transport, #state{data = false, mailbox = null} = State, <<"MAIL FROM:",Mailbox/binary>>) ->
    Transport:send(Socket, "250 OK\r\n"),
    State#state{mailbox = clean_recipient(Mailbox)};


%% @private 
%% @doc handle RCPT TO: command
handle(Socket, Transport, #state{data = false, recipients = Recipients} = State, <<"RCPT TO:",Recipient/binary>>) ->
    Transport:send(Socket, "250 OK\r\n"),
    State#state{recipients = [clean_recipient(Recipient) | Recipients]};


%% @private 
%% @doc handle RSET command
handle(Socket, Transport, #state{data = false, tempfile = TempFile} = State, <<"RSET">> = Data) when TempFile =/= null ->
    file:delete(TempFile),
    handle(Socket, Transport, State#state{tempfile = null}, Data);


%% @private 
%% @doc handle RSET command
handle(Socket, Transport, #state{data = false}, <<"RSET">>) ->
    Transport:send(Socket, "250 OK\r\n"),
    #state{};


%% @private 
%% @doc handle QUIT command
handle(Socket, Transport, #state{data = false, tempfile = TempFile} = State, <<"QUIT">> = Data) when TempFile =/= null ->
    % Place temporary file to recipients's mailboxes
    {ok, _} = testmailsrv_mailboxes:add_message(State#state.mailbox, State#state.recipients, TempFile),
    file:delete(TempFile),
    handle(Socket, Transport, State#state{tempfile = null}, Data);


%% @private 
%% @doc handle QUIT command
handle(Socket, Transport, #state{data = false} = State, <<"QUIT">>) ->
    Transport:send(Socket, "221 Closing connection\r\n"),
    Transport:close(Socket),
    State;


%% @private 
%% @doc handle DATA command
handle(Socket, Transport, #state{data = false, mailbox = null} = State, <<"DATA">>) ->
    Transport:send(Socket, "550 No sender given\r\n"),
    State;


%% @private 
%% @doc handle DATA command
handle(Socket, Transport, #state{data = false, recipients = []} = State, <<"DATA">>) ->
    Transport:send(Socket, "550 No recipients given\r\n"),
    State;


%% @private 
%% @doc handle DATA command
handle(Socket, Transport, #state{data = false, tempfile = TempFile} = State, <<"DATA">>) when TempFile =/= null ->
    Transport:send(Socket, "550 Data already sent\r\n"),
    State;


%% @private 
%% @doc handle DATA command
handle(Socket, Transport, #state{data = false} = State, <<"DATA">>) ->
    {ok, Filename} = testmailsrv_mailboxes:create_tempfile(),
    Transport:send(Socket, "354 Enter mail, end with line containing only \".\"\r\n"),
    State#state{data = true, tempfile = Filename};


%% @private 
%% @doc handle DATA command
handle(_Socket, _Transport, #state{data = true, tempfile = TempFile} = State, <<"..">>) ->
    ok = write_file(TempFile, <<".\r\n">>),
    State;


%% @private 
%% @doc handle DATA command
handle(Socket, Transport, #state{data = true, tempfile = TempFile} = State, <<".">>) ->
    Transport:send(Socket, io_lib:format("250 ~p bytes accepted\r\n", [filelib:file_size(TempFile)])),
    State#state{data = false};


%% @private 
%% @doc handle DATA command
handle(_Socket, _Transport, #state{data = true, tempfile = TempFile} = State, Data) ->
    ok = write_file(TempFile, <<Data/binary, "\r\n">>),
    State;


%% @private 
%% @doc handle invalid command
handle(Socket, Transport, #state{data = false} = State, _Data) ->
    Transport:send(Socket, "500 Error\r\n"),
    State.


%%====================================================================
%% internal functions
%%====================================================================


%% @private 
%% @doc Write or append data to file
write_file(Filename, Data) ->
    case file:read_file_info(Filename) of
        {ok, _FileInfo} -> file:write_file(Filename, Data, [append]);
        {error, enoent} -> file:write_file(Filename, Data)
    end.


%% @private
%% @doc Clean recipient address
clean_recipient(<<" ", Tail/binary>>) ->
    clean_recipient(Tail);

clean_recipient(<<"<", Tail/binary>>) ->
    clean_recipient(Tail);

clean_recipient(Address) ->
    Size = byte_size(Address) - 1,
    case Address of
        <<Recipient:Size/binary, " ">> -> clean_recipient(Recipient);
        <<Recipient:Size/binary, ">">> -> clean_recipient(Recipient);
        _ -> Address
    end.




