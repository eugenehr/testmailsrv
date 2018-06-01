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
%% @doc Test mail server POP3 handler

-module(testmailsrv_pop3).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").

-behaviour(ranch_protocol).

%% ranch_protocol callbacks
-export([start_link/4, init/4]).


%%====================================================================
%% ranch_protocol callbacks
%%====================================================================

-record(state, {
    mailbox  = null,
    messages = [],
    deleted  = []
}).


start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.


%% @private
init(Ref, Socket, Transport, _Opts) ->
    ok = ranch:accept_ack(Ref),
    Transport:send(Socket, <<"+OK Test Mail Server ready\r\n">>),
    loop(Socket, Transport, #state{}).


%% @private
loop(Socket, Transport, State) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} -> State2 = handle(Socket, Transport, State, Data), 
                      loop(Socket, Transport, State2);          
        _Other     -> Transport:close(Socket)
    end.


%% @private 
%% @doc handle CAPA command
handle(Socket, Transport, State, <<"CAPA\r\n">>) ->
    Transport:send(Socket, "+OK\r\nUIDL\r\n.\r\n"),
    State;


%% @private 
%% @doc handle NOOP command
handle(Socket, Transport, State, <<"NOOP\r\n">>) ->
    Transport:send(Socket, "+OK\r\n"),
    State;


%% @private 
%% @doc handle QUIT command
handle(Socket, Transport, #state{deleted = []} = State, <<"QUIT\r\n">>) ->
    Transport:send(Socket, "+OK\r\n"),
    Transport:close(Socket),
    State;


%% @private 
%% @doc handle QUIT command
handle(Socket, Transport, #state{deleted = Deleted} = State, <<"QUIT\r\n">> = Data) ->
    % delete files
    [file:delete(File) || {_Msg, File} <- Deleted],
    handle(Socket, Transport, State#state{deleted = []}, Data);


%% @private 
%% @doc handle USER command
handle(Socket, Transport, #state{mailbox = Mailbox} = State, <<"USER ",_/binary>>) when Mailbox =/= null ->
    Transport:send(Socket, <<"-ERR User already given\r\n">>),
    State;


%% @private 
%% @doc handle USER command
handle(Socket, Transport, State, <<"USER ",Mailbox/binary>>) ->
    Transport:send(Socket, <<"+OK\r\n">>),
    Mbox = clean_recipient(Mailbox),
    case testmailsrv_mailboxes:lock_mailbox(Mbox) of
        {ok, Messages}  -> State#state{mailbox = Mbox, messages = Messages};
        {error, locked} -> Transport:send(Socket, "-ERR Mailbox already locked\r\n"), State
    end;    


%% @private 
%% @doc handle APOP command
handle(Socket, Transport, #state{mailbox = Mailbox} = State, <<"APOP ",_/binary>>) when Mailbox =/= null ->
    Transport:send(Socket, <<"-ERR User already given\r\n">>),
    State;


%% @private 
%% @doc handle APOP command
handle(Socket, Transport, State, <<"APOP ",Mailbox/binary>>) ->
    Transport:send(Socket, <<"+OK\r\n">>),
    Mbox = clean_recipient(Mailbox),
    case testmailsrv_mailboxes:lock_mailbox(Mbox) of
        {ok, Messages}  -> State#state{mailbox = Mbox, messages = Messages};
        {error, locked} -> Transport:send(Socket, "-ERR Mailbox already locked\r\n"), State
    end;    


%% @private 
%% @doc handle PASS command
handle(Socket, Transport, #state{mailbox = null} = State, <<"PASS ",_/binary>>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle PASS command
handle(Socket, Transport, State, <<"PASS ",_/binary>>) ->
    Transport:send(Socket, <<"+OK\r\n">>),
    State;


%% @private 
%% @doc handle RSET command
handle(Socket, Transport, State, <<"RSET\r\n">>) ->
    Transport:send(Socket, <<"+OK\r\n">>),
    State#state{deleted = []};


%% @private 
%% @doc handle LIST command
handle(Socket, Transport, #state{mailbox = null} = State, <<"LIST\r\n">>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle LIST command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"LIST\r\n">>) ->
    Transport:send(Socket, <<"+OK\r\n">>),
    lists:foldl(fun({Msg, File}, Counter) -> 
        case proplists:lookup(Msg, Deleted) of
            none -> C = list_to_binary(integer_to_list(Counter)),
                    S = list_to_binary(integer_to_list(filelib:file_size(File))),
                    Transport:send(Socket, <<C/binary, " ", S/binary, "\r\n">>);
            _    -> ok
        end,     
        Counter + 1 
    end, 1, Messages),
    Transport:send(Socket, <<".\r\n">>),
    State;


%% @private 
%% @doc handle LIST command
handle(Socket, Transport, #state{mailbox = null} = State, <<"STAT\r\n">>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle LIST command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"STAT\r\n">>) ->
    {C, S} = lists:foldl(fun({Msg, File}, {Count, Size}) -> 
        case proplists:lookup(Msg, Deleted) of
            none -> {Count + 1, Size + filelib:file_size(File)};
            _    -> {Count, Size}
        end
    end, {0, 0}, Messages),
    CB = list_to_binary(integer_to_list(C)),
    SB = list_to_binary(integer_to_list(S)),
    Transport:send(Socket, <<"+OK ", CB/binary, " ", SB/binary, "\r\n">>),
    State;


%% @private 
%% @doc handle DELE command
handle(Socket, Transport, #state{mailbox = null} = State, <<"DELE ", _/binary>>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle DELE command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"DELE ", B/binary>>) ->
    case get_message(Messages, Deleted, B) of
        {ok, Msg, File} -> 
            Transport:send(Socket, "+OK\r\n"),
            State#state{deleted = Deleted ++ [{Msg, File}]}; 
        {error, deleted} -> 
            Transport:send(Socket, "-ERR Already deleted\r\n"),
            State
    end;


%% @private 
%% @doc handle RETR command
handle(Socket, Transport, #state{mailbox = null} = State, <<"RETR ", _/binary>>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle RETR command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"RETR ", B/binary>>) ->
    case get_message(Messages, Deleted, B) of
        {ok, _Msg, File} -> 
            Size = list_to_binary(integer_to_list(filelib:file_size(File))),
            Transport:send(Socket, <<"+OK ", Size/binary, "\r\n">>),
            Transport:sendfile(Socket, File), 
            Transport:send(Socket, <<".\r\n">>),
            State;              
        {error, out_of_range} -> 
            Transport:send(Socket, "-ERR Message number out of range\r\n"),
            State;
        {error, deleted} -> 
            Transport:send(Socket, "-ERR Deleted\r\n"),
            State
    end;


%% @private 
%% @doc handle TOP command
handle(Socket, Transport, #state{mailbox = null} = State, <<"TOP ", _/binary>>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle TOP command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"TOP ", B/binary>>) ->
    [Index, Count] = binary:split(B, <<" ">>),
    case get_message(Messages, Deleted, Index) of
        {ok, _Msg, File} -> 
            Transport:send(Socket, <<"+OK\r\n">>),
            {ok, Data} = file:read_file(File),
            AllLines = binary:split(Data, <<"\r\n">>, [global, trim]),
            % Split file to headers and rows
            {Headers, _Lines} = lists:splitwith(fun (<<"">>) -> false; (_) -> true end, AllLines),
            % Send headers
            lists:foreach(fun(H) -> Transport:send(Socket, <<H/binary, "\r\n">>) end, Headers),
            % Send lines
            Num = min(list_to_integer(binary_to_list(trim(Count))), length(_Lines) - 1),
            {Lines, _Rest} = lists:split(Num + 1, _Lines),
            lists:foreach(fun(L) -> Transport:send(Socket, <<L/binary, "\r\n">>) end, Lines),
            Transport:send(Socket, <<".\r\n">>),
            State;              
        {error, out_of_range} -> 
            Transport:send(Socket, "-ERR Message number out of range\r\n"),
            State;
        {error, deleted} -> 
            Transport:send(Socket, "-ERR Deleted\r\n"),
            State
    end;


%% @private 
%% @doc handle UIDL command
handle(Socket, Transport, #state{mailbox = null} = State, <<"UIDL", _/binary>>) ->
    Transport:send(Socket, <<"-ERR No user given\r\n">>),
    State;


%% @private 
%% @doc handle UIDL command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"UIDL\r\n">>) ->
    Transport:send(Socket, "+OK\r\n"),
    lists:foldl(fun({Msg, _File}, Counter) -> 
        case proplists:lookup(Msg, Deleted) of
            none -> C = list_to_binary(integer_to_list(Counter)),
                    S = list_to_binary(Msg),
                    Transport:send(Socket, <<C/binary, " ", S/binary, "\r\n">>);
            _    -> ok
        end,     
        Counter + 1 
    end, 1, Messages),
    Transport:send(Socket, <<".\r\n">>),
    State;


%% @private 
%% @doc handle UIDL command
handle(Socket, Transport, #state{messages = Messages, deleted = Deleted} = State, <<"UIDL ", Index/binary>>) ->
    case get_message(Messages, Deleted, Index) of
        {ok, Msg, _File} -> 
            I = trim(Index),
            M = list_to_binary(Msg),
            Transport:send(Socket, <<"+OK\r\n", I/binary, " ", M/binary, "\r\n.\r\n">>),
            State;              
        {error, out_of_range} -> 
            Transport:send(Socket, "-ERR Message number out of range\r\n"),
            State;
        {error, deleted} -> 
            Transport:send(Socket, "-ERR Deleted\r\n"),
            State
    end,
    State;


%% @private 
%% @doc handle invalid command
handle(Socket, Transport, State, _Data) ->
    Transport:send(Socket, "-ERR Unknown command\r\n"),
    State.


%%====================================================================
%% internal functions
%%====================================================================


%% @private
%% @doc Clean recipient address
clean_recipient(<<" ", Tail/binary>>) ->
    clean_recipient(Tail);

clean_recipient(<<"<", Tail/binary>>) ->
    clean_recipient(Tail);

clean_recipient(Address) ->
    Size = byte_size(Address) - 1,
    case Address of
        <<Recipient:Size/binary, " ">>  -> clean_recipient(Recipient);
        <<Recipient:Size/binary, ">">>  -> clean_recipient(Recipient);
        <<Recipient:Size/binary, "\r">> -> clean_recipient(Recipient);
        <<Recipient:Size/binary, "\n">> -> clean_recipient(Recipient);
        _ -> Address
    end.


%% @private
%% @doc Clean recipient address
trim(List) when is_list(List) ->
    binary_to_list(trim(list_to_binary(List)));

trim(<<" ", Tail/binary>>) ->
    trim(Tail);

trim(<<"\r", Tail/binary>>) ->
    trim(Tail);

trim(<<"\n", Tail/binary>>) ->
    trim(Tail);

trim(Bin) ->
    Skip = byte_size(Bin) - 1,
    case Bin of
        <<Head:Skip/binary, " ">> -> trim(Head);
        <<Head:Skip/binary, "\r">> -> trim(Head);
        <<Head:Skip/binary, "\n">> -> trim(Head);
        Bin -> Bin
    end.


%% @private
%% @doc Get undeleted message by index
get_message(Messages, Deleted, Index) ->
    Num = list_to_integer(binary_to_list(trim(Index))),
    case Num =< length(Messages) of
        true -> 
            M = lists:nth(Num, Messages),
            {Msg, File} = M,
            case proplists:lookup(Msg, Deleted) of
                none -> {ok, Msg, File};              
                _    -> {error, deleted}
            end;
        _ -> {error, out_of_range}
    end.
