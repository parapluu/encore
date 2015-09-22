%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.      
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.                                
%%
%% %CopyrightEnd%

%%%-------------------------------------------------------------------
%%% File    : big.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : A simple message passing benchmark
%%%
%%% Created : 30 Dec 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
%%% Modified by Mikael Östlund 
%%%-------------------------------------------------------------------
-module(big).

-export([run/1, main/1]).

start([]) ->
    ok;
start([P|Procs]) ->
    P ! {pong},
    start(Procs).

run_aux(WorkerCount, MessageAmount, ChunkSize, Chunks, Counter, ProcsAcc) when Counter < Chunks + 1 ->
    ArraySize = case (ChunkSize * (Counter + 1) > WorkerCount) of
                    true -> case (WorkerCount > ChunkSize) of
                                true -> ChunkSize + WorkerCount rem ChunkSize;
                                false -> WorkerCount
                            end;
                    false -> ChunkSize
                end,
    Procs = spawn_procs(ArraySize, MessageAmount),
    ProcsArr = array:from_list(Procs),
    send_procs(Procs, {procs, self(), ProcsArr}),
    start(Procs),
    run_aux(WorkerCount, MessageAmount, ChunkSize, Chunks, Counter+1, [Procs|ProcsAcc]);
run_aux(_, _, _, _, _, ProcsAcc) ->
    ProcsAcc.

run(N) ->
    WorkerCount = N,
    MessageAmount = 16000,
    ChunkSize = 1000,
    Counter = 1,
    Chunks = case (WorkerCount >= ChunkSize) of
                 true -> WorkerCount div ChunkSize;
                 false -> 1
             end,
    ProcsList = run_aux(WorkerCount, MessageAmount, ChunkSize, Chunks, Counter, []),
    receive_msgs(WorkerCount),
    [lists:foreach(fun (P) -> P ! die end, Procs)  || Procs <- ProcsList],
    ok.

random(Low, High) ->
    Low+random:uniform(High-Low).

pinger([], true, Limit, 0) ->
    receive
	{procs, ReportTo, ProcsArr} -> 
	    pinger(ProcsArr, ReportTo, Limit, 0)
    end;
pinger([], false, _, 0) ->
    receive
	{ping, From} -> 
	    From ! {pong},
	    pinger([],false,0,0);
	die ->
	    ok			
    end;
pinger(ProcsArr, ReportTo, Limit, Pings) ->
    receive
	{ping, From} -> 
	    From ! {pong},
	    pinger(ProcsArr, ReportTo, Limit, Pings);
	{pong} ->
	    Len = array:size(ProcsArr),
	    TargetIndex = random(0, Len-1),
	    Target = array:get(TargetIndex, ProcsArr),
	    case Pings of
		Limit ->
		    ReportTo ! {done, self()},
		    pinger([], false, Limit, 0);
		_ ->
		    Target ! {ping, self()},
		    pinger(ProcsArr, ReportTo, Limit, Pings+1)
	    end
    end.

spawn_procs(N, _) when N =< 0 ->
    [];
spawn_procs(N, Limit) ->
    [spawn_link(fun () -> pinger([], true, Limit, 0) end) | spawn_procs(N-1, Limit)].

send_procs([], Msg) ->
    Msg;
send_procs([P|Ps], Msg) ->
    P ! Msg,
    send_procs(Ps, Msg).

receive_msgs(0) ->
    io:fwrite("Everything should be done now!\n"),
    ok;
receive_msgs(RemainingMessages) ->
    receive
	_ -> 
	    receive_msgs(RemainingMessages-1)
    end.

main([Arg]) ->
    N = list_to_integer(Arg),
    run(N),
    io:fwrite("~n"),
    halt(0).
