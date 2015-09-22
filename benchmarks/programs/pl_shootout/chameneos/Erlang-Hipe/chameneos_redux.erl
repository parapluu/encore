% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%%% contributed by Christian von Roques
%%% modified by Jiri Isa
%%% (more) modified by Mikael Östlund to support chameneos amount as parameter

%% Each chameneos is its own process.
%% A chameneos sends {self(), Color} to the broker to request a
%% meeting with another chameneos.
%% The broker replies with {Pid, Color} of the partner met or 'stop'
%% whereupon the chameneos prints the Meetings and Selfmeetings it had
%% and replies with the number of Meetings for the broker to sum.

-module(chameneos_redux).
-export([main/1]).

-import(lists, [foreach/2]).

spell(0) -> " zero";
spell(N) -> spell(N, []).

spell(0, L) -> L;
spell(N, L) -> spell(N div 10, [element(N rem 10 + 1, {" zero", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine"}) | L]).


complement(C, C) -> C;
complement(blue, red) -> yellow;
complement(blue, yellow) -> red;
complement(red, blue) -> yellow;
complement(red, yellow) -> blue;
complement(yellow, blue) -> red;
complement(yellow, red) -> blue.


show_complements() ->
    [ io:fwrite("~p + ~p -> ~p~n", [A, B, complement(A, B)]) ||
        A <- [blue, red, yellow],
        B <- [blue, red, yellow]].


print_header(L) ->
    io:fwrite("~n"),
    foreach(fun(C) -> io:fwrite(" ~p", [C]) end, L),
    io:fwrite("~n").


run(L, N, Silent) ->
    case Silent of
        false -> print_header(L);
        _ -> io:fwrite("~n")
    end,
    Broker = self(),
    foreach(fun(Color) -> spawn(fun() -> chameneos(Broker, Color, 0, 0, Silent) end) end, L),
    broker(N),
    cleanup(length(L), 0).


chameneos(Broker, Color, Meetings, MetSelf, Silent) ->
    Broker ! { self(), Color },
    receive
        {OPid, OColor} ->
            chameneos(Broker, complement(Color, OColor), Meetings+1,
                      if OPid == self() -> MetSelf+1; true -> MetSelf end, Silent);
        stop ->
            case Silent of
                false -> io:fwrite("~w~s\n", [Meetings, spell(MetSelf)]);
                _ -> noop
            end,
            Broker ! Meetings
    end.


broker(0) -> nil;
broker(N) ->
    receive
        C1 = {Pid1, _} -> nil
    end,
    receive
        C2 = {Pid2, _} ->
            Pid1 ! C2,
            Pid2 ! C1,
            broker(N-1)
    end.

cleanup(0, M) -> io:fwrite("~s~n", [spell(M)]);
cleanup(N, M) ->
    receive
        {Pid, _Color} ->
            Pid ! stop,
            cleanup(N, M);
        Meetings ->
            cleanup(N-1, M+Meetings)
    end.

intToColour(Number) ->
    case Number rem 3 of
        0 -> blue;
        1 -> red;
        2 -> yellow
    end.

chameneosGeneratorAux(0, ChameneosList) -> ChameneosList;
chameneosGeneratorAux(Number, ChameneosList) -> 
    chameneosGeneratorAux(Number-1, [intToColour(Number)|ChameneosList]).
chameneosGenerator(ChameneosAmount) ->
    chameneosGeneratorAux(ChameneosAmount, []).

main([Meetings]) ->
    N = list_to_integer(Meetings),
    show_complements(),
    run([blue, red, yellow], N, false),
    run([blue, red, yellow, red, yellow, blue, red, yellow, red, blue], N, false),
    io:fwrite("~n"),
    halt(0);
main([Meetings,Chameneos|_]) ->
    N = list_to_integer(Meetings),
    C = list_to_integer(Chameneos),
    show_complements(),
    run(chameneosGenerator(C), N, true),
    io:fwrite("~n"),
    halt(0).

