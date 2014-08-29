-module(sqrt).
-export([sqrt/1, sqrt_binary/1]).

% http://www.ardendertat.com/2012/01/26/programming-interview-questions-27-squareroot-of-a-number/

% Find the squareroot of a given number rounded down to the nearest integer, without using the sqrt function. For example, squareroot of a number between [9, 15] should return 3, and [16, 24] should be 4.


sqrt(N) when (N >= 0) and (erlang:is_integer(N) or erlang:is_float(N)) ->
    TopGuess = erlang:trunc(N/2),
    sqrt(N, 0, 0,  TopGuess, sq(TopGuess));
sqrt(_N) ->
    erlang:throw(badarg).

% 15, 0, 0, 7, 49
% 15, 1, 1, 6, 36
% 15, 2, 4, 5, 25
% 15, 3, 9, 4, 24
%
%
% 9, 0, 0, 4, 16
% 9, 0, 1, 3, 9

sqrt(N, BottomGuess, BottomValue, _TopGuess, _TopValue) when N =:= BottomValue ->
    BottomGuess;
sqrt(N, _BottomGuess, _BottomValue, TopGuess, TopValue) when N =:= TopValue ->
    TopGuess;
sqrt(N, BottomGuess, BottomValue, _TopGuess, _TopValue) when BottomValue > N ->
    BottomGuess - 1;
sqrt(N, _BottomGuess, _BottomGuess, TopGuess, TopValue) when TopValue < N ->
    TopGuess;
sqrt(N, BottomGuess, _BottomValue, TopGuess, _TopValue) ->
    %io:format("~p ~p ~p ~p ~p~n", [N, BottomGuess, _BottomValue, TopGuess, _TopValue]),
    sqrt(N, BottomGuess + 1, sq(BottomGuess + 1), TopGuess - 1, sq(TopGuess - 1)).

sq(N) ->
    N*N.


sqrt_binary(N) when (N >= 0) and (erlang:is_integer(N) or erlang:is_float(N)) ->
    Guess = erlang:trunc(N/4),
    sqrt_binary(N,  Guess, sq(Guess));
sqrt_binary(_N) ->
    erlang:throw(badarg).

sqrt_binary(N, Guess, GuessSquared) when GuessSquared =:= N ->
    Guess;
sqrt_binary(N, Guess, GuessSquared) when GuessSquared > N ->
    NewGuess = erlang:trunc(Guess/2),
    sqrt_binary(N, NewGuess, sq(NewGuess));
sqrt_binary(N, Guess, _) ->
    NewGuess = erlang:trunc(Guess*2),
    sqrt_binary(N, NewGuess, sq(NewGuess)).
