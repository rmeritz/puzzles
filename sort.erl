-module(sort).
-export([quicksort/1, monte_carlo_sort/1]).


quicksort([]) ->
    [];
quicksort([X|[]]) ->
    [X];
quicksort([H|T]) ->
    quicksort([X || X <- T,  X =< H]) ++ [H] ++ quicksort([X || X <- T, X > H]).


monte_carlo_sort([]) ->
    [];
monte_carlo_sort(L =[H|T]) ->
    case is_sorted(L) of
        true ->
            L;
        false ->
            monte_carlo_sort(randomly_insert(H, T))
    end.

randomly_insert(H, T) ->
    randomly_insert(H, T, random:uniform(erlang:length(T)), []).

randomly_insert(H, T, 0, Acc) ->
    Acc ++ [H] ++ T;
randomly_insert(H, [T|Ts], N, Acc) ->
    randomly_insert(H, Ts, N -1, [T|Acc]).

is_sorted([]) ->
    true;
is_sorted([_|[]]) ->
    true;
is_sorted([A, B|Rest]) when A =< B ->
    is_sorted([B|Rest]);
is_sorted(_) ->
    false.
