-module(sort).
-export([ quicksort/1
        , monte_carlo_sort/1
        , merge_sort/1
        , insertion_sort/1
        , bubble_sort/1
        , selection_sort/1
        ]).


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

merge_sort(L) ->
    merge_sort(lists:map(fun(X) -> [X] end, L), ceiling(erlang:length(L)/2), []).

merge_sort([], 0, []) ->
    [];
merge_sort([L], 0, []) ->
    L;
merge_sort([X1, X2|Xs], N, Acc) ->
    merge_sort(Xs, N, [merge_sort(X1, X2)|Acc]);
merge_sort([X|[]], N, Acc) ->
    merge_sort([X|Acc], N - 1, []);
merge_sort([], N, Acc) ->
    merge_sort(Acc, N - 1, []).

merge_sort([X|Xs], [Y|Ys]) when X >= Y ->
    merge_sort([Y, X|Xs], Ys);
merge_sort([X|Xs], [Y|Ys]) ->
    merge_sort([X | merge_sort(Xs, [Y])], Ys);
merge_sort(Xs, [Y|[]]) ->
    Xs ++ [Y];
merge_sort(Xs, []) ->
    Xs.

ceiling(N) ->
    X = erlang:trunc(N),
    case N - X of
        0 ->
            X;
        _ ->
            X + 1
    end.

insertion_sort(L) ->
    insertion_sort(L, []).

insertion_sort([X|Xs], Sorted) ->
    insertion_sort(Xs, insert(X, Sorted));
insertion_sort([], Sorted) ->
    Sorted.

insert(E, []) ->
    [E];
insert(E, [X|Xs]) when E =< X ->
    [E, X|Xs];
insert(E, [X|Xs]) ->
    [X | insert(E, Xs)].

bubble_sort(Xs) ->
    Length = erlang:length(Xs),
    bubble_sort(array:from_list(Xs), 0, Length, Length).

bubble_sort(Xs, 0, _Index, 0) ->
    array:to_list(Xs);
bubble_sort(Xs, L, N, L) ->
    bubble_sort(Xs, 0, N - 1, L - 1);
bubble_sort(Xs, Index, N, L) ->
    bubble_sort(bubble_sort(Xs, Index), Index + 1, N, L).

bubble_sort(Xs, N) ->
    X1 = array:get(N, Xs),
    X2 = array:get(N + 1, Xs),
    if
        X1 > X2 ->
            array:set(N, X2, array:set(N + 1, X1, Xs));
        true ->
            Xs
    end.

selection_sort([X|Xs]) ->
    selection_sort(Xs, [], X, []);
selection_sort(L) when erlang:is_list(L) ->
    L.

selection_sort([X|Xs], Sorted, Min, NotMinAcc) when Min < X ->
    selection_sort(Xs, Sorted, Min, [X|NotMinAcc]);
selection_sort([X|Xs], Sorted, Min, NotMinAcc) ->
    selection_sort(Xs, Sorted, X, [Min|NotMinAcc]);
selection_sort([], Sorted, Min, []) ->
    lists:reverse([Min|Sorted]);
selection_sort([], Sorted, Min, [X|NotMinAcc]) ->
    selection_sort(NotMinAcc, [Min|Sorted], X, []).
