-module(water_level).
-export([water_level/1]).

% water_level([2, 3, 0, 1, 1, 2]) -> 4.
%   _
% _| |       _
%    |  _ _ |
%    |_|
%


water_level(L) ->
    lists:sum(lists:map(fun area_of_pool/1, find_pools(L))).

find_pools([]) -> [];
find_pools(L) ->
    {Pool, Rest} = find_a_pool(L),
    [Pool|find_pools(Rest)].

find_a_pool([A, B |Xs]) when B > A ->
    {[], [B|Xs]};
find_a_pool([A, B |Xs]) ->
    {RestOfPool, RestOfPossiblePools} = up_to_end_of_pool(A, B, Xs),
    case RestOfPool of
        [] ->
            {[], [B|Xs]};
        _ ->
            {[A, B| RestOfPool], RestOfPossiblePools}
    end;
find_a_pool([_A|[]])->
    {[], []}.

up_to_end_of_pool(_A, _B, []) ->
    [];
up_to_end_of_pool(_A, B, [X|[]]) when  X > B ->
    {[X], []};
up_to_end_of_pool(_A, _B, [X|[]]) ->
    {[], [X]};
up_to_end_of_pool(A, _B, [X|Xs]) when X >= A ->
    {[X], Xs};
up_to_end_of_pool(A, B, [X|Xs]) ->
    {RestOfPool, RestOfPossiblePools} = up_to_end_of_pool(A, B, Xs),
    {[X|RestOfPool], RestOfPossiblePools}.


area_of_pool(Pools = [P1, _P2 |_Ps]) ->
    MinHeight = erlang:min(P1, lists:last(Pools)),
    lists:foldr(fun(X, Sum) ->
                    S = MinHeight - X,
                    if
                        S > 0 ->
                            Sum + S;
                        true ->
                            Sum
                    end
                    end,
                    0, Pools);
area_of_pool([]) -> 0;
area_of_pool([_]) -> 0.
