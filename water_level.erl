-module(water_level).
-export([water_level/1]).

% water_level([2, 3, 0, 1, 1, 2]) -> 4.
%   _
% _| |       _
%    |  _ _ |
%    |_|
%


water_level(L) ->
    ListWithoutOneEdge = remove_edge(L),
    Height = side_height(ListWithoutOneEdge),
    ListWithoutEdges = remove_edge(lists:reverse(ListWithoutOneEdge)),
    MinHeight = erlang:min(Height, side_height(ListWithoutEdges)),
    lists:foldr(fun(X, Sum) ->
                    S = MinHeight - X,
                    if
                        S > 0 ->
                            Sum + S;
                        true ->
                            Sum
                    end
                    end,
                    0, ListWithoutEdges).


remove_edge([H1, H2|T]) when H2 > H1 ->
    remove_edge([H2|T]);
remove_edge([H1, H2|T]) ->
    [H1, H2|T];
remove_edge([_]) ->
    [];
remove_edge([]) ->
    [].

side_height(L) ->
    try
        erlang:hd(L)
    catch
        error:badarg ->
            0
    end.
