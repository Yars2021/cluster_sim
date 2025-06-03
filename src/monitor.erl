-module(monitor).

-export([init_cluster/2]).


init_cluster(ClusterSize, BrokenNodes) ->
    Cluster =
        [{ID, spawn(fun() -> worker:init(self(), ID) end)} || ID <- lists:seq(1, ClusterSize)],

    lists:foreach(fun({_, PidFrom}) ->
        lists:foreach(fun({_, PidTo}) ->
            PidFrom ! {connect, PidTo}
        end, lists:filter(fun({_, Pid}) ->
            Pid /= PidFrom
        end, Cluster))
    end, Cluster),

    lists:foreach(fun({_, Pid}) ->
        Pid ! {break}
    end, [lists:nth(rand:uniform(ClusterSize), Cluster) || _ <- lists:seq(1, BrokenNodes)]),

    set_strategy(Cluster, a).


set_strategy(, strat) -> todo.
