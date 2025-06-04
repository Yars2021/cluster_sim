-module(monitor).

-export([
    create_cluster/3,
    cluster_to_graphviz/1,
    simulate/4
]).


create_cluster(ClusterSize, Edges, BrokenNodes) ->
    Cluster =
        [{ID, spawn(fun() -> worker:init(self(), ID) end)} || ID <- lists:seq(1, ClusterSize)],

    lists:foreach(fun({_, PidFrom}) ->
        lists:foreach(fun(NodeTo) ->
            PidFrom ! {connect, NodeTo}
        end, lists:filter(fun({_, Pid}) ->
            Pid /= PidFrom
        end, [lists:nth(rand:uniform(ClusterSize), Cluster) || _ <- lists:seq(1, Edges)]))
    end, Cluster),

    Broken = [lists:nth(rand:uniform(ClusterSize), Cluster) || _ <- lists:seq(1, BrokenNodes)],

    lists:foreach(fun({_, Pid}) ->
        Pid ! {break}
    end, Broken),

    {ok, {Cluster, Broken}}.


cluster_to_graphviz(Info) ->
    {Cluster, Broken} = Info,

    "digraph Cluster {~n" ++
    lists:foldl(fun({Src, Pid}, OutAcc) ->
        Pid ! {get_connections, self()},

        OutAcc ++ receive
            {Connections} ->
                lists:foldl(fun({Dst, _}, Acc) ->
                    Acc ++ "\t" ++ integer_to_list(Src) ++ " -> " ++ integer_to_list(Dst) ++ ";~n"
                end, "", Connections);

            _ ->
                ""
        end
    end, "", Cluster) ++ "~n" ++
    lists:foldl(fun({ID, _}, Acc) ->
        Acc ++ "\t" ++ integer_to_list(ID) ++ " [color=red];~n"
    end, "", Broken) ++
    "}~n".


simulate(Strategy, Timeout, PacketTTL, Info) ->
    {Cluster, _} = Info,

    lists:foreach(fun({_, Pid}) ->
        Pid ! {strategy, Strategy}
    end, Cluster),



    lists:foreach(fun({_, Pid}) ->
        Pid ! {strategy, not_specified}
    end, Cluster).