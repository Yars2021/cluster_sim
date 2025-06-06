-module(monitor).

-export([
    shell_autorun/ 1,
    create_cluster/3,
    cluster_to_graphviz/1,
    simulate/6,
    autorun/7
]).


shell_autorun(Args) ->
    [RawStrategies, RawClusterSize, RawEdges, RawBrokenNodes, RawTimeout, RawTTL] = Args,
    autorun(
        read_term(RawStrategies),
        list_to_integer(RawClusterSize),
        list_to_integer(RawEdges),
        list_to_integer(RawBrokenNodes),
        1,
        list_to_integer(RawTimeout),
        list_to_integer(RawTTL)
    ).


read_term(Arg) ->
    {ok, Tokens, _} = erl_scan:string(Arg ++ "."),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Value.


create_cluster(ClusterSize, Edges, BrokenNodes) ->
    Cluster = [{ID, spawn(fun() -> worker:init(no_monitor, ID) end)} || ID <- lists:seq(1, ClusterSize)],

    lists:foreach(fun({_, PidFrom}) ->
        lists:foreach(fun(NodeTo) ->
            PidFrom ! {connect, NodeTo}
        end, lists:filter(fun({_, Pid}) ->
            Pid /= PidFrom
        end, [lists:nth(rand:uniform(ClusterSize), Cluster) || _ <- lists:seq(1, Edges)]))
    end, Cluster),

    Broken = [lists:nth(rand:uniform(ClusterSize - 1) + 1, Cluster) || _ <- lists:seq(1, BrokenNodes)],

    lists:foreach(fun({_, Pid}) ->
        Pid ! {break}
    end, Broken),

    {ok, {Cluster, Broken}}.


cluster_to_graphviz(Info) ->
    {Cluster, Broken} = Info,

    file:write_file("./data/cluster.dot", io_lib:fwrite("digraph Cluster {~n" ++
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
    end, "", Cluster) ++ "~n\t1 [color=limegreen,style=bold];~n" ++
    lists:foldl(fun({ID, _}, Acc) ->
        Acc ++ "\t" ++ integer_to_list(ID) ++ " [color=red,style=bold];~n"
    end, "", Broken) ++
    "}~n", [])).


simulate(Starter, LogsFile, Strategy, Timeout, PacketTTL, Info) ->
    {Cluster, _} = Info,
    {_, FirstPid} = lists:nth(Starter, Cluster),

    Main = self(),
    TelemetryMonitor = spawn(fun() -> monitor_routine(Main, LogsFile, Cluster, [], Timeout) end),

    lists:foreach(fun({_, Pid}) ->
        Pid ! {set_monitor, TelemetryMonitor},
        Pid ! {strategy, Strategy}
    end, Cluster),

    FirstPid ! {ping, PacketTTL, [0]},

    receive {finish} -> io:fwrite("Simulation round finished for ~p.~n", [Strategy]) end,

    SentStats = lists:map(fun({ID, Pid}) ->
        Pid ! {get_sent, self()},

        receive
            {Sent} ->
                [ID, Sent];

            _ ->
                [ID, 0]
        end
    end, Cluster),

    file:write_file(LogsFile ++ ".sent", io_lib:fwrite("~w~n", [SentStats])),

    lists:foreach(fun({_, Pid}) ->
        Pid ! {set_monitor, no_monitor},
        Pid ! {strategy, not_specified},
        Pid ! {reset_reported},
        Pid ! {reset_sent}
    end, Cluster).


monitor_routine(Main, LogsFile, Cluster, Logs, Timeout) ->
    receive
        {ping_recv, ID, RecvTTL, Route, Status} ->
            monitor_routine(Main, LogsFile, Cluster, [[ID, RecvTTL, atom_to_list(Status), lists:reverse(Route)] | Logs], Timeout);

        _ ->
            monitor_routine(Main, LogsFile, Cluster, Logs, Timeout)

    after 1000 * Timeout ->
        file:write_file(LogsFile, io_lib:fwrite("~p~n", [lists:reverse(Logs)])),
        Main ! {finish}
    end.


autorun(Strategies, ClusterSize, Edges, BrokenNodes, Starter, Timeout, TTL) ->
    {ok, Cluster} = create_cluster(ClusterSize, Edges, BrokenNodes),
    cluster_to_graphviz(Cluster),

    lists:foreach(fun(Strategy) ->
        simulate(Starter, "./data/" ++ strategy_to_str(Strategy) ++ ".info", Strategy, Timeout, TTL, Cluster)
    end, Strategies).


strategy_to_str({S}) ->
    atom_to_list(S);

strategy_to_str({S, Arg}) ->
    atom_to_list(S) ++ "_" ++ integer_to_list(Arg).