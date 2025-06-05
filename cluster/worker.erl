-module(worker).

-export([init/2]).


init(Monitor, ID) ->
    main_routine(Monitor, ID, ok, not_specified, []).


main_routine(Monitor, ID, Status, Strategy, Connections) ->
    receive
        {set_monitor, NewMonitor} ->
            main_routine(NewMonitor, ID, Status, Strategy, Connections);

        {get_connections, Requester} ->
            Requester ! {Connections},
            main_routine(Monitor, ID, Status, Strategy, Connections);

        {break} ->
            main_routine(Monitor, ID, broken, Strategy, Connections);

        {fix} ->
            main_routine(Monitor, ID, ok, Strategy, Connections);

        {strategy, NewStrategy} ->
            main_routine(Monitor, ID, Status, NewStrategy, Connections);

        {connect, Node} ->
            main_routine(Monitor, ID, Status, Strategy, lists:sort([Node | Connections]));

        {ping, From, 0} ->
            Monitor ! {ping_recv, From, ID, 0, ttl_end},
            main_routine(Monitor, ID, Status, Strategy, Connections);

        {ping, From, TTL} ->
            Monitor ! {ping_recv, From, ID, TTL, case Status of
                ok ->
                    case Strategy of
                        {single_cast} ->
                            [{_, NextPid} | _] =  Connections,
                            NextPid ! {ping, ID, TTL - 1},
                            ok;

                        {multicast, GroupSize} ->
                            ping_next(ID, 1, GroupSize, TTL - 1, Connections),
                            ok;

                        {broadcast} ->
                            ping_next(ID, 1, length(Connections), TTL - 1, Connections),
                            ok;

                        {gossip} ->
                            ping_next(ID, rand:uniform(length(Connections)), 1, TTL - 1, Connections),
                            ok;

                        _ -> unknown_strategy
                    end;

                _ -> ping_fail
            end},

            main_routine(Monitor, ID, Status, Strategy, Connections);

        _ ->
            main_routine(Monitor, ID, Status, Strategy, Connections)
    end.


ping_next(_, _, _, 0, _) ->
    0;

ping_next(ID, StartIndex, Size, TTL, Connections) when Size > length(Connections) ->
    ping_next(ID, StartIndex, length(Connections), TTL, Connections);

ping_next(ID, StartIndex, Size, TTL, Connections) ->
    lists:foreach(fun({_, Pid}) ->
        Pid ! {ping, ID, TTL}
    end, lists:sublist(Connections, StartIndex, Size)).
