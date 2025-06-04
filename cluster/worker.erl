-module(worker).

-export([init/2]).


init(Monitor, ID) ->
    main_routine(Monitor, ID, ok, not_specified, []).


main_routine(Monitor, ID, Status, Strategy, Connections) ->
    receive
        {get_connections, Requester} ->
            Requester ! {Connections},
            main_routine(Monitor, ID, Strategy, Strategy, Connections);

        {break} ->
            main_routine(Monitor, ID, broken, Strategy, Connections);

        {fix} ->
            main_routine(Monitor, ID, ok, Strategy, Connections);

        {strategy, NewStrategy} ->
            main_routine(Monitor, ID, Status, NewStrategy, Connections);

        {connect, Node} ->
            main_routine(Monitor, ID, Status, Strategy, lists:sort([Node | Connections]));

        {ping, 0} ->
            Monitor ! {ping_recv, ID, ttl_end},
            main_routine(Monitor, ID, Status, Strategy, Connections);

        {ping, TTL} ->
            Monitor ! {ping_recv, ID, case Status of
                ok ->
                    case Strategy of
                        {single_cast} ->
                            ping_next(ID + 1, 1, TTL - 1, Connections),
                            ok;

                        {multicast, GroupSize} ->
                            ping_next(ID + 1, GroupSize, TTL - 1, Connections),
                            ok;

                        {broadcast} ->
                            ping_next(1, length(Connections), TTL - 1, Connections),
                            ok;

                        {gossip} ->
                            ping_next(rand:uniform(length(Connections)), 1, TTL - 1, Connections),
                            ok;

                        _ -> unknown_strategy
                    end;

                _ -> ping_fail
            end},

            main_routine(Monitor, ID, Status, Strategy, Connections);

        _ ->
            main_routine(Monitor, ID, Status, Strategy, Connections)
    end.


ping_next(_, _, 0, _) ->
    0;

ping_next(StartIndex, Size, TTL, Connections) when Size > length(Connections) ->
    ping_next(StartIndex, length(Connections), TTL, Connections);

ping_next(StartIndex, Size, TTL, Connections) ->
    lists:foreach(fun({_, Pid}) ->
        Pid ! {ping, TTL}
    end, lists:sublist(Connections, StartIndex, Size)).
