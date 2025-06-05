-module(worker).

-export([init/2]).


init(Monitor, ID) ->
    main_routine(Monitor, ID, ok, not_specified, [], false, false).


main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported) ->
    receive
        {reset_reported} ->
            main_routine(Monitor, ID, Status, Strategy, Connections, false, false);

        {set_monitor, NewMonitor} ->
            main_routine(NewMonitor, ID, Status, Strategy, Connections, Reported, TermReported);

        {get_connections, Requester} ->
            Requester ! {Connections},
            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported);

        {break} ->
            main_routine(Monitor, ID, broken, Strategy, Connections, Reported, TermReported);

        {fix} ->
            main_routine(Monitor, ID, ok, Strategy, Connections, Reported, TermReported);

        {strategy, NewStrategy} ->
            main_routine(Monitor, ID, Status, NewStrategy, Connections, Reported, TermReported);

        {connect, Node} ->
            main_routine(Monitor, ID, Status, Strategy, lists:sort([Node | Connections]), Reported, TermReported);

        {ping, 0, Route} ->
            case {Monitor, TermReported} of
                {no_monitor, _} ->
                    io:fwrite("");

                {_, false} ->
                    Monitor ! {ping_recv, ID, 0, [ID | Route], terminated};

                _ ->
                    io:fwrite("")
            end,

            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, true);

        {ping, TTL, Route} ->
            TransmitStatus = case Status of
                ok ->
                    case Strategy of
                        {single_cast} ->
                            [{_, NextPid} | _] =  Connections,
                            NextPid ! {ping, TTL - 1, [ID | Route]},
                            ok;

                        {multicast, GroupSize} ->
                            ping_next(ID, Route, 1, GroupSize, TTL - 1, Connections),
                            ok;

                        {broadcast} ->
                            ping_next(ID, Route, 1, length(Connections), TTL - 1, Connections),
                            ok;

                        {gossip} ->
                            ping_next(ID, Route, rand:uniform(length(Connections)), 1, TTL - 1, Connections),
                            ok;

                        _ -> unknown_strategy
                    end;

                _ -> ping_fail
            end,

            case {Monitor, Reported} of
                {no_monitor, _} ->
                    io:fwrite("");

                {_, false} ->
                    Monitor ! {ping_recv, ID, TTL, [ID | Route], TransmitStatus};

                _ ->
                    io:fwrite("")
            end,

            main_routine(Monitor, ID, Status, Strategy, Connections, true, TermReported);

        _ ->
            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported)
    end.


ping_next(ID, Route, StartIndex, Size, TTL, Connections) ->
    lists:foreach(fun({_, Pid}) ->
        Pid ! {ping, TTL, [ID | Route]}
    end, lists:sublist(Connections, StartIndex, Size)).
