-module(worker).

-export([init/2]).


init(Monitor, ID) ->
    main_routine(Monitor, ID, ok, not_specified, [], false, false, 0).


main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported, Sent) ->
    receive
        {get_sent, Requester} ->
            Requester ! {Sent},
            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported, Sent);

        {reset_sent} ->
            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported, 0);

        {reset_reported} ->
            main_routine(Monitor, ID, Status, Strategy, Connections, false, false, Sent);

        {set_monitor, NewMonitor} ->
            main_routine(NewMonitor, ID, Status, Strategy, Connections, Reported, TermReported, Sent);

        {get_connections, Requester} ->
            Requester ! {Connections},
            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported, Sent);

        {break} ->
            main_routine(Monitor, ID, broken, Strategy, Connections, Reported, TermReported, Sent);

        {fix} ->
            main_routine(Monitor, ID, ok, Strategy, Connections, Reported, TermReported, Sent);

        {strategy, NewStrategy} ->
            main_routine(Monitor, ID, Status, NewStrategy, Connections, Reported, TermReported, Sent);

        {connect, Node} ->
            main_routine(Monitor, ID, Status, Strategy, lists:sort([Node | Connections]), Reported, TermReported, Sent);

        {ping, 0, Route} ->
            case {Monitor, TermReported} of
                {no_monitor, _} ->
                    io:fwrite("");

                {_, false} ->
                    Monitor ! {ping_recv, ID, 0, [ID | Route], terminated};

                _ ->
                    io:fwrite("")
            end,

            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, true, Sent);

        {ping, TTL, Route} ->
            TransmitStatus = case Status of
                ok ->
                    case Strategy of
                        {single_cast} ->
                            [{_, NextPid} | _] =  Connections,
                            NextPid ! {ping, TTL - 1, [ID | Route]},
                            NewPackets = 1,
                            ok;

                        {multicast, GroupSize} ->
                            NewPackets = ping_next(ID, Route, 1, GroupSize, TTL - 1, Connections),
                            ok;

                        {broadcast} ->
                            NewPackets = ping_next(ID, Route, 1, length(Connections), TTL - 1, Connections),
                            ok;

                        {gossip} ->
                            NewPackets = ping_next(ID, Route, rand:uniform(length(Connections)), 1, TTL - 1, Connections),
                            ok;

                        _ ->
                            NewPackets = 0,
                            unknown_strategy
                    end;

                _ ->
                    NewPackets = 0,
                    ping_fail
            end,

            case {Monitor, Reported} of
                {no_monitor, _} ->
                    io:fwrite("");

                {_, false} ->
                    Monitor ! {ping_recv, ID, TTL, [ID | Route], TransmitStatus};

                _ ->
                    io:fwrite("")
            end,

            main_routine(Monitor, ID, Status, Strategy, Connections, true, TermReported, Sent + NewPackets);

        _ ->
            main_routine(Monitor, ID, Status, Strategy, Connections, Reported, TermReported, Sent)
    end.


ping_next(ID, Route, StartIndex, Size, TTL, Connections) ->
    Sublist = lists:sublist(Connections, StartIndex, Size),

    lists:foreach(fun({_, Pid}) ->
        Pid ! {ping, TTL, [ID | Route]}
    end, Sublist),

    length(Sublist).
