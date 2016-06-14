-module(mis).
-export([execute/2, waitforpids/2, test/0]).

%%This code returns an arbitrary Maximal Independent set of a given graph

test() ->
    execute('data.txt', 'computer.txt').

%%This extracts data from files and sends it to spawner
execute(Graphfile, Machinefile) ->
    {ok, Graph} = file:consult(Graphfile),
    {ok, Machines} = file:consult(Machinefile),
    spawner(true, Graph, Machines, [], [], []).

spawner(Start, Graph, Machines, ListofNode, Inmis, Eliminated) ->
    case Start of
        true ->
            [NoofNodes | Graphn] = Graph,
            Self = self(),
            ListofNodes = for(1, NoofNodes, Machines, Self, fun(Mach_name, Curr_nodeno, Selfi) -> spawn(Mach_name, ?MODULE, waitforpids, [Curr_nodeno, Selfi])end),
            printall(ListofNodes),
            sendlist(1, length(Graphn), Graphn, ListofNodes),
            spawner(false, Graphn, Machines, ListofNodes, Inmis, Eliminated);
        false ->
            receive
                {selected, Nodenumber, Pid} -> 
                    case length(Eliminated) + length(Inmis) +1 == length(Graph) of 
                        false ->
                            spawner(false, Graph, Machines, ListofNode, [{Nodenumber, Pid} | Inmis], Eliminated);
                        true ->
                            %% All nodes have replied
                            io:format("List of all the nodes in the MIS~n"),
                            printall([{Nodenumber, Pid} | Inmis]),
                            killall(ListofNode),
                            io:format("All the nodes are killed in true of selected~n")
                    end;
                {eliminated, Nodenumber, Pid} -> 
                    io:format("Eliminated = ~p, Inmis = ~p, Graph= ~p Nodenumber=~p~n", [length(Eliminated)+1, length(Inmis), length(Graph), Nodenumber]),
                    case length(Eliminated) + length(Inmis) +1 == length(Graph) of 
                        false ->
                            spawner(false, Graph, Machines, ListofNode, Inmis, [{Nodenumber, Pid} | Eliminated]);
                        true ->
                            %% All nodes have replied
                            io:format("List of all the nodes in the MIS~n"),
                            printall(Inmis),
                            killall(ListofNode),
                            io:format("All the nodes are killed in true of eliminated~n")
                    end
            end
    end.

%%Kills all the pids given in the list
killall([]) -> [];
killall([{_, Pid} |T]) ->
    exit(Pid, kill),
    killall(T).

%%Prints all the nodes in the list
printall([]) -> io:format("~n");
printall([H|T]) ->
    io:format("~p ", [H]),
    printall(T).
%%works like a contractor to send its all the nodes their list of neighbours
sendlist(_,_,[],_) -> 
    %%Send message to all the spawned nodes to start computation
    %%lists:foreach(fun({_, Pid}) -> Pid ! {start} end, ListofNodes),
    io:format("All are sent :\)~n");
sendlist(I, Max, Graph, ListofNodes) ->
    [H|T] = Graph,
    sendeach(I, H, ListofNodes, []),
    sendlist(I+1, Max, T, ListofNodes).

%%sends the list of neighbours to the individual nodes
sendeach(I, [], ListofNodes, Ans) -> 
    %io:format("Send at last~n"),
    Sendto = proplists:get_value(I, ListofNodes),
    Sendto ! {neighbours, Ans};
sendeach(I, [H|T], ListofNodes, Ans) ->
    {Nodenumber, Pid} = lists:nth(H, ListofNodes),
    sendeach(I, T, ListofNodes, [{Nodenumber, Pid}|Ans]).

waitforpids(Nodenumber, Adam) ->
    receive
        {neighbours, Neighbours} -> 
            Random = rand:uniform(),
            io:format("Node ~p has Random ~p~n", [Nodenumber, Random]),
            %Random = Nodenumber,
            mis(true, Adam, Nodenumber, Neighbours, false, false, Random, [], [])
    end.

%%Minimum Random number is in the MIS
mis(Start, Adam, Nodenumber, Neighbours, Inmis, Eliminated, Random, Fromothers, Notselected) ->
    case Start of
        true ->
            case length(Neighbours) == 0 of
                true ->
                    io:format("Node ~p is in the Mis~n", [Nodenumber]),
                    Adam ! {inmis, Nodenumber, self()},
                    mis(false, Adam, Nodenumber, Neighbours, true, false, Random, Fromothers, Notselected);

                false ->
                    io:format("Node ~p is sending a its random number to all its neighbours~n", [Nodenumber]),
                    broadcast({querry, {Random, Nodenumber, self()}}, Neighbours),
                    mis(false, Adam, Nodenumber, Neighbours, Inmis, Eliminated, Random, Fromothers, Notselected)
            end;
        false -> 
            receive
                {querry, {RandJ, _J, PidJ}} ->
                    case Random < RandJ of
                        true -> 
                            case length(Fromothers) + 1 == length(Neighbours) of
                                true ->
                                    broadcast({selected, Nodenumber, self()}, Neighbours),
                                    Adam ! {selected, Nodenumber, self()},
                                    %%mis(false, Adam, Nodenumber, Neighbours, true, false, Random, [{RandJ, PidJ}|Fromothers], Notselected);
                                    exit(self(), kill);
                                false ->
                                    mis(false, Adam, Nodenumber, Neighbours, Inmis, Eliminated, Random, [{RandJ, PidJ}|Fromothers], Notselected)
                            end;
                        false ->
                           broadcast({notselected, Nodenumber, self()}, Neighbours),
                           mis(false, Adam, Nodenumber, Neighbours, false, Eliminated, Random, [{RandJ, PidJ}|Fromothers], Notselected)
                    end;
                {notselected, J, Pid} ->
                    %%io:format("Got notselected from ~p to ~p~n", [J, Nodenumber]),
                    broadcast({noteliminated, Nodenumber, self()}, Neighbours),
                    mis(false, Adam, Nodenumber, Neighbours, Inmis, Eliminated, Random, Fromothers, [{J, Pid}|Notselected]);

                {selected, _J, PidJ} ->
                    broadcast({eliminated, Nodenumber, self()}, Notselected),
                    Adam ! {eliminated, Nodenumber, PidJ},
                    %%mis(false, Adam, Nodenumber, Neighbours, false, true, Random, Fromothers, Notselected);
                    exit(self(), kill);

                {noteliminated, _J, _PidJ} ->
                    mis(false, Adam, Nodenumber, Neighbours, Inmis, Eliminated, Random, Fromothers, Notselected);

                {eliminated, J, _PidJ} ->
                   mis(false, Adam, Nodenumber, lists:keydelete(J, 1, Neighbours), Inmis, Eliminated, Random, Fromothers, Notselected) 
            end
    end.

broadcast(_, []) ->
    true;
broadcast(Msg, [{_, Pid} | T])->
    Pid ! Msg,
    broadcast(Msg, T).


for(Max, Max, Machines, Adam, F) -> [{Max, F(lists:nth(random:uniform(length(Machines)), Machines), Max, Adam)}];
for(I, Max, Machines, Adam, F) -> [{I, F(lists:nth(random:uniform(length(Machines)), Machines), I, Adam)}|for(I+1, Max, Machines, Adam, F)].
