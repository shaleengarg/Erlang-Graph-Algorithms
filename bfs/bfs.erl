-module(bfs).
-export([execute/2, waitforpid/1]).

%Input a file with data of the form
%list with number of nodes, then a list of neighbours according to the number  
%[2, [2], [1] where the first number is the number of the nodes in the graph 
%The next list is the list of nodes connected to the first node and all else respectively

execute(Filename, Computernames) ->
    start(true, Filename, Computernames, 0, [], []).

start(Start, Filename, Computernames, Numberofnodes, ListofPids, Answer) ->
    case Start of 
        true ->
            {ok, List} = file:consult(Filename),
            {ok, Machines} = file:consult(Computernames),
            [Number_of_nodes | Edges] = List,
            Listofpids = for(1, Number_of_nodes, Machines, fun(A)-> spawn(A, ?MODULE, waitforpid, [self()])end),
            sendpid(1, Number_of_nodes, Listofpids, Edges),
            start(false, Filename, Computernames, Number_of_nodes, Listofpids, Answer);
        false ->
            receive
                {in_the_tree, Parent, Child} -> 
                    case length(Answer) + 1 == Numberofnodes of 
                        true -> 
                            killallnodes(ListofPids),
                            {[{Parent, Child} | Answer]};
                        false -> 
                            start(false, Filename, Computernames, Numberofnodes, ListofPids, [{Parent, Child} | Answer])
                    end
            end
    end.

%Kills all the nodes spawned earlier
killallnodes([]) -> [];
killallnodes([H|T]) ->
    exit(H, kill),
    killallnodes(T).

%% Sends all the nodes it associated Pids
sendpid(_, _, _, []) -> io:format("All pids are sent~n");
sendpid(I, Max, Listofpids, Edges) ->
    [Nodedges | List] = Edges,
    List_ofedgepids = getlist_pid(Listofpids, Nodedges, []),
    Node = lists:nth(I, Listofpids),
    Node ! {I, List_ofedgepids},
    sendpid(I+1, Max, Listofpids, List).

%% Generates a list of pids attached to the particular node from the Nodedges
getlist_pid(_Listofpids, [], List) -> List;
getlist_pid(Listofpids, Nodedges, List) -> 
    [H|T] = Nodedges,
    getlist_pid(Listofpids, T, [{H, lists:nth(H, Listofpids)} | List]).

%wait for the actual bfs, start after getting all the neighbours
%Adam is the pid of the node which evaluates start
waitforpid(Adam) ->
    receive
        {I, List_ofedgepids} -> bfs(Adam, I, List_ofedgepids, false, [])
    end.

%bfs(spawner process, Node number, List of edges, Intree, Neighboursintree list)
bfs(Adam, 1, Listofedges, Intree, Neighboursintree) ->
    case Intree of
        false ->
            io:format("I am root, Node 1~nSending message to all the neighbours~n"),
            lists:foreach(fun({_Node, Pid}) -> Pid ! {includeintree, 1, self()}end, Listofedges),
            Adam ! {in_the_tree, 0, 1},
            bfs(Adam, 1, Listofedges, true, Neighboursintree);
        true ->
            receive
                {includeintree, _Nodename, Pid} ->
                    Pid ! {already_in_tree, 1, self()};
                {already_in_tree, _Nodename, _Pid} ->
                    io:format("There is a mistake~n")
            end,
            bfs(Adam, 1, Listofedges, Intree, Neighboursintree)
    end;

bfs(Adam, Myname, Listofedges, Intree, Neighboursintree) ->
    case Intree of 
        false ->
            receive
                {includeintree, Nodename, Pid} ->
                    io:format("Node ~p is now in the tree with ~p as its parent Pid = ~p~n", [Myname, Nodename, Pid]),
                    Adam ! {in_the_tree, Nodename, Myname},
                    lists:foreach(fun({_Node, Ppid}) -> Ppid ! {includeintree, Myname, self()}end, Listofedges),
                    bfs(Adam, Myname, Listofedges, true, Neighboursintree)
            end;

        true ->
            receive
                {includeintree, _Nodename, Pid} ->
                    Pid ! {already_in_tree, Myname, self()},
                    bfs(Adam, Myname, Listofedges, Intree, Neighboursintree);

                {already_in_tree, Name, Pid} -> 
                    case length(Neighboursintree) + 1 == length(Listofedges) of
                        true -> 
                            io:format("Leaf edge reached, ~p node number , ~p~n", [Myname, self()]);

                        false ->
                            bfs(Adam, Myname, Listofedges, Intree, [{Name, Pid} | Neighboursintree])
                    end
            end
    end.

for(Max, Max, Machine, F) -> [F(lists:nth(random:uniform(length(Machine)), Machine))];
for(I, Max, Machine, F) -> [F(lists:nth(random:uniform(length(Machine)), Machine)) | for(I+1, Max, Machine, F)].
